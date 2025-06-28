;;; org-ai-openai.el --- OpenAI API related functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Robert Krahn and contributers
;; Copyright (C) 2025 github.com/Anoncheg1

;; This file is NOT part of GNU Emacs.

;; org-ai.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; org-ai.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with org-ai.el.
;; If not, see <https://www.gnu.org/licenses/>.

;;; Changelog:
;; - comment redundent "org-ai-use-auth-source" variable
;; - check that it working
;; - add debug-switch
;; - TODO: make clear URLs without hardcoding and bunch of variables
;; - TODO: remove org-ai-block, org, org-element
;; - TODO: rename all functions to convention.
;; - TODO: rename file to -api.el
;; - DONE: add :stream nil
;; - TODO: allow to disable role system.
;; - DONE: BUG: :completion req-type don't output anything
;; - DONE: `org-ai-after-chat-insertion-hook' not called for :stream nil and :completion
;; - DONE: hook errors handling at user side.
;; - DONE: stop timer when "When no connection"
;; - TODO: shut network process when timer expire - kill buffer, remove callback.
;; - TODO: rename org-ai-stream-completion to "compose" something
;; - DONE: coding is broken for received text for not English languages
;;; Commentary: encode, timer, interface two steps

;; Get info block from #begin_ai and call url-retrieve. Asynchronous
;; but only one call per buffer.
;;
;; Interface function: "org-ai-stream-completion".
;;
;; (org-ai-stream-completion service|model messages|prompt context)
;; context - org-ai-special-block ?????
;; -> (org-ai-stream-request service messages|prompt callback)
;; -> org-ai--get-headers, org-ai--get-endpoint, org-ai--payload, url-retrieve
;; - org-ai--get-headers = org-ai-openai-api-token = "api-key" or "x-api-key" or "Authorization"
;; - org-ai--get-endpoint = hardcoded URL or org-ai-openai-chat-endpoint, org-ai-openai-completion-endpoint, org-ai-google-chat-endpoint
;; -> callback: (org-ai--insert-stream-response) or (org-ai--insert-single-response)
;;
;; Main variables:
;; URL = org-ai--get-endpoint()  or org-ai-openai-chat-endpoint, org-ai-openai-completion-endpoint, org-ai-google-chat-endpoint
;; Headers = org-ai--get-headers
;; Token = org-ai-openai-api-token
;;
;;; Code:

(require 'org)
(require 'org-element)
(require 'url)
(require 'url-http)
(require 'cl-lib)
(require 'gv)
(require 'json)

(require 'org-ai-block)


;;; - Constants
(defcustom org-ai-jump-to-end-of-block t
  "If non-nil, jump to the end of the block after inserting the completion."
  :type 'boolean
  :group 'org-ai)

(defcustom org-ai-auto-fill nil
  "If non-nil, will fill paragraphs when inserting completions."
  :type 'boolean
  :group 'org-ai)

(defcustom org-ai-openai-api-token ""
  "This is your OpenAI API token.
You need to specify if you do not store the token in
`auth-sources'. You can retrieve it at
https://platform.openai.com/account/api-keys."
  :type 'string
  :group 'org-ai)

;; (defcustom org-ai-use-auth-source t
;;   "If non-nil, use auth-source to retrieve the OpenAI API token.
;; The secret should be stored in the format
;;   machine api.openai.com login org-ai password <your token>
;; in the `auth-sources' file."
;;   :type 'boolean
;;   :group 'org-ai)

(defcustom org-ai-default-completion-model "text-davinci-003"
  "The default model to use for completion requests. See https://platform.openai.com/docs/models for other options."
  :type 'string
  :group 'org-ai)

(defcustom org-ai-default-chat-model "gpt-4o-mini"
  "The default model to use for chat-gpt requests. See https://platform.openai.com/docs/models for other options."
  :type 'string
  :group 'org-ai)

(defcustom org-ai-chat-models '("gpt-4o-mini"
                                "gpt-4"
                                "gpt-4-32k"
                                "gpt-4-turbo"
                                "gpt-4o"
                                "gpt-4o-mini"
                                "gpt-4o-realtime-preview"
                                "gpt-4o-search-preview"
                                "gpt-4o-mini-search-preview"
                                "gpt-4.1"
                                "gpt-4.1-nano"
                                "gpt-4.1-mini"
                                "gpt-3.5-turbo"
                                "o1"
                                "o1-pro"
                                "o1-preview"
                                "o1-mini"
                                "o3"
                                "o3-mini"
                                "o4-mini"
                                "chatgpt-4o-latest")
  "Alist of available chat models. See https://platform.openai.com/docs/models."
  :type '(alist :key-type string :value-type string)
  :group 'org-ai)

(defcustom org-ai-default-max-tokens nil
  "The default maximum number of tokens to generate. This is what costs money."
  :type 'string
  :group 'org-ai)

(defcustom org-ai-default-chat-system-prompt "You are a helpful assistant inside Emacs."
  "The system message helps set the behavior of the assistant:
https://platform.openai.com/docs/guides/chat/introduction. This
default prompt is send as the first message before any user (ME)
or assistant (AI) messages. Inside a +#begin_ai...#+end_ai block
you can override it with: '[SYS]: <your prompt>'."
  :type 'string
  :group 'org-ai)

(defcustom org-ai-default-inject-sys-prompt-for-all-messages nil
  "Wether to add the `org-ai-default-chat-system-prompt' before all user messages.

By default the system prompt is only added before the first
message.

You can set this to true for a single block using the
:sys-everywhere option on the #+begin_ai block.

This can be useful to enforce the behavior specified by this
messages."
  :type '(choice (const :tag "Before every message" all)
                 (const :tag "Before first" first)
                 (const :tag "Before last" last)
                 (const :tag "Don't add" nil)
                 )
  :group 'org-ai)

(make-obsolete-variable 'org-ai-default-inject-sys-prompt-for-all-messages
                        "With newer ChatGPT versions this is no longer necessary."
                        "2023-12-26")

(defcustom org-ai-service 'openai
  "Service to use."
  :type '(choice (const :tag "OpenAI" openai)
                 (const :tag "Azure-OpenAI" azure-openai)
                 (const :tag "perplexity.ai" perplexity.ai)
                 (const :tag "anthropic" anthropic)
                 (const :tag "DeepSeek" deepseek)
                 (const :tag "google" google)
                 (const :tag "Together" together))
  :group 'org-ai)

(defvar org-ai-openai-chat-endpoint "https://api.openai.com/v1/chat/completions")

(defvar org-ai-openai-completion-endpoint "https://api.openai.com/v1/completions")

(defvar org-ai-google-chat-endpoint "https://generativelanguage.googleapis.com/v1beta/openai/chat/completions")

;; Azure-Openai specific variables

(defcustom org-ai-azure-openai-api-base "https://your-instance.openai.azure.com"
  "Base API URL for Azure-OpenAI."
  :type 'string
  :group 'org-ai)

;; Additional Azure-Openai specific variables
(defcustom org-ai-azure-openai-deployment "azure-openai-deployment-name"
  "Deployment name for Azure-OpenAI API."
  :type 'string
  :group 'org-ai)

(defcustom org-ai-azure-openai-api-version "2023-07-01-preview"
  "API version for Azure-OpenAI."
  :type 'string
  :group 'org-ai)

(defcustom org-ai-anthropic-api-version "2023-06-01"
  "API version for api.anthropic.com."
  :type 'string
  :group 'org-ai)

(defvar org-ai--current-request-buffer-for-stream nil
  "Internal var that stores the current request buffer.
For stream responses.
May be shown for debugging.")

(defvar org-ai--current-request-buffer nil
  "Internal var that stores the current request buffer.
For chat completion responses.")

(defvar org-ai--current-request-callback nil
  "Internal var that stores the current request callback.")

(defvar org-ai--current-request-is-streamed nil
  "Whether we expect a streamed response or a single completion payload.")

(defvar org-ai--current-progress-reporter nil
  "progress-reporter for non-streamed responses to make them less boring.")

(defvar org-ai--current-progress-timer nil
  "Timer for updating the progress reporter for non-streamed responses to make them less boring.")

(defvar org-ai--current-progress-timer-remaining-ticks 0
  "The time when the timer started.")

(defcustom org-ai-progress-duration 15
  "The total duration in seconds for which the timer should run.
Delay after which it will be killed."
  :type 'integer
  :group 'org-ai)

(defvar org-ai-after-chat-insertion-hook nil
  "Hook that is called when a chat response is inserted.
Note this is called for every stream response so it will typically
only contain fragments.")

(defvar org-ai--current-insert-position-marker nil
  "Where to insert the result.")
(make-variable-buffer-local 'org-ai--current-insert-position-marker)

(defvar org-ai--current-chat-role nil
  "During chat response streaming, this holds the role of the \"current speaker\".")

(defvar org-ai--chat-got-first-response nil)
(make-variable-buffer-local 'org-ai--chat-got-first-response)

(defvar org-ai--currently-inside-code-markers nil)
(make-variable-buffer-local 'org-ai--currently-inside-code-markers)

(defvar org-ai--currently-reasoning nil)
(make-variable-buffer-local 'org-ai--currently-reasoning)

(defvar org-ai--url-buffer-last-position-marker nil
  "Local buffer var to store last read position.")
;; (make-variable-buffer-local 'org-ai--url-buffer-last-position-marker)
;; (makunbound 'org-ai--url-buffer-last-position-marker)

(cl-deftype org-ai--response-type ()
  '(member role text stop error))

(cl-defstruct org-ai--response
  (type (:type org-ai--response-type))
  payload)

;; (defvar org-ai--debug-data nil)
;; (defvar org-ai--debug-data-raw nil)

;; (with-current-buffer "*scratch*"
;;   (erase-buffer)
;;   (pop-to-buffer "*scratch*" t)
;;   (let ((n 16))
;;    (insert (car (nth n org-ai--debug-data-raw)))
;;    (goto-char (cadr (nth n org-ai--debug-data-raw)))
;;    (beginning-of-line)))

(defcustom org-ai-debug-buffer "*debug-org-ai*"
  "If non-nil, enable debuging to a debug buffer."
  :type 'boolean
  :group 'org-ai)

;;; - debugging
(defun org-ai--prettify-json-string (json-string)
  "Convert a compact JSON string to a prettified JSON string.
This function uses a temporary buffer to perform the prettification.
Returns the prettified JSON string."
  (condition-case err
      (let* ((parsed-json (json-read-from-string json-string))
             ;; 1. First, encode the JSON object. This will be compact with your json-encode.
             (compact-json (json-encode parsed-json)))
        (with-temp-buffer
           (insert compact-json)
           (json-pretty-print-buffer)
           (buffer-string)))
    (error
        (message "Error formatting JSON: %S" err)
        (message "Input JSON: %S" json-string))))

(defun org-ai--debug-get-caller()
  (let* ((backtrace-line-length 20)
         (print-level 3)
         (print-length 10)
         (bt
          ;; (with-output-to-string (backtrace))
          (backtrace-to-string (backtrace-get-frames 'backtrace))
          )
         (caller))
         ;; (print bt)
         (seq-find
          ; - predicate
          (lambda (line)
            (let* ( (mpos (string-match "(" line))
                   (sline (substring line 0 mpos))
                   (tline (string-trim-right (string-trim-left sline))))
                   (if (and (not (string-empty-p tline))
                            (not (member tline '("org-ai--debug-get-caller" "org-ai--debug" ) )))
                       (setq caller tline)
                     nil ; else
                     ))
            )
          ;; - lines
          (cdr (split-string bt "\n" t)))
         caller))

(defun org-ai--debug (&rest args)
  "If firt argument of args is a stringwith %s than behave like format.
Otherwise format every to string and concatenate."
  (when (and org-ai-debug-buffer args)
    (let* ((buf-exist (get-buffer org-ai-debug-buffer))
           (bu (or buf-exist (get-buffer-create org-ai-debug-buffer)))
           (bu-window (or (get-buffer-window bu) ; to prevent reopening
                          (display-buffer-use-some-window bu nil))))

      (with-current-buffer bu
        ;; - move point to  to bottom
        (if buf-exist ; was not created
            (goto-char (point-max)))
        ;; - scroll debug buffer down
        (if bu-window
            (with-selected-window (get-buffer-window bu)
              (with-no-warnings
                (end-of-buffer nil))
              ;; (recenter '(t))
              ))
        ;; ;; - output caller function
        ;; (let ((caller
        ;;        (org-ai--debug-get-caller)))
        ;;   (when caller
        ;;     (insert "Din ")
        ;;     (insert caller)
        ;;     (insert " :")))
        ;; - output args
        (if (and (equal (type-of (car args)) 'string)
                 (string-match "%s" (car args)))
            (progn
              (insert (apply 'format (car args) (cdr args)))
              (newline))

          ;; else
          (insert (apply #'concat (mapcar (lambda (arg)
                                            (if (equal (type-of arg) 'string)
                                                (format "%s\n" arg)
                                              (concat (prin1-to-string arg) "\n"))
                                            ) args)))
          ;; (newline))
          )))))

;; (org-ai--debug "test %s" 2)
;; (org-ai--debug "test" 2 3 "sd")

(defun org-ai--debug-urllib (source-buf)
  (when (and source-buf org-ai-debug-buffer)
    (save-excursion
      (let* ((buf-exist (get-buffer org-ai-debug-buffer))
             (bu (or buf-exist (get-buffer-create org-ai-debug-buffer))))
        (with-current-buffer bu
          (let ((stri (with-current-buffer source-buf
                        ;; (save-excursion
                          (buffer-substring-no-properties (or org-ai--url-buffer-last-position-marker
                                                              (point-min))
                                                          (point-max)))))
            (goto-char (point-max))
            (insert "response:\n")
            (insert stri)
            (newline))))
      )))

;;; - Get constant functions
(defun org-ai--check-model (model endpoint)
  "Check if the model name is somehow mistyped.
`MODEL' is the model name. `ENDPOINT' is the API endpoint."
  (unless model
    (error "No org-ai model specified."))

  (when (or (string-match-p "api.openai.com" endpoint)
            (string-match-p "openai.azure.com" endpoint))

    (let ((lowercased (downcase model)))
      (when (and (string-prefix-p "gpt-" model) (not (string-equal lowercased model)))
        (warn "Model name '%s' should be lowercase. Use '%s' instead." model lowercased)))

    (unless (member model org-ai-chat-models)
      (message "Model '%s' is not in the list of available models. Maybe this is because of a typo or maybe we haven't yet added it to the list. To disable this message add (add-to-list 'org-ai-chat-models \"%s\") to your init file." model model))))

(defun org-ai--read-service-name (name)
  "Map a service name such as 'openai' to a valid `org-ai-service' symbol."
  (intern-soft name))

(defun org-ai--service-of-model (model)
  "Return the service of the model.
`MODEL' is the model name."
  (cond
   ((string-prefix-p "gpt-" model) 'openai)
   ((string-prefix-p "chatgpt-" model) 'openai)
   ((string-prefix-p "o1" model) 'openai)
   ((string-prefix-p "o3" model) 'openai)
   ((string-prefix-p "o4" model) 'openai)
   ((string-prefix-p "claude" model) 'anthropic)
   ((string-prefix-p "gemini" model) 'google)
   ((string-prefix-p "deepseek" model) 'deepseek)
   (t nil)))

(defun org-ai--openai-get-token (&optional service)
  "Try to get the openai token.
Either from `org-ai-openai-api-token' or from auth-source."
  (or (and
       (stringp org-ai-openai-api-token)
       (not (string-empty-p org-ai-openai-api-token))
       org-ai-openai-api-token)
      (and
       ;; org-ai-use-auth-source
       (org-ai--openai-get-token-auth-source service))
      (error "Please set `org-ai-openai-api-token' to your OpenAI API token or setup auth-source (see org-ai readme)")))

(defun strip-api-url (url)
  "Strip the leading https:// and trailing / from an URL"
  (let ((stripped-url (if (string-prefix-p "https://" url)
                          (substring url 8)
                        url)))
    (if (string-suffix-p "/" stripped-url)
        (substring stripped-url 0 -1)
      stripped-url)))

(defun org-ai--openai-get-token-auth-source (&optional service)
  "Retrieves the authentication token for the OpenAI service using auth-source."
  (require 'auth-source)
  (let* ((service (or service org-ai-service))
         (endpoint (pcase service
                     ('openai "api.openai.com")
                     ('deepseek "api.deepseek.com")
                     ('perplexity.ai "api.perplexity.ai")
                     ('anthropic "api.anthropic.com")
                     ('google "generativelanguage.googleapis.com")
                     ('together "api.together.xyz")
                     ('azure-openai (strip-api-url org-ai-azure-openai-api-base)))))
    (or (auth-source-pick-first-password :host endpoint :user "org-ai")
        (auth-source-pick-first-password :host endpoint :login "org-ai"))))


(defun org-ai--get-endpoint (messages &optional service)
  "Determine the correct endpoint based on the service and
whether messages are provided."
  (let ((service (or service org-ai-service)))
    (cond
     ((eq service 'azure-openai)
      (format "%s/openai/deployments/%s%s/completions?api-version=%s"
	      org-ai-azure-openai-api-base org-ai-azure-openai-deployment
	      (if messages "/chat" "") org-ai-azure-openai-api-version))
     ((eq service 'perplexity.ai)
      "https://api.perplexity.ai/chat/completions")
     ((eq service 'deepseek)
      "https://api.deepseek.com/v1/chat/completions")
     ((eq service 'anthropic)
      "https://api.anthropic.com/v1/messages")
     ((eq service 'google)
      org-ai-google-chat-endpoint)
     ((eq service 'together)
      "https://api.together.xyz/v1/chat/completions")
     (t
      (if messages org-ai-openai-chat-endpoint org-ai-openai-completion-endpoint)))))

(defun org-ai--get-headers (&optional service)
  "Determine the correct headers based on the service."
  (let ((service (or service org-ai-service)))
    `(("Content-Type" . "application/json")
      ,@(cond
        ((eq service 'azure-openai)
         `(("api-key" . ,(org-ai--openai-get-token service))))
        ((eq service 'anthropic)
         `(("x-api-key" . ,(org-ai--openai-get-token service))
           ("anthropic-version" . ,org-ai-anthropic-api-version)))
        ((eq service 'google)
         `(("Accept-Encoding" . "identity")
           ("Authorization" . ,(encode-coding-string (string-join `("Bearer" ,(org-ai--openai-get-token service)) " ") 'utf-8))))
        (t
         `(("Authorization" . ,(encode-coding-string (string-join `("Bearer" ,(org-ai--openai-get-token service)) " ") 'utf-8))))))))

;;; - main
(cl-defun org-ai-stream-completion (&optional &key prompt messages context model max-tokens temperature top-p frequency-penalty presence-penalty service stream)
  "Compose API request.
Start a server-sent event stream.
Called from `org-ai-complete-block' in main file with query string in `PROMPT' or in
`MESSAGES'.
`CONTEXT' - where to put result.
`MODEL' is the model to use.
`MAX-TOKENS' is the maximum number of tokens to generate.
`TEMPERATURE' is the temperature of the distribution.
`TOP-P' is the top-p value.
`FREQUENCY-PENALTY' is the frequency penalty.
`PRESENCE-PENALTY' is the presence penalty.
`CONTEXT' is the context of the special block.
`SERVICE' string - is the AI cloud service such as 'openai or 'azure-openai'.
`STREAM' string - as bool, indicates whether to stream the response.
"
  ;; - Step 1) get Org properties or block parameters
  (let* ((context (or context (org-ai-special-block)))
         (buffer (current-buffer))
         (info (org-ai-get-block-info context)) ; ((:max-tokens . 150) (:service . "together") (:model . "xxx"))
         (callback (cond ; for org-ai-stream-request
                    (messages (lambda (result) (org-ai--insert-stream-response context buffer result t)))
                    ; prompt, req-type = completion
                    (t (lambda (result) (org-ai--insert-single-response context buffer result))))))

    (org-ai--debug info)
    ;; This macro helps in defining local variables by trying to get their values from:
    ;; 1. Existing local variable (if passed as argument to the main function)
    ;; 2. Alist `info` (from Org-AI block header, e.g., :model "gpt-4")
    ;; 3. Inherited Org property (e.g., #+PROPERTY: model gpt-4)
    ;; 4. Default form (if provided)
    (cl-macrolet ((let-with-captured-arg-or-header-or-inherited-property ; NAME
                    (definitions &rest body) ; ARGLIST
                    `(let ,(cl-loop for (sym . default-form) in definitions ; BODY
                                    collect `(,sym ; Try existing variable first
                                                (or ,sym ; try function parameter
                                                       (alist-get ,(intern (format ":%s" (symbol-name sym))) info) ; Then from Org-AI block info
                                                       (when-let ((prop (org-entry-get-with-inheritance ,(symbol-name sym)))) ; Then from inherited Org property
                                                         ;; --- Conversion Logic for Parameters ---
                                                         (if (eq (quote ,sym) 'model)
                                                             prop
                                                           (if (stringp prop) (string-to-number prop) prop)))
                                                       ,@default-form)))
                       ,@body)))
      ;; FORM
      (let-with-captured-arg-or-header-or-inherited-property
       ((model (if messages org-ai-default-chat-model org-ai-default-completion-model))
        (max-tokens org-ai-default-max-tokens) ; int
        (top-p)
        (temperature)
        (frequency-penalty)
        (presence-penalty)
        (service)
        (stream)
        )
       ;; - Step 2) get Org properties or block parameters
       (org-ai-stream-request :prompt prompt
                                :messages messages
                                :model model
                                :max-tokens max-tokens
                                :temperature temperature
                                :top-p top-p
                                :frequency-penalty frequency-penalty
                                :presence-penalty presence-penalty
                                :service service
                                :callback callback
                                :stream stream)))))
;; Together.xyz 2025
;; '(id "nz7KyaB-3NKUce-9539d1912ce8b148" object "chat.completion" created 1750575101 model "meta-llama/Llama-3.3-70B-Instruct-Turbo-Free" prompt []
;;   choices [(finish_reason "length" seed 3309196889559996400 logprobs nil index 0
;;             message (role "assistant" content " The answer is simple: live a long time. But how do you do that? Well, itâs not as simple as it sounds." tool_calls []))] usage (prompt_tokens 5 completion_tokens 150 total_tokens 155 cached_tokens 0))


(defun org-ai--insert-single-response (context buffer &optional response)
  "For Completion LLM mode.
Used as callback for `org-ai-stream-request'.
Insert the response from the OpenAI API into #+begin_ai block.
`CONTEXT' is the context of the special block. `BUFFER' is the
buffer to insert the response into. `RESPONSE' is the response
from the OpenAI API."
  (org-ai--debug "context:" context
                 "response:" response)
  (when response
    (if-let ((error (plist-get response 'error)))
        (if-let ((message (plist-get error 'message))) (error message) (error error))
      (if-let* ((choice (aref (plist-get response 'choices) 0))
                (text (or (plist-get choice 'text)
                          ;; Together.xyz way
                          (plist-get (plist-get choice 'message) 'content)))
                )
        (with-current-buffer buffer
          (org-ai--debug "text:" text)
          ;; set mark so we can easily select the generated text (e.g. to delet it to try again)
          (unless org-ai--current-insert-position-marker
            (push-mark (org-element-property :contents-end context)))
          (let ((pos (or (and org-ai--current-insert-position-marker
                              (marker-position org-ai--current-insert-position-marker))
                         (org-element-property :contents-end context)))
                (text-decoded (decode-coding-string text 'utf-8)))
            (save-excursion
              (goto-char pos)

              (when (string-suffix-p "#+end_ai" (buffer-substring-no-properties (point) (line-end-position)))
                (insert "\n")
                (backward-char))
              (insert text-decoded)

              (condition-case hook-error
                  (run-hook-with-args 'org-ai-after-chat-insertion-hook 'end text-decoded)
                (error
                 (message "Error during \"after-chat-insertion-hook\": %s" hook-error)))

              (setq org-ai--current-insert-position-marker (point-marker)))))))))


;; Here is an example for how a full sequence of OpenAI responses looks like:
;; '((id "chatcmpl-9hM1UJgWe4cWKcJKvoMBzzebOOzli" object "chat.completion.chunk" created 1720119788 model "gpt-4o-2024-05-13" system_fingerprint "fp_d576307f90" choices [(index 0 delta (role "assistant" content "") logprobs nil finish_reason nil)])
;;   (id "chatcmpl-9hM1UJgWe4cWKcJKvoMBzzebOOzli" object "chat.completion.chunk" created 1720119788 model "gpt-4o-2024-05-13" system_fingerprint "fp_d576307f90" choices [(index 0 delta (content "Hello") logprobs nil finish_reason nil)])
;;   (id "chatcmpl-9hM1UJgWe4cWKcJKvoMBzzebOOzli" object "chat.completion.chunk" created 1720119788 model "gpt-4o-2024-05-13" system_fingerprint "fp_d576307f90" choices [(index 0 delta (content ",") logprobs nil finish_reason nil)])
;;   (id "chatcmpl-9hM1UJgWe4cWKcJKvoMBzzebOOzli" object "chat.completion.chunk" created 1720119788 model "gpt-4o-2024-05-13" system_fingerprint "fp_d576307f90" choices [(index 0 delta (content " Robert") logprobs nil finish_reason nil)])
;;   (id "chatcmpl-9hM1UJgWe4cWKcJKvoMBzzebOOzli" object "chat.completion.chunk" created 1720119788 model "gpt-4o-2024-05-13" system_fingerprint "fp_d576307f90" choices [(index 0 delta nil logprobs nil finish_reason "stop")])
;;   nil)
;;
;; and Anthropic:
;; '((type "message_start" message (id "msg_01HoMq4LgkUpHpkXqXoZ7R1W" type "message" role "assistant" model "claude-3-5-sonnet-20240620" content [] stop_reason nil stop_sequence nil usage (input_tokens 278 output_tokens 2)))
;;   (type "content_block_start" index 0 content_block (type "text" text ""))
;;   (type "ping")
;;   (type "content_block_delta" index 0 delta (type "text_delta" text "Hello Robert"))
;;   (type "content_block_delta" index 0 delta (type "text_delta" text "."))
;;   (type "content_block_stop" index 0)
;;   (type "message_delta" delta (stop_reason "end_turn" stop_sequence nil) usage (output_tokens 22))
;;   (type "message_stop"))
;;
;; and Google
;; '((created 1745008491
;;    model "gemini-2.5-pro-preview-03-25"
;;    object "chat.completion.chunk"
;;    choices [(delta (content "Hello Robert! How can I help you today?"
;;                     role "assistant")
;;              finish_reason "stop"
;;              index 0)]))
(defun org-ai--normalize-response (response)
  "This function normalizes JSON data received from OpenAI-style, Anthropic, and Perplexity endpoints.
`RESPONSE' is one JSON message of the stream response."
  ;; (org-ai--debug "response:" response)

  (if-let ((error-message (plist-get response 'error)))
      (list (make-org-ai--response :type 'error :payload (or (plist-get error 'message) error-message)))

    (let ((response-type (plist-get response 'type)))

      ;; first try anthropic
      (cond
       ((string= response-type "ping") nil)
       ((string= response-type "message_start")
        (when-let ((role (plist-get (plist-get response 'message) 'role)))
          (list (make-org-ai--response :type 'role :payload role))))
       ((string= response-type "content_block_start")
        (when-let ((text (plist-get (plist-get response 'content_block) 'text)))
          (list (make-org-ai--response :type 'text :payload text))))
       ((string= response-type "content_block_delta")
        (when-let ((text (plist-get (plist-get response 'delta) 'text)))
          (list (make-org-ai--response :type 'text :payload text))))
       ((string= response-type "content_block_stop") nil)
       ((string= response-type "message_delta")
        (when-let ((stop-reason (plist-get (plist-get response 'delta) 'stop_reason)))
          (list (make-org-ai--response :type 'stop :payload stop-reason))))
       ((string= response-type "message_stop") nil)


       ;; try perplexity.ai
       ((and (plist-get response 'model) (string-prefix-p "llama-" (plist-get response 'model)))
        (let ((choices (plist-get response 'choices)))
          (when (and choices (> (length choices) 0))
            (let* ((choice (aref choices 0))
                   (message (plist-get choice 'message))
                   (delta (plist-get choice 'delta))
                   (role (or (plist-get delta 'role) (plist-get message 'role)))
                   (content (or (plist-get delta 'content) (plist-get message 'content)))
                   (finish-reason (plist-get choice 'finish_reason)))
              (append
               (when role
                 (list (make-org-ai--response :type 'role :payload role)))
               (when content
                 (list (make-org-ai--response :type 'text :payload content)))
               (when finish-reason
                 (list (make-org-ai--response :type 'stop :payload finish-reason))))))))

       ;; single message e.g. from non-streamed completion
       ((let ((choices (plist-get response 'choices)))
          (and (= 1 (length choices))
               (plist-get (aref choices 0) 'message)))
        (let* ((choices (plist-get response 'choices))
               (choice (aref choices 0))
               (text (plist-get (plist-get choice 'message) 'content))
               (role (plist-get (plist-get choice 'message) 'role))
               (finish-reason (or (plist-get choice 'finish_reason) 'stop)))
          (list (make-org-ai--response :type 'role :payload role)
                (make-org-ai--response :type 'text :payload text)
                (make-org-ai--response :type 'stop :payload finish-reason))))

       ;; try openai, deepseek, gemini streamed
       (t (let ((choices (plist-get response 'choices)))
            (cl-loop for choice across choices
                     append (let* ((delta (plist-get choice 'delta))
                                   (role (when-let ((role (plist-get delta 'role)))
                                           (if (and (string= "assistant" role)
                                                    (plist-get delta 'reasoning_content))
                                               "assistant_reason"
                                             role)))
                                   (content (plist-get (plist-get choice 'delta) 'content))
                                   (reasoning-content (plist-get delta 'reasoning_content))
                                   (finish-reason (plist-get choice 'finish_reason))
                                   (result nil))
                              (when finish-reason
                                (push (make-org-ai--response :type 'stop :payload finish-reason) result))
                              (when reasoning-content
                                (setq org-ai--currently-reasoning t)
                                (push (make-org-ai--response :type 'text :payload reasoning-content) result))
                              (when (and content (> (length content) 0))
                                (push (make-org-ai--response :type 'text :payload content) result)
                                (when org-ai--currently-reasoning
                                  (setq org-ai--currently-reasoning nil)
                                  (push (make-org-ai--response :type 'role :payload "assistant") result)))
                              (when role
                                (push (make-org-ai--response :type 'role :payload role) result))
                              result))))))))

(defun org-ai--insert-stream-response (context buffer &optional response insert-role)
  "`RESPONSE' is one JSON message of the stream response.
Used as callback for `org-ai-stream-request'.
When `RESPONSE' is nil, it means we are done. `CONTEXT' is the
context of the special block. `BUFFER' is the buffer to insert
the response into."
  (org-ai--debug "response:" response)
  (let ((normalized (org-ai--normalize-response response)))
   (cl-loop for response in normalized
            do (let ((type (org-ai--response-type response)))
                 (when (eq type 'error)
                   (error (org-ai--response-payload response)))

                 (with-current-buffer buffer
                   (let ((pos (or (and org-ai--current-insert-position-marker
                                       (marker-position org-ai--current-insert-position-marker))
                                  (and context (org-element-property :contents-end context))
                                  (point))))
                     (save-excursion
                       (goto-char pos)

                       ;; make sure we have enough space at end of block, don't write on same line
                       (when (string-suffix-p "#+end_ai" (buffer-substring-no-properties (point) (line-end-position)))
                         (insert "\n")
                         (backward-char)))

                     (cl-case type

                       (role (let ((role (org-ai--response-payload response)))
                               (when (not (string= role org-ai--current-chat-role))
                                 (save-excursion
                                   (goto-char pos)

                                   (setq org-ai--current-chat-role role)
                                   (let ((role (and insert-role (org-ai--response-payload response))))
                                     (cond
                                      ((string= role "assistant_reason")
                                       (insert "\n[AI_REASON]: "))
                                      ((string= role "assistant")
                                       (insert "\n[AI]: "))
                                      ((string= role "user")
                                       (insert "\n[ME]: "))
                                      ((string= role "system")
                                       (insert "\n[SYS]: ")))

                                     (condition-case hook-error
                                       (run-hook-with-args 'org-ai-after-chat-insertion-hook 'role role)
                                     (error
                                      (message "Error during \"after-chat-insertion-hook\": %s" hook-error)))

                                     (setq org-ai--current-insert-position-marker (point-marker)))))))

                       (text (let ((text (decode-coding-string (org-ai--response-payload response)
                                                                'utf-8)))
                               (save-excursion
                                 (goto-char pos)
                                 (when (or org-ai--chat-got-first-response (not (string= (string-trim text) "")))
                                   (when (and (not org-ai--chat-got-first-response) (string-prefix-p "```" text))
                                     ;; start markdown codeblock responses on their own line
                                     (insert "\n"))
                                   ;; track if we are inside code markers
                                   (setq org-ai--currently-inside-code-markers (and (not org-ai--currently-inside-code-markers)
                                                                                    (string-match-p "```" text)))
                                   (org-ai--debug response)
                                   (org-ai--debug text)
                                   (insert text)
                                   ;; "auto-fill"
                                   (when (and org-ai-auto-fill (not org-ai--currently-inside-code-markers))
                                     (fill-paragraph))

                                   (condition-case hook-error
                                       (run-hook-with-args 'org-ai-after-chat-insertion-hook 'text text)
                                     (error
                                      (message "Error during \"after-chat-insertion-hook\": %s" hook-error)))
                                   )
                                 (setq org-ai--chat-got-first-response t)
                                 (setq org-ai--current-insert-position-marker (point-marker)))))

                       (stop (progn
                               (save-excursion
                                 (when org-ai--current-insert-position-marker
                                   (goto-char org-ai--current-insert-position-marker))

                                 ;; (message "inserting user prompt: %" (string= org-ai--current-chat-role "user"))
                                 (let ((text (if insert-role
                                                 (let ((text "\n\n[ME]: "))
                                                   (insert text)
                                                   text)
                                               "")))

                                   (condition-case hook-error
                                       (run-hook-with-args 'org-ai-after-chat-insertion-hook 'end text)
                                     (error
                                      (message "Error during \"after-chat-insertion-hook\": %s" hook-error)))
                                   (setq org-ai--current-insert-position-marker (point-marker))))

                               (org-element-cache-reset)
                               (when org-ai-jump-to-end-of-block (goto-char org-ai--current-insert-position-marker)))))))))
   normalized))

(cl-defun org-ai-stream-request (&optional &key prompt messages model max-tokens temperature top-p frequency-penalty presence-penalty service stream callback stream)
  "Executed by `org-ai-stream-completion'
`PROMPT' is the query for completions `MESSAGES' is the query for
chatgpt. `CALLBACK' is the callback function. `MODEL' is the
model to use. `MAX-TOKENS' is the maximum number of tokens to
generate. `TEMPERATURE' is the temperature of the distribution.
`TOP-P' is the top-p value. `FREQUENCY-PENALTY' is the frequency
penalty. `PRESENCE-PENALTY' is the presence penalty."
  ;; - `org-ai--insert-stream-response'
  (setq org-ai--current-insert-position-marker nil)
  (setq org-ai--chat-got-first-response nil)
  ;; (setq org-ai--debug-data nil)
  ;; (setq org-ai--debug-data-raw nil)
  (setq org-ai--currently-inside-code-markers nil)
  ;; - local
  (setq service (or (if (stringp service) (org-ai--read-service-name service) service)
                    (org-ai--service-of-model model)
                    org-ai-service))
  (org-ai--debug stream (type-of stream))
  (setq stream (if (and stream (string-equal-ignore-case stream "nil"))
                   nil
                 ;; else
                 (org-ai--stream-supported service model)))

  ;; - HTTP body preparation as a string
  (let* ((url-request-extra-headers (org-ai--get-headers service))
         (url-request-method "POST")
         (endpoint (org-ai--get-endpoint messages service))
         (url-request-data (org-ai--payload :prompt prompt
					    :messages messages
					    :model model
					    :max-tokens max-tokens
					    :temperature temperature
					    :top-p top-p
					    :frequency-penalty frequency-penalty
					    :presence-penalty presence-penalty
					    :service service
					    :stream stream)))
    ;; - regex check
    (org-ai--check-model model endpoint) ; not empty and if "api.openai.com" or "openai.azure.com"

    (org-ai--debug "endpoint:" endpoint (type-of endpoint)
                   "request-data:" (org-ai--prettify-json-string url-request-data)
                   ;; "callback:" callback
                   )

    ;; - `org-ai--url-request-on-change-function', `org-ai-reset-stream-state', `org-ai--current-request-is-streamed'
    (setq org-ai--current-request-is-streamed stream)
    ;; - `org-ai--url-request-on-change-function' (call) , `org-ai-reset-stream-state' (just set nil)
    ;; - it is `org-ai--insert-stream-response' or `org-ai--insert-single-response'
    (setq org-ai--current-request-callback callback)
    ;; - run timer that show /-\ looping
    (when (not stream) (org-ai--progress-reporter-until-request-done))


    (org-ai--debug "Main request before, that return a \"urllib buffer\".")
    (setq org-ai--current-request-buffer-for-stream
          (url-retrieve
           endpoint
           (lambda (_events)
             (org-ai--debug "in url-retrieve callback!!!!!!!!!!." _events)

             (with-current-buffer org-ai--current-request-buffer-for-stream
               (org-ai--debug-urllib org-ai--current-request-buffer-for-stream)
               (org-ai--url-request-on-change-function nil nil nil))
             (org-ai--maybe-show-openai-request-error org-ai--current-request-buffer-for-stream)
             (org-ai-reset-stream-state))))
    (org-ai--debug "Main request after.")

    ;; for stream add hook, otherwise remove
    (if stream
        (unless (member 'org-ai--url-request-on-change-function after-change-functions)
          (with-current-buffer org-ai--current-request-buffer-for-stream
            (add-hook 'after-change-functions #'org-ai--url-request-on-change-function nil t)))
      ;; else - not stream
      (with-current-buffer org-ai--current-request-buffer-for-stream
        (remove-hook 'after-change-functions #'org-ai--url-request-on-change-function t)))

    ;; - return, not used
    org-ai--current-request-buffer-for-stream))


(defun org-ai--maybe-show-openai-request-error (request-buffer)
  "If the API request returned an error, show it.
`REQUEST-BUFFER' is the buffer containing the request."
  (with-current-buffer request-buffer
    (when (and (boundp 'url-http-end-of-headers) url-http-end-of-headers)
      (goto-char url-http-end-of-headers))
    (condition-case nil
        (when-let* ((body (json-read))
                    (err (or (alist-get 'error body)
                             (plist-get body 'error)))
                    (message (or (alist-get 'message err)
                                 (plist-get err 'message)))
                    (message (if (and message (not (string-blank-p message)))
                                 message
                               (json-encode err))))
          (org-ai--show-error message))
      (error nil))))

(defun org-ai--show-error (error-message)
  "Show an error message in a buffer.
`ERROR-MESSAGE' is the error message to show."
  (condition-case nil
      (let ((buf (get-buffer-create "*org-ai error*")))
        (with-current-buffer buf
          (read-only-mode -1)
          (erase-buffer)
          (insert "Error from the service API:\n\n")
          (insert error-message)
          (display-buffer buf)
          (goto-char (point-min))
          (toggle-truncate-lines -1)
          (read-only-mode 1)
          ;; close buffer when q is pressed
          (local-set-key (kbd "q") (lambda () (interactive) (kill-buffer)))
          t))
    (error nil)))

(cl-defun org-ai--payload (&optional &key prompt messages model max-tokens temperature top-p frequency-penalty presence-penalty service stream)
  "Create the payload for the OpenAI API.
`PROMPT' is the query for completions `MESSAGES' is the query for
chatgpt. `MODEL' is the model to use. `MAX-TOKENS' is the
maximum number of tokens to generate. `TEMPERATURE' is the
temperature of the distribution. `TOP-P' is the top-p value.
`FREQUENCY-PENALTY' is the frequency penalty. `PRESENCE-PENALTY'
is the presence penalty.
`STREAM' is a boolean indicating whether to stream the response."
  (let ((extra-system-prompt)
        (max-completion-tokens))

    (when (eq service 'anthropic)
      (when (string-equal (plist-get (aref messages 0) :role) "system")
        (setq extra-system-prompt (plist-get (aref messages 0) :content))
        (cl-shiftf messages (cl-subseq messages 1)))
      (setq max-tokens (or max-tokens 4096)))

    ;; o1 models currently does not support system prompt
    (when (and (or (eq service 'openai) (eq service 'azure-openai))
               (or (string-prefix-p "o1" model) (string-prefix-p "o3" model)))
      (setq messages (cl-remove-if (lambda (msg) (string-equal (plist-get msg :role) "system")) messages))
      ;; o1 does not support max-tokens
      (when max-tokens
        (setq max-tokens nil)
        (setq max-completion-tokens (or max-tokens 128000))))

   (let* ((input (if messages `(messages . ,messages) `(prompt . ,prompt)))
          ;; TODO yet unsupported properties: n, stop, logit_bias, user
          (data (map-filter (lambda (x _) x)
                            `(,input
                              (model . ,model)
                              ,@(when stream                `((stream . ,stream)))
                              ,@(when max-tokens            `((max_tokens . ,max-tokens)))
                              ,@(when max-completion-tokens `((max-completion-tokens . ,max-completion-tokens)))
                              ,@(when temperature           `((temperature . ,temperature)))
                              ,@(when top-p                 `((top_p . ,top-p)))
                              ,@(when frequency-penalty     `((frequency_penalty . ,frequency-penalty)))
                              ,@(when presence-penalty      `((presence_penalty . ,presence-penalty)))))))

     (when extra-system-prompt
       (setq data (append data `((system . ,extra-system-prompt)))))

     (encode-coding-string (json-encode data) 'utf-8))))

(defun org-ai--url-request-on-change-function (_beg _end _len)
  "Look into the url-request buffer and manually extracts JSON stream responses.
Three arguments are passed to each function: the positions of
the beginning and end of the range of changed text,
and the length in chars of the pre-change text replaced by that range."
  (with-current-buffer org-ai--current-request-buffer-for-stream
    (when (and (boundp 'url-http-end-of-headers) url-http-end-of-headers)
      (save-match-data
        (save-excursion
          (if org-ai--url-buffer-last-position-marker
              (goto-char org-ai--url-buffer-last-position-marker)
            (goto-char url-http-end-of-headers)
            (setq org-ai--url-buffer-last-position-marker (point-marker)))

          ;; Avoid a bug where we skip responses because url has modified the http
          ;; buffer and we are not where we think we are.
          ;; TODO this might break
          (unless (eolp)
            (beginning-of-line))

          (let ((errored nil))
            ;; - Non-streamed - response of a single json object
            (if (not org-ai--current-request-is-streamed)
                (when (not errored)
                  (let ((json-object-type 'plist)
                        (json-key-type 'symbol)
                        (json-array-type 'vector))
                    (condition-case _err
                        (let (
                              (data (json-read-from-string
                                     (buffer-substring-no-properties
                                      (point) (point-max))))
                              ;; (data (json-read))  ; problem: with codepage, becaseu url buffer not utf-8
                              )
                          ;; (org-ai--debug data)
                          (when org-ai--current-request-callback
                            (funcall org-ai--current-request-callback data)))
                      (error
                       (setq errored t))))
                  ;; when
                  (when org-ai--current-request-callback
                    (funcall org-ai--current-request-callback nil))
                  ;; (org-ai--debug "test2")
                  (org-ai-reset-stream-state)
                  (message "org-ai request done"))

              ;; - else - streamed, multiple json objects prefixed with "data: "
              (while (and org-ai--current-request-is-streamed
                          (not errored)
                          (search-forward "data: " nil t))
                (let* ((line (buffer-substring-no-properties (point) (line-end-position)))
                       (tmp-buf "*org-ai--temp*"))
                  ;; (message "...found data: %s" line)
                  (if (not (string= line "[DONE]"))
                      (let ((json-object-type 'plist)
                            (json-key-type 'symbol)
                            (json-array-type 'vector))
                        (condition-case _err
                            (let (
                                  ;; (data (json-read-from-string line)) ; slow

                                  (data (with-current-buffer (get-buffer-create tmp-buf t) ; faster
                                          (erase-buffer)
                                          (insert line)
                                          (goto-char (point-min))
                                          (json-read)))
                                  )
                              ;; (org-ai--debug data)
                              (end-of-line)
                              ;; (setq org-ai--debug-data (append org-ai--debug-data (list data)))
                              (when org-ai--current-request-callback
                                (funcall org-ai--current-request-callback data))
                              (set-marker org-ai--url-buffer-last-position-marker (point)))
                          (error
                           (setq errored t)
                           (if (get-buffer tmp-buf)
                               (kill-buffer tmp-buf))
                           (when org-ai--url-buffer-last-position-marker
                             (goto-char org-ai--url-buffer-last-position-marker)))))
                    ;; - else DONE
                    (progn
                      (if (get-buffer tmp-buf)
                               (kill-buffer tmp-buf))
                      (when org-ai--current-request-callback
                        (funcall org-ai--current-request-callback nil))
                      (set-marker org-ai--url-buffer-last-position-marker (point))
                      (org-ai-reset-stream-state)
                      (message "org-ai request done"))
                    ))))))))))

(defun org-ai--stream-supported (service model)
  "Check if the stream is supported by the service and model.
`SERVICE' is the service to use. `MODEL' is the model to use."
  ;; stream not supported by openai o1 models
  (not (and (or (eq service 'openai) (eq service 'azure-openai))
            (or
             (string-prefix-p "o1-pro" model)))))

(defun org-ai-interrupt-current-request ()
  "Interrupt the current request."
  (interactive)
  (when (and org-ai--current-request-buffer-for-stream (buffer-live-p org-ai--current-request-buffer-for-stream))
    (let (kill-buffer-query-functions)
      (kill-buffer org-ai--current-request-buffer-for-stream))
    (setq org-ai--current-request-buffer-for-stream nil)
    (org-ai-reset-stream-state)))

(defun org-ai-reset-stream-state ()
  "Reset the stream state."
  (interactive)
  (when (and org-ai--current-request-buffer-for-stream (buffer-live-p org-ai--current-request-buffer-for-stream))
    (with-current-buffer org-ai--current-request-buffer-for-stream
      (remove-hook 'after-change-functions #'org-ai--url-request-on-change-function t)
      (setq org-ai--url-buffer-last-position-marker nil)))
  (setq org-ai--current-request-callback nil)
  (setq org-ai--url-buffer-last-position-marker nil)
  (setq org-ai--current-chat-role nil)
  (setq org-ai--current-request-is-streamed nil)
  (org-ai--progress-reporter-cancel))

(defvar org-ai--progress-reporter-waiting-for-response "Waiting for a response")

(defun org-ai--progress-reporter-until-request-done ()
  "
Set
- `org-ai--current-progress-reporter' - lambda that return a string.
- `org-ai--current-progress-timer' - timer that output /-\ to echo area.
- `org-ai--current-progress-timer-remaining-ticks'
"
  (org-ai--progress-reporter-cancel)
  (setq org-ai--current-progress-reporter (make-progress-reporter org-ai--progress-reporter-waiting-for-response))

  (let ((repeat-every-sec 0.2))
    ;; - precalculate ticks based on duration
    (setq org-ai--current-progress-timer-remaining-ticks
                (round (/ org-ai-progress-duration repeat-every-sec)))
    ;; (org-ai--debug "Create timer" org-ai--current-progress-timer-remaining-ticks)

    ;; - Run timer after 1 sec:
    (setq org-ai--current-progress-timer
          (run-with-timer
           1.0 repeat-every-sec
           (lambda ()
             ;; (org-ai--debug "In timer" org-ai--current-progress-timer-remaining-ticks)
             (if (<= org-ai--current-progress-timer-remaining-ticks 0)
                   ; failed
                   (org-ai--progress-reporter-cancel 'failed)
               ;; else
               (progress-reporter-update org-ai--current-progress-reporter)
               (setq org-ai--current-progress-timer-remaining-ticks
                     (1- org-ai--current-progress-timer-remaining-ticks))))))))

(defun org-ai--progress-reporter-cancel (&optional failed)
  "Stop reporter for not stream."
  (when org-ai--current-progress-reporter

    (if failed ; timeout
        (progn ; from `url-queue-kill-job'
          ;; (progress-reporter-done org-ai--current-progress-reporter)
          (progress-reporter-update org-ai--current-progress-reporter nil "- Connection failed")
          (message (concat org-ai--progress-reporter-waiting-for-response "- Connection failed"))
          (setq org-ai--current-progress-reporter nil)
          (when (buffer-live-p org-ai--current-request-buffer-for-stream)
            ;; (ignore-errors
              (let ((proc (get-buffer-process org-ai--current-request-buffer-for-stream)))
              ;; (interrupt-process (get-buffer-process org-ai--current-request-buffer-for-stream))
              ;; (kill-process (get-buffer-process org-ai--current-request-buffer-for-stream))
              (set-process-sentinel proc 'ignore)
              ;; (with-current-buffer org-ai--current-request-buffer-for-stream
              ;;   (let ((buffer-modified-p nil))
              ;;     (kill-buffer org-ai--current-request-buffer-for-stream)))

              ;; (setq org-ai--current-request-buffer-for-stream nil)
              (delete-process proc)
              ))
          )
      ;; else success
      (progress-reporter-done org-ai--current-progress-reporter)
      (setq org-ai--current-progress-reporter nil)))
  ;; clear time
  (when org-ai--current-progress-timer
    (cancel-timer org-ai--current-progress-timer)
    (setq org-ai--current-progress-timer-remaining-ticks 0)))

(defun org-ai-open-request-buffer ()
  "A debug helper that opens the url request buffer."
  (interactive)
  (when (buffer-live-p org-ai--current-request-buffer-for-stream)
    (pop-to-buffer org-ai--current-request-buffer-for-stream)))

(defun org-ai-switch-chat-model ()
  "Change `org-ai-default-chat-model'."
  (interactive)
  (let ((model (completing-read "Model: "
                                (append org-ai-chat-models
                                        '("claude-3-opus-latest" "claude-3-5-sonnet-latest" "claude-3-7-sonnet-latest"
                                          "gemini-2.5-pro-preview-03-25" "gemini-2.5-flash-preview-04-17" "gemini-2.0-flash" "gemini-2.0-pro-exp"
                                          "deepseek-chat" "deepseek-reasoner"))
                                nil t)))
    (setq org-ai-default-chat-model model)))


(provide 'org-ai-openai)

;;; org-ai-openai.el ends here
