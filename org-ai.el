;;; org-ai.el --- Refactored Use ChatGPT and other LLMs in org-mode and beyond -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Robert Krahn and contributers
;; Copyright (C) 2025 github.com/Anoncheg1

;; URL: https://github.com/Anoncheg1/org-aire
;; Version: 0.1,  Orig. Version: 0.5.6
;;
;; Package-Requires: ((emacs "27.1") (websocket "1.15"))

;;; Changes:
;; - DONE: remove websocket
;; - DONE: additional should be additional
;; - TODO: make org-ai-block dependent on org and org-ai-openai dependent on url only
;; - TODO: rename all to "file-shit" naming convention
;; - TODO: guide for connecting to custom LLM provider.
;; - TODO: separate 'org-ai-block and 'org-ai-openai, for now org-ai-openai dependes on org-ai-block and org.
;; - TODO: make org-ai-variable.el and pass them to -api.el functions as parameters.
;; - DONE: :stream nil/t parameter
;; - TODO: rename "org-ai-stream-completion" to "org-ai-openai-call"
;; - TODO: where in org-ai-expand-block is it really what will be send? check corectness of roles with json
;; - TODO: Guide to add custom functions for text post-processing.
;; - TODO: provide ability to replace url-http with plz or org-ai-openai with llm(plz)
;; - TODO: Org properties extraction in separate function.
;;
;;; License

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

;;; Commentary:

;; Refactored version of "org-ai".
;; Provides a minor-mode for org-mode and a global minor-mode that allows you to
;; interact with the OpenAI API (and, with Stable Diffusion, as well as various local LLMs.)
;;
;; It allows you to:
;; - #+begin_ai..#+end_ai blocks for org-mode
;; - "chat" with a language model from within an org mode buffer
;; - generate images
;; - has support for speech input and output
;; - various commands usable everywhere
;;
;; For the Internet connection used built-in libs: url.el and url-http.el.
;;
;; See see https://github.com/rksm/org-ai for the full set of features and setup
;; instructions.
;;
;; Configuration:
;;
;; (add-to-list 'load-path "path/to/org-ai")
;; (require 'org)
;; (require 'org-ai)
;; (setopt org-ai-default-chat-model "gpt-4") ; org-ai-openai.el
;; (add-hook 'org-mode-hook #'org-ai-mode) ; org-ai.el
;; (org-ai-global-mode) ; org-ai.el

;;
;; You will need an OpenAI API key. It can be stored in the format
;;   machine api.openai.com login org-ai password <your-api-key>
;; in your ~/.authinfo.gpg file (or other auth-source) and will be picked up
;; when the package is loaded.
;;
;; For the speech input/output setup please see
;; https://github.com/rksm/org-ai/blob/master/README.md#setting-up-speech-input--output
;;
;; Available commands (TODO to refine):
;;
;; - Inside org-mode / #+begin_ai..#+end_ai blocks:
;;     - C-c C-c to send the text to the OpenAI API and insert a response
;;     - Press C-c <backspace> (org-ai-kill-region-at-point) to remove the chat part under point.
;;     - org-ai-mark-region-at-point will mark the region at point.
;;     - org-ai-mark-last-region will mark the last chat part.
;;
;; - Speech input/output. Talk with your AI!
;;     - In org-mode / #+begin_ai..#+end_ai blocks:
;;       - C-c r to record and transcribe speech via whisper.el in org blocks.
;;     - Everywhere else:
;;         - Enable speech input with org-ai-talk-input-toggle for other commands (see below).
;;     - Enable speech output with org-ai-talk-output-enable. Speech output uses os internal speech synth (macOS) or espeak otherwise.
;;     - See [Setting up speech input / output](#setting-up-speech-input--output) below for more details.
;;
;; - Non-org-mode commands
;;     - org-ai-on-region: Ask a question about the selected text or tell the AI to do something with it.
;;     - org-ai-refactor-code: Tell the AI how to change the selected code, a diff buffer will appear with the changes.
;;     - org-ai-on-project: Query / modify multiple files at once. Will use projectile if available.
;;     - org-ai-prompt: prompt the user for a text and then print the AI's response in current buffer.
;;     - org-ai-summarize: Summarize the selected text.
;;     - org-ai-explain-code: Explain the selected code.
;;
;; - org-ai-open-account-usage-page show how much money you burned.
;; - org-ai-install-yasnippets install snippets for #+begin_ai..#+end_ai blocks.
;; - org-ai-open-request-buffer for debugging, open the request buffer.

;;; Code:

(require 'org-ai-block)
(require 'org-ai-openai)
;; (require 'org-ai-openai-image)
;; (require 'org-ai-useful)
;; (require 'org-ai-on-project)
;; (require 'org-ai-talk)
;; (require 'org-ai-sd)
;; (require 'org-ai-oobabooga)

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; -= C-c C-c
(defun org-ai-ctrl-c-ctrl-c ()
  "C-c C-c command step 1) for #+begin_ai."
  (when-let ((context (org-ai-special-block))) ; org-ai-block.el
    (org-ai-complete-block) ; here
    t))

(defun org-ai-complete-block ()
  "C-c C-c command step 2) for #+begin_ai.
Send the text content to the OpenAI API and replace the block with the
result."
  (interactive)
  (let* ((context (org-ai-special-block)) ; org-ai-block.el
         (info (org-ai-get-block-info context)) ; org-ai-block.el
         (content (org-ai-get-block-content context)) ; org-ai-block.el
         (req-type (org-ai--get-request-type info)) ; org-ai-block.el
         (sys-prompt-for-all-messages (or (not (eql 'x (alist-get :sys-everywhere info 'x)))
                                          (org-entry-get-with-inheritance "SYS-EVERYWHERE") ; org
                                          org-ai-default-inject-sys-prompt-for-all-messages)) ; org-ai-openai.el
         (sys-prompt (or (org-entry-get-with-inheritance "SYS") ; org
                                    (org-ai--get-sys :info info ; org-ai-block.el
                                                     :default org-ai-default-chat-system-prompt)))) ; org-ai-openai.el variable
    (cl-case req-type
      (completion (org-ai-stream-completion :prompt content ; org-ai-openai.el
                                            :context context))
      ;; (image (org-ai-create-and-embed-image context)) ; org-ai-openai-image.el
      ;; (sd-image (org-ai-create-and-embed-sd context)) ; org-ai-sd.el
      ;; (local-chat (org-ai-oobabooga-stream :messages (org-ai--collect-chat-messages ; org-ai-oobabooga
      ;;                                                 content
      ;;                                                 default-system-prompt
      ;;                                                 sys-prompt-for-all-messages)
      ;;                                      :context context))
      ;; by default:
      (t (org-ai-stream-completion :messages (org-ai--collect-chat-messages ; org-ai-openai
                                              content
                                              sys-prompt
                                              sys-prompt-for-all-messages)
                                   :context context)))))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; -= M-x org-ai-expand-block
;;;###autoload
(defun org-ai-expand-block (&optional context)
  "M-x org-ai-expand-block
Show a temp buffer with what the org-ai block expands to. This is what
will be sent to the api. CONTEXT is the org-ai block."
  (interactive)
  (let* ((context (or context (org-ai-special-block))) ; org-ai-block.el
         (expanded (org-ai-get-block-content context))) ; org-ai-block.el
    (if (called-interactively-p 'any)
        (let ((buf (get-buffer-create "*Org-Ai Preview*")))
          (with-help-window buf (with-current-buffer buf
                                  (insert expanded))))
      expanded)))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; keyboard quit

(defvar org-ai-talk--reading-process)

(defun org-ai-keyboard-quit ()
  "Keyboard quit advice.
It's designed to \"do the right thing\":
- If there is an active region, do nothing (normal \\<mapvar> & \\[keyboard-quit] will deactivate it).
- If there is speech recorded or played, stop it.
- If there is currently a running openai request, stop it."
  (interactive)
  (condition-case _
      (cond
       ((region-active-p) nil)
       ;; ((and (boundp 'org-ai-talk--reading-process) ; org-ai-talk.el
       ;;       (fboundp 'org-ai-talk-stop) ; org-ai-talk.el
       ;;       org-ai-talk--reading-process ; org-ai-talk.el
       ;;       (process-live-p org-ai-talk--reading-process)) ; org-ai-talk
       ;;  (org-ai-talk-stop)) ; org-ai-talk
       ;; (org-ai-oobabooga--current-request ; org-ai-oobabooga
       ;;  (org-ai-oobabooga-stop)) ; org-ai-oobabooga
       (org-ai--current-request-buffer-for-stream ; org-ai-openai.el
        (org-ai-interrupt-current-request)) ; org-ai-openai.el
       (org-ai--current-request-buffer ; org-ai-openai.el
        (org-ai-interrupt-current-request)) ; org-ai-openai.el
       ;; (org-ai--current-request-buffer-for-image ; org-ai-openai-image.el
       ;;  (org-ai-image-interrupt-current-request)) ; org-ai-openai-image.el
       )
    (error nil)))

(defun org-ai--install-keyboard-quit-advice ()
  "Cancel current request when `keyboard-quit' is called."
  (unless (advice-member-p #'org-ai-keyboard-quit 'keyboard-quit) ; here
    (advice-add 'keyboard-quit :before #'org-ai-keyboard-quit)))

(defun org-ai--uninstall-keyboard-quit-advice ()
  "Remove the advice that cancels current request when `keyboard-quit' is called."
  (advice-remove 'keyboard-quit #'org-ai-keyboard-quit)) ; here

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defvar org-ai-mode-map (make-sparse-keymap)
  "Keymap for `org-ai-mode'.")

(let ((map org-ai-mode-map))
  ;; (define-key map (kbd "C-c M-a v") 'org-ai-image-variation) ; org-ai-openai-image.el
  ;; (define-key map (kbd "C-c M-a $") 'org-ai-open-account-usage-page) ; org-ai-openai-image.el
  (define-key map (kbd "C-c M-a SPC") 'org-ai-mark-region-at-point) ; org-ai-block.el
  ;; (define-key map (kbd "C-c DEL") 'org-ai-kill-region-at-point) ; org-ai-block.el
  (define-key map (kbd "C-c <backspace>") 'org-ai-kill-region-at-point) ; org-ai-block.el
  ;; (define-key map (kbd (string-join (list "C-c" " r"))) 'org-ai-talk-capture-in-org) ; org-ai-talk.el
  )

;; create a minor-mode for org-mode
(define-minor-mode org-ai-mode
  "Minor mode for `org-mode' integration with the OpenAI API."
  :init-value nil
  :lighter " org-ai"
  :keymap org-ai-mode-map
  :group 'org-ai
  (add-hook 'org-ctrl-c-ctrl-c-hook #'org-ai-ctrl-c-ctrl-c nil t))

(org-ai--install-keyboard-quit-advice) ; here

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defvar org-ai-global-prefix-map (make-sparse-keymap)
  "Keymap for `org-ai-global-mode'.")

(let ((map org-ai-global-prefix-map)) ; here
  ;; (define-key map (kbd "p") 'org-ai-on-project) ; org-ai-on-project.el
  ;; (define-key map (kbd "P") 'org-ai-prompt-in-new-buffer) ; org-ai-useful.el
  ;; (define-key map (kbd "r") 'org-ai-on-region) ; org-ai-useful.el
  ;; (define-key map (kbd "c") 'org-ai-refactor-code) ; org-ai-useful.el
  ;; (define-key map (kbd "s") 'org-ai-summarize) ; org-ai-useful.el
  (define-key map (kbd "m") 'org-ai-switch-chat-model) ; org-ai-openai.el
  (define-key map (kbd "!") 'org-ai-open-request-buffer) ; org-ai-openai.el
  ;; (define-key map (kbd "$") 'org-ai-open-account-usage-page) ; org-ai-openai-image.el
  ;; (define-key map (kbd "t") 'org-ai-talk-input-toggle) ; org-ai-talk.el
  ;; (define-key map (kbd "T") 'org-ai-talk-output-toggle) ; org-ai-talk.el
  ;; (define-key map (kbd "R") 'org-ai-talk-read-region) ; org-ai-talk.el
  (define-key map (kbd "SPC") 'org-ai-mark-region-at-point)) ; org-ai-block.el

(defvar org-ai-global-mode-map (make-sparse-keymap)
  "Keymap for `org-ai-global-mode'.")

(define-key org-ai-global-mode-map (kbd "C-c M-a") org-ai-global-prefix-map) ; here

;;;###autoload
(define-minor-mode org-ai-global-mode
  "Non `org-mode' specific minor mode for the OpenAI API."
  :init-value nil
  :lighter ""
  :global t
  :keymap org-ai-global-mode-map
  :group 'org-ai)

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(provide 'org-ai)

;;; org-ai.el ends here
