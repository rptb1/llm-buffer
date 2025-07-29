;;; llm-buffer -- a tool for feeding structured buffers to an LLM -*- lexical-binding: t -*-
;;;
;;; Much simpler than ellama.  Focussed on creative single-buffer stuff.

(provide 'llm-buffer)

(require 'llm) ; See <https://github.com/ahyatt/llm>.
(require 'llm-openai)

;; Requires llama-server running locally, e.g. ::
;;   ./build/bin/llama-server -m models/Meta-Lllama.gguf

(defcustom llm-buffer-provider
  (make-llm-openai-compatible :url "http://127.0.0.1:8080")
  "Backend LLM provider."
  :type '(sexp :validate llm-standard-provider-p))

(defcustom llm-buffer-separator "^---$"
  "Regular expression use to divide buffers into chat chunks."
  ;; Can be overriden in e.g. file local variables.
  :local t)

(defvar-local llm-buffer-request nil)

(defun llm-buffer-cancel ()
  "Cancel the LLM request that's inserting into the buffer."
  (when llm-buffer-request
    (llm-cancel-request llm-buffer-request)
    (setq llm-buffer-request nil)))

;; TODO: I inherited this overloading of quit from ellama and I'm not
;; sure I like it.
(defun llm-buffer-cancel-quit ()
  "Cancel the LLM request that's inserting into the buffer and quit."
  (interactive)
  (llm-buffer-cancel)
  (keyboard-quit))

;; This minor mode is applied when the buffer has an active
;; asynchronous request to the LLM that might be inserting text.  The
;; purpose of the mode is to allow the request to be cancelled, and
;; make it visible that this is happening.
(define-minor-mode llm-request-mode
  "Minor mode for buffers with active LLM requests."
  :interactive nil
  :lighter " LLM"
  :keymap '(([remap keyboard-quit] . llm-buffer-cancel-quit))
  (if llm-request-mode
      (add-hook 'kill-buffer-hook 'llm-buffer-cancel nil t)
    (remove-hook 'kill-buffer-hook 'llm-buffer-cancel t)
    (llm-buffer-cancel)))

;; TODO: This is where things should be clever, e.g. breaking an RST
;; document into sections, looing for temperature hints, etc.  Perhaps
;; dispatch by mode.
(defun llm-buffer-to-prompt (&optional centitemp)
  "Form an LLM prompt from the region or buffer."
  (let* (;; The input text
         (text
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (buffer-substring-no-properties (point-min) (point-max))))
         ;; Try to split the text into chunks with the separator
         (split (split-string text llm-buffer-separator nil "\\s-*"))
         ;; Drop or add empty last element to allow chat to continue
         (split-length (length split))
         (chunks
          (cond
           ((= (* (/ split-length 2) 2) split-length) split)
           ((string= (car (last split)) "") (butlast split))
           (t (append split '("")))))
         (temperature (when centitemp (/ centitemp 100.0)))
         ;; Possible system prompt buffer
         (sys (get-buffer "system-prompt")))
    ;; Form a prompt
    (cond
     ;; Check if the text contains the split regexp
     ((> (length chunks) 1)
      (llm-make-chat-prompt (cdr chunks) :context (car chunks) :temperature temperature))
     ;; Use the content of the "system-prompt" buffer if it exists
     (sys
      (llm-make-chat-prompt text
                            :context (with-current-buffer sys (buffer-string))
                            :temperature temperature))
     ;; Otherwise send the whole text
     (t
      (llm-make-chat-prompt text :temperature temperature)))))

(defun llm-buffer (centitemp)
  "Send the region or buffer to the LLM, scheduling the response to arrive at the point.

If the buffer contains lines like \"---\" then the first chunk is
sent as the system prompt, and the remaining chunks are sent as a
chat conversation.

A prefix argument may be used to specify the LLM temperature for
the request in hundredths, e.g. a prefix argument of 75 is a
temperature of 0.75."
  (interactive "P")
  (let* ((prompt (llm-buffer-to-prompt centitemp))
         ;; Remember where to insert the results
         (request-buffer (current-buffer))
         (end-marker (copy-marker (point) t))
         (beg-marker (copy-marker end-marker nil))
         ;; The partial result is built up in this variable as it arrives
         (partial-text "")
         ;; Insert the partial result into the buffer by replacing the
         ;; previous partial result.
         (partial-callback
          (lambda (text)
            (with-current-buffer request-buffer
              (replace-region-contents beg-marker end-marker (lambda () text)))))
         ;; When the final result arrives, put it in the buffer and cancel the mode.
         (response-callback
          (lambda (text)
            (funcall partial-callback text)
            (with-current-buffer request-buffer
              ;; TODO: Insert separator.
              (llm-request-mode 0))))
         ;; TODO: This message could contain useful information.
         ;; TODO: This message, and the insertion, could be in a highlighted face.
         (chunk-count (length (llm-chat-prompt-interactions prompt)))
         (temperature (llm-chat-prompt-temperature prompt))
         (waiting-text (format "[Sending %d chunks%s.  Waiting for LLM...]"
                               chunk-count
                               (if temperature
                                   (format " at temperature %g" temperature)
                                 "")))
         (error-callback
          (lambda (_ msg)
            (with-current-buffer request-buffer
              (llm-request-mode 0)
              ;; Remove the placeholder if the request was cancelled
              ;; before any text arrived.
              (when (string= (buffer-substring beg-marker end-marker) waiting-text)
                (delete-region beg-marker end-marker)))
            (error msg))))
    (replace-region-contents beg-marker end-marker (lambda () waiting-text))
    (llm-request-mode 1)
    (setq llm-buffer-request
          (llm-chat-streaming llm-buffer-provider prompt
                                    partial-callback
                                    response-callback
                                    error-callback))))

;; Llama LLM integration
;; Requires llama-server running locally, e.g. ::
;;   ./build/bin/llama-server -m models/Meta-Lllama.gguf
;;
;; On Tails, requieres plz-curl-default-args has "--proxy ''" so that
;; the LLM module can talk to localhost.
;;
;; Test with (llm-chat (make-llm-openai-compatible :url "http://127.0.0.1:8080") (llm-make-chat-prompt "Who are you?"))
;;
;; I'm not using most of ellama, just a way to extend the current
;; buffer in a context-free manner, by sending it to the LLM.  See
;; <https://elpa.gnu.org/packages/ellama.html>.

;;(require 'llm)
;;(require 'llm-openai)
;; (require 'ellama)

;; ;; Send the current region or buffer, scheduling the response to
;; ;; arrive at the point.  If the text contains "---" then split it into
;; ;; a system prompt and conversation at each subsequent "---".  Or if
;; ;; there's a buffer called "system-prompt" use that.  Otherwise send
;; ;; the whole buffer without specifying a system prompt.
;; (defun ellama-buffer ()
;;   (interactive)
;;   (let* ((text
;;           (if (use-region-p)
;;               (buffer-substring-no-properties (region-beginning) (region-end))
;;             (buffer-substring-no-properties (point-min) (point-max))))
;;          (split (split-string text "^---$" nil "\\s-*"))
;;          ;; Drop empty last element to allow for trailing separator
;;          (texts (if (string= (car (last split)) "") (butlast split) split))
;;          (sys (get-buffer "system-prompt")))
;;     (cond
;;      ;; Check if the text contains the split regexp
;;      ((> (length texts) 1)
;;       (ellama-stream (cdr texts) :system (car texts)))
;;      ;; Use the content of the "system-prompt" buffer if it exists
;;      (sys
;;       (ellama-stream text :system (with-current-buffer sys (buffer-string))))
;;      ;; Send the whole text
;;      (t 
;;       (ellama-stream text)))))

;; (define-key global-map (kbd "C-c e") 'ellama-buffer)

;; End.
