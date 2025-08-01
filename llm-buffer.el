;;; llm-buffer -- a tool for feeding structured buffers to an LLM -*- lexical-binding: t -*-
;;;
;;; Much simpler than ellama.  Focussed on creative single-buffer stuff.
;;;
;;; TODO:
;;; https://github.com/ggml-org/llama.cpp/blob/baad94885df512bb24ab01e2b22d1998fce4d00e/tools/server/server.cpp#L261-L308
;;; suggests a list of non-standard params.  There should be a way of
;;; specifying arbitrary parameters like this.
;;;
;;; TODO: It might be important to retain the same llm-chat-prompt
;;; object between requests so that the LLM can cache.  That doesn't
;;; seem to matter to llama-cpp's llama-server.  To do that, we could
;;; maintain the object in a buffer local variable and update it,
;;; rather than generating a new one.

(require 'cl-lib)
(require 'llm) ; See <https://github.com/ahyatt/llm>.
(require 'llm-openai)

(defface llm-buffer-waiting '((t :inherit warning))
  "Face used for temporary waiting message."
  :group 'llm-buffer-faces)

(defface llm-buffer-partial '((t :inherit success))
  "Face used for partially inserted LLM response."
  :group 'llm-buffer-faces)

(defconst llm-buffer-partial-props
  '(face llm-buffer-partial
    font-lock-face llm-buffer-partial))

;; Requires llama-server running locally, e.g. ::
;;   ./build/bin/llama-server -m models/Meta-Lllama.gguf

(defcustom llm-buffer-provider
  (make-llm-openai-compatible :url "http://127.0.0.1:8080")
  "Backend LLM provider."
  :type '(sexp :validate llm-standard-provider-p))

(defcustom llm-buffer-separator "^---$"
  "Regular expression use to divide buffers into chat parts."
  :type 'regexp
  ;; Can be overriden in e.g. file local variables.
  :local t)

(defcustom llm-buffer-comment "^--- .*\n?"
  "Regular expression used to identify commentary which is not to
be sent to the LLM."
  :type 'regexp
  ;; Can be overriden in e.g. file local variables.
  :local t)

(defvar-local llm-buffer-canceller nil)

(defun llm-buffer-cancel ()
  "Cancel the LLM request that's inserting into the buffer."
  (when llm-buffer-canceller
    (funcall llm-buffer-canceller)
    (setq llm-buffer-canceller nil)
    ))

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
          ;; TODO: Consider comment replacement *after* splitting?
          (replace-regexp-in-string
           llm-buffer-comment ""
           (if (use-region-p)
               (buffer-substring-no-properties (region-beginning) (region-end))
             (buffer-substring-no-properties (point-min) (point-max)))))
         ;; Try to split the text into parts with the separator
         (split (split-string text llm-buffer-separator nil "\\s-*"))
         ;; Drop or add empty last element to allow chat to continue
         (split-length (length split))
         (parts
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
     ((> (length parts) 1)
      (llm-make-chat-prompt (cdr parts) :context (car parts) :temperature temperature))
     ;; Use the content of the "system-prompt" buffer if it exists
     (sys
      (llm-make-chat-prompt text
                            :context (with-current-buffer sys (buffer-string))
                            :temperature temperature))
     ;; Otherwise send the whole text
     (t
      (llm-make-chat-prompt text :temperature temperature)))))

(defun llm-buffer-waiting-text (prompt)
  "Compose waiting message text to insert into the buffer as a
placeholder while waiting the LLM to respond."
  ;; NOTE: Nothing in this text is necessary to the function of the
  ;; rest of the llm-buffer module, so it could be simplified.
  (let* ((part-count (length (llm-chat-prompt-interactions prompt)))
         (temperature (llm-chat-prompt-temperature prompt))
         (interactions (llm-chat-prompt-interactions prompt))
         (token-count
          (cl-reduce
           #'+
           (mapcar
            (lambda (interaction)
              (llm-count-tokens llm-buffer-provider
                                (llm-chat-prompt-interaction-content interaction)))
            interactions)))
         (last (llm-chat-prompt-interaction-content (car (last interactions))))
         (empty-last (string= last "")))
    ;; TODO: Could include provider or model name rather than just "LLM".
    (format "[Sending approx %d tokens from %s%s parts%s.  Waiting for LLM...]"
            token-count
            (if (llm-chat-prompt-context prompt)
                "system prompt and "
              "")
            (if empty-last
                (format "%d+1" (- part-count 1))
              part-count)
            (if temperature
                (format " at temperature %g" temperature)
              ""))))

(defun llm-buffer-inserter (buffer beg end)
  "Make an insertion callback for llm-chat-streaming that appends
the LLM output to a region.  The first call to the callback
replaces the entire region, including the waiting message, but
subsequent calls only append to the region, so that it can be
edited by the user while the LLM is still generating."
  (let ((prefix ""))
    (lambda (text)
      (with-current-buffer buffer
        ;; TODO: prefix should always be a prefix
        (if (and (not (string-empty-p prefix))
                 (string-prefix-p prefix text))
            (save-excursion
              (goto-char end)
              (insert
               (propertize (substring text (length prefix))
                           'face 'llm-buffer-partial
                           'font-lock-face 'llm-buffer-partial)))
          (replace-region-contents beg end (lambda () text))
          (add-text-properties beg end llm-buffer-partial-props))
        (setq prefix text)))))

(defun llm-buffer (&optional centitemp)
  "Send the region or buffer to the LLM, scheduling the response to arrive at the point.

If the buffer contains lines like \"---\" then the first part is
sent as the system prompt, and the remaining parts are sent as a
chat conversation.

A prefix argument may be used to specify the LLM temperature for
the request in hundredths, e.g. a prefix argument of 75 is a
temperature of 0.75."
  (interactive "P")
  (when llm-buffer-canceller (funcall llm-buffer-canceller))
  (let* ((prompt (llm-buffer-to-prompt centitemp))
         ;; Remember where to insert the results
         (request-buffer (current-buffer))
         (end-marker (copy-marker (point) t))
         (beg-marker (copy-marker end-marker nil))
         ;; Insert the partial result into the buffer by replacing the
         ;; previous partial result.
         (partial-callback (llm-buffer-inserter request-buffer beg-marker end-marker))
         (finish-text
          (lambda ()
            ;; TODO: Could incorporate remove-waiting?
            (remove-text-properties beg-marker end-marker llm-buffer-partial-props)))
         ;; When the final result arrives, put it in the buffer and cancel the mode.
         (response-callback
          (lambda (text)
            (funcall partial-callback text)
            (funcall finish-text)
            (with-current-buffer request-buffer
              ;; TODO: Insert separator.
              (llm-request-mode 0)
              (setq llm-buffer-canceller nil))))
         (waiting-text (propertize (llm-buffer-waiting-text prompt)
                                   'face 'llm-buffer-waiting
                                   'font-lock-face 'llm-buffer-waiting))
         (remove-waiting
          ;; Remove the placeholder if the request was cancelled
          ;; before any text arrived.
          (lambda ()
            (with-current-buffer request-buffer
              (when (string= (buffer-substring beg-marker end-marker) waiting-text)
                (delete-region beg-marker end-marker)))))
         (error-callback
          (lambda (_ msg)
            (with-current-buffer request-buffer (llm-request-mode 0))
            (funcall remove-waiting)
            (funcall finish-text)
            ;; TODO: Maybe the error message should be inserted?
            (error msg))))
    ;; Insert the waiting text
    (replace-region-contents beg-marker end-marker (lambda () waiting-text))
    ;; Send the request to the LLM, setting up a cancel operation.
    (llm-request-mode 1)
    (let* ((request 
            (llm-chat-streaming llm-buffer-provider prompt
                                partial-callback
                                response-callback
                                error-callback))
           (canceller
            (lambda ()
              (llm-cancel-request request)
              (funcall remove-waiting)
              (funcall finish-text))))
      (setq llm-buffer-canceller canceller)
      ;; Cancel the LLM request if the output text is killed.  This also
      ;; catches the case where the whole buffer is killed.
      (letrec ((timer (run-at-time
                       t 5
                       (lambda ()
                         (with-current-buffer request-buffer
                           ;; Is the request still running?
                           (if (eq llm-buffer-canceller canceller)
                               ;; Is the output region still there?
                               (when (equal beg-marker end-marker)
                                 (llm-buffer-cancel))
                             ;; Request isn't running, so kill timer.
                             (cancel-timer timer)))))))
        (add-hook 'kill-buffer-hook
                  (lambda () (when (timerp timer) (cancel-timer timer))))))))

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

(provide 'llm-buffer)

;; End.
