;;; llm-buffer -- a tool for feeding structured buffers to an LLM -*- lexical-binding: t -*-
;;;
;;; Much simpler than ellama.  Focussed on creative single-buffer stuff.
;;;
;;; Recommended:
;;; (define-key global-map (kbd "C-c e") 'ellama-buffer)
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
;;
;; On Tails, requieres plz-curl-default-args has "--proxy ''" so that
;; the LLM module can talk to localhost.

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

(defcustom llm-buffer-temperature nil
  "If non-nil, override the default temperature for the model.
Can be set per-buffer.  This will be overridden by a prefix
argument to llm-buffer."
  :type 'float
  :local t)

;; TODO: Could be more flexible to allow post-processing.
(defcustom llm-buffer-prefix ""
  "A string to insert before the LLM output when it starts
arriving, such as \"assistant: \"."
  :type 'string
  :local t)

(defcustom llm-buffer-postfix ""
  "A string to append after the LLM output when it is complete,
such as \"\\\\n---\\\\n\"."
  :type 'string
  :local t)

(defvar-local llm-buffer-canceller nil
  "When non-nil, there is an LLM request running in the buffer,
and this function can be called to cancel it.")

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

(defun llm-buffer-split (&optional centitemp)
  "Form an LLM prompt from the region or buffer by splitting the
buffer at the regular expression in llm-buffer-separator.

The first part is sent as a system prompt.  The rest are sent as
a chat conversation, between the user and the LLM as assistant.
If necessary, an empty user prompt is appended.

If there are no separators, the whole buffer is sent.  And if a
buffer named system-prompt exists, it is sent as a system
prompt."
  (let* (;; The input text
         (text
           (if (use-region-p)
               (buffer-substring-no-properties (region-beginning) (region-end))
             (buffer-substring-no-properties (point-min) (point-max))))
         ;; Try to split the text into parts with the separator
         (parts (split-string text llm-buffer-separator nil "\\s-*"))
         ;; Strip comments
         ;; TODO: Replace with space?
         (parts
          (mapcar
           (lambda (string)
             (replace-regexp-in-string llm-buffer-comment "" string))
           parts))
         (temperature (or (when centitemp (/ centitemp 100.0))
                          llm-buffer-temperature))
         ;; Possible system prompt buffer
         (sys (get-buffer "system-prompt")))
    ;; Form a prompt
    (cond
     ;; Check if the text contains the split regexp
     ((> (length parts) 1)

      (let* ((system (car parts))
             (parts (cdr parts))
             ;; Parts must have odd length, so append or remove empty
             ;; last part as necessary.
             (parts
              (cond
               ((= (mod (length parts) 2) 1) parts)
               ((string= (car (last parts)) "") (butlast parts))
               (t (append parts '(""))))))
        (llm-make-chat-prompt parts
                              :context system :temperature temperature)))

     ;; Use the content of the "system-prompt" buffer if it exists
     (sys
      (llm-make-chat-prompt text
                            :context (with-current-buffer sys (buffer-string))
                            :temperature temperature))

     ;; Otherwise send the whole text
     (t
      (llm-make-chat-prompt text :temperature temperature)))))

(defvar-local llm-buffer-to-prompt #'llm-buffer-split
  "Form an llm-chat-prompt from the region or buffer.

This function may be overriden per buffer, so that buffers can
use different kinds of markup.")

(defun llm-buffer-markers-to-prompt (start-regexp &optional end-regexp centitemp)
  "Form an LLM prompt from the region or buffer by looking for
text between start-regexp and end-regexp (or another start-regexp
if not supplied).  The start-regexp should return a match 1 of
either \"system\", \"user\" or \"assistant\" to indicate the role
of the part.  The first \"system\" part is used as the context
field in the prompt, and subsequent system parts are ignored."
  (let* ((start (if (use-region-p) (region-beginning) (point-min)))
         (end (if (use-region-p) (region-end) (point-max)))
         (temperature (or (when centitemp (/ centitemp 100.0))
                          llm-buffer-temperature))
         (prompt (make-llm-chat-prompt
                  :temperature temperature)))
    (save-excursion
      (goto-char start)
      (while (re-search-forward start-regexp end t)
        (let ((role (intern (match-string 1)))
              (text-start (match-end 0)))
          (if (re-search-forward (or end-regexp start-regexp) end t)
              (goto-char (match-beginning 0))
            (goto-char end))
          (let ((text
                 (string-trim
                  (replace-regexp-in-string
                   llm-buffer-comment "" ; TODO: replace with space?
                   (buffer-substring-no-properties text-start (point))))))
            (if (eq role 'system)
                (setf (llm-chat-prompt-context prompt)
                      (or (llm-chat-prompt-context prompt) text))
              (llm-chat-prompt-append-response prompt text role))))))
    (when (= (mod (length (llm-chat-prompt-interactions prompt)) 2) 0)
      (llm-chat-prompt-append-response prompt ""))
    prompt))

(defun llm-buffer-chat-to-prompt (&optional centitemp)
  "Form an LLM prompt from the region or buffer by looking for
chat-like markers at the beginning of lines like \"user:\" and
\"assistant:\", and sending what follows as a conversation.  If
there is a marker like \"system:\" then what follows is sent as a
system prompt.

So an example chat buffer might look like this:

  This is an example buffer.  This text is not sent because it is
  before the first marker.

  system: Your name is Bob.
  user: What is your name?
  assistant: My name is Bob."
  (llm-buffer-markers-to-prompt "^\\(system\\|user\\|assistant\\):" nil centitemp))

(defun llm-buffer-markup-to-prompt (&optional centitemp)
  "Form an LLM prompt from the region or buffer by looking for
special markup, which can be within the comments of whatever
language is in the buffer, allowing for chats to be formed within
source code or structured documents.

Each part starts on the line after a marker like
\"@llm-start(ROLE)\" and ends on the line before a marker like
\"@llm-end\".  ROLE must be system, user, or assistant.  The
first system part is sent as a system prompt.

So an example buffer might look like this:

  This text will be be ignored.

  /* @llm-start(system) */
  Your name is Bob.
  /* @llm-end */

  This text will not be sent either.

  .. @llm-start(user)
  What is your name?"
  (llm-buffer-markers-to-prompt "@llm-start(\\(system\\|user\\|assistant\\)).*$"
                                "^.*@llm-end" centitemp))
    
(defun llm-buffer-waiting-text (prompt)
  "Compose waiting message text to insert into the buffer as a
placeholder while waiting the LLM to respond."
  ;; NOTE: Nothing in this text is necessary to the function of the
  ;; rest of the llm-buffer module, so it could be simplified.
  (let* ((part-count (length (llm-chat-prompt-interactions prompt)))
         (temperature (llm-chat-prompt-temperature prompt))
         (interactions (llm-chat-prompt-interactions prompt))
         (token-count
          (+
           (cl-reduce
            #'+
            (mapcar
             (lambda (interaction)
               (llm-count-tokens llm-buffer-provider
                                 (llm-chat-prompt-interaction-content interaction)))
             interactions))
           (llm-count-tokens llm-buffer-provider
                             (or (llm-chat-prompt-context prompt) ""))))
         (last (llm-chat-prompt-interaction-content (car (last interactions))))
         (empty-last (string= last ""))
         ;; TODO: Investigate how to reliably work out which model
         ;; we're talking to by testing with a multi-model server.
         (model-name (or (and (not (string= (llm-name llm-buffer-provider) "unset"))
                              (llm-name llm-buffer-provider))
                         (and (member 'model-list (llm-capabilities llm-buffer-provider))
                              (= (length (llm-models llm-buffer-provider)) 1)
                              (car (llm-models llm-buffer-provider)))
                         "LLM")))
    ;; TODO: Could include provider or model name rather than just "LLM".
    (format "[Sending approx %d tokens from %s%s parts%s.  Waiting for %s...]"
            token-count
            (if (llm-chat-prompt-context prompt)
                "system prompt and "
              "")
            (if empty-last
                (format "%d+1" (- part-count 1))
              part-count)
            (if temperature
                (format " at temperature %g" temperature)
              "")
            model-name)))

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
              (setq beg (point))
              (insert (substring text (length prefix))))
          (replace-region-contents
           beg end
           (lambda ()
             (concat llm-buffer-prefix text))))
        (add-text-properties beg end llm-buffer-partial-props)
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
  (let* ((prompt (funcall llm-buffer-to-prompt centitemp))

         ;; Remember where to insert the results
         (request-buffer (current-buffer))
         (end-marker (copy-marker (point) t))
         (beg-marker (copy-marker end-marker nil))

         ;; Set up a waiting message to be inserted while waiting for
         ;; a response from the LLM.
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

         ;; Set up callbacks to receive results from the LLM via
         ;; llm-chat-streaming.
         (partial-callback (llm-buffer-inserter request-buffer beg-marker end-marker))
         (finish-text
          (lambda ()
            ;; TODO: Could incorporate remove-waiting?
            (remove-text-properties beg-marker end-marker llm-buffer-partial-props)))
         ;; When the final result arrives, put it in the buffer and cancel the mode.
         (response-callback
          (lambda (text)
            (funcall partial-callback (concat text llm-buffer-postfix))
            (funcall finish-text)
            (with-current-buffer request-buffer
              (llm-request-mode 0)
              (setq llm-buffer-canceller nil))))
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

(provide 'llm-buffer)

;; End.
