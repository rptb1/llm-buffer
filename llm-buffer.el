;;; llm-buffer -- a tool for feeding structured buffers to an LLM -*- lexical-binding: t -*-
;;;
;;; Much simpler than ellama.  Focussed on creative single-buffer stuff.
;;;
;;; The basic idea is to have a conversation with an LLM that is
;;; derived from a single buffer.  The buffer is processed into a
;;; prompt, the prompt is sent, and the reply from the LLM is streamed
;;; into the buffer.
;;;
;;; Various formats of buffer are supported, and markup of the
;;; conversation can be done in comments, such as code comments or the
;;; comments of a reStructuredText document.  You can override the
;;; format of the buffer and various other things in file local
;;; variables.  The idea is that you can save your conversation in a
;;; file and resume it later.
;;;
;;; You can edit the LLM's output while it's streaming into the
;;; buffer.  You can cancel the LLM using quit (usually C-g) or by
;;; deleting the LLM's output.
;;;
;;; Recommended in your Emacs init file::
;;;
;;;   (require 'llm-buffer)
;;;   (define-key global-map (kbd "C-c e") 'llm-buffer)
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

(defgroup llm-buffer nil
  "Tool for talking to LLMs from buffers."
  :group 'tools)

;; On Tails, requieres plz-curl-default-args has "--proxy ''" so that
;; the LLM module can talk to localhost.

(defcustom llm-buffer-provider
  (make-llm-openai-compatible :url "http://127.0.0.1:8080")
  "Backend LLM provider.

The default value is intended for llama.cpp's llama-server
running on the local host machine, e.g.

  ./build/bin/llama-server -m models/Meta-Lllama.gguf"
  :type '(sexp :validate llm-standard-provider-p)
  :local t)

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

(defcustom llm-buffer-params nil
  "An alist of extra JSON parameters to pass to the LLM.

Can be overriden in the buffer to adjust LLM parameters, for
example:

  ((:seed . 1234)
   (:max_tokens . 100)
   (:mirostat_tau . 7.5))

This value is passed to llm-make-chat-prompt as the
NON-STANDARD-PARAMS."
  :type '(alist :key-type symbol)
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

(defcustom llm-buffer-keys nil
  "A plist of placeholder values to be substituted in the prompt.

If non-nil, llm-prompt-fill-text is applied to the text from the
buffer with this list of keys in order to substitute placeholders
with the value of keys from this list.

For example, if \"Hello {{name}}.\" appears in the text, and
llm-buffer-keys is defined as (:name \"John\") then the text sent
to the LLM will say \"Hello John.\"

See the documentation for llm-prompt-fill-text for details."
  :type '(plist :value-type string)
  :local t)

(defgroup llm-buffer-faces nil "Faces used in llm-buffer."
  :group 'faces
  :group 'llm-buffer)

(defface llm-buffer-waiting '((t :inherit warning))
  "Face used for temporary waiting message."
  :group 'llm-buffer-faces)

(defface llm-buffer-partial '((t :inherit success))
  "Face used for partially inserted LLM response."
  :group 'llm-buffer-faces)

(defface llm-buffer-error '((t :inherit error))
  "Face used for error messages from the LLM."
  :group 'llm-buffer-faces)

(defvar-local llm-buffer-overlay nil
  "When non-nil there is an LLM request running in the buffer that
will insert text into this overlay.")

(defun llm-buffer-cancel ()
  "Cancel the LLM request that's inserting into the buffer."
  (when llm-buffer-overlay
    ;; Stop the LLM.
    (llm-cancel-request (overlay-get llm-buffer-overlay 'request))
    ;; Remove the waiting text, but not any other text.  This allows
    ;; for easy cancellation before the LLM starts writing, but
    ;; preserves any writing or edits.
    (funcall (overlay-get llm-buffer-overlay 'remove-waiting))
    (llm-request-mode 0)
    (delete-overlay llm-buffer-overlay)
    (setq llm-buffer-overlay nil)))

;; TODO: I inherited this overloading of quit from ellama and I'm not
;; sure I like it.  It's a bit too easy to stop the LLM.  Maybe have
;; llm-buffer toggle instead?
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
    (remove-hook 'kill-buffer-hook 'llm-buffer-cancel t)))

;; This is the heart of the module.
(defun llm-buffer-alist-to-prompt (parts &optional centitemp)
  "Form an LLM prompt from an alist of roles and text.

e.g. ((system . \"Your name is Bob\")
      (user . \"What is your name?\")
      (assistant . \"My name is Bob.\")
      (user . \"Do you have a surname?\"))

If the alist contains more than one system element, they are
concatenated, separated by a newline, to form the system prompt.

The roles may be nil, in which case roles are guessed as follows:

- If there is only one element a simple user prompt is formed.

- Otherwise, the first element is used as the system prompt, with
  user and assistant prompts following."
  (let* ((temperature (or (when centitemp (/ centitemp 100.0))
                          llm-buffer-temperature))
         (prompt (make-llm-chat-prompt
                  :temperature temperature
                  :non-standard-params llm-buffer-params)))
    ;; Process the parts into a prompt
    ;; Just one part without a role?  A simple user prompt.
    (if (and (= (length parts) 1) (not (caar parts)))
        (llm-chat-prompt-append-response prompt (cdar parts))
      (let ((last-role nil))
        (while parts
          (let ((role (or (caar parts)
                          (pcase last-role
                            ;; No role on first part?  Make it the system prompt.
                            ('nil 'system)
                            ;; Alternate user and assistant roles.
                            ('user 'assistant)
                            (_ 'user))))
                (text (cdar parts)))
            (when llm-buffer-keys
              (setq text
                    (apply #'llm-prompt-fill-text
                           text
                           llm-buffer-provider
                           llm-buffer-keys)))
            (if (eq role 'system)
                ;; TODO: Append to system prompt rather than ignoring?
                (setf (llm-chat-prompt-context prompt)
                      (if (llm-chat-prompt-context prompt)
                          (concat (llm-chat-prompt-context prompt) "\n" text)
                        text))
              (llm-chat-prompt-append-response prompt text role))
            (setq last-role role)
            (setq parts (cdr parts))))))
    prompt))

(defun llm-buffer-split (&optional centitemp)
  "Form an LLM prompt from the region or buffer by splitting the
buffer at the regular expression in llm-buffer-separator.

The first part is sent as a system prompt.  The rest are sent as
a chat conversation, between the user and the LLM as assistant.
If necessary, an empty user prompt is appended.

If there are no separators, the whole buffer is sent."
  (let* ((text
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (buffer-substring-no-properties (point-min) (point-max))))
         ;; Split the text into parts with the separator
         (parts
          (mapcar
           (lambda (string)
             (cons nil
                   (string-trim
                    (if llm-buffer-comment
                        (replace-regexp-in-string llm-buffer-comment " " string)
                      string))))
           (split-string text llm-buffer-separator))))
    (llm-buffer-alist-to-prompt parts centitemp)))

(defvar-local llm-buffer-to-prompt #'llm-buffer-split
  "Form an llm-chat-prompt from the region or buffer.

This function may be overriden per buffer, so that buffers can
use different kinds of markup.")

(defun llm-buffer-markers-to-prompt (start-regexp &optional end-regexp centitemp)
  "Form an LLM prompt from the region or buffer by looking for
parts between start-regexp and end-regexp.

If the end-regexp is nil, parts continue until the next
start-regexp, or the end of the text.

The start-regexp may return a match 1 of either \"system\",
\"user\" or \"assistant\" to indicate the role of the part.  But
if it does not, and there is more than one part, the first part
is used as the system prompt, followed by alternating user then
assistant parts.

Multiple \"system\" parts are concatenated with newlines to form
a system prompt.

If llm-buffer-comment is not nil, any occurences within the parts
are replaced by a single space."
  (let* ((start (if (use-region-p) (region-beginning) (point-min)))
         (end (if (use-region-p) (region-end) (point-max)))
         (parts nil))
    ;; Find the parts
    (save-excursion
      (goto-char start)
      (while (re-search-forward start-regexp end t)
        (let ((role (when (match-string 1) (intern (match-string 1))))
              (text-start (match-end 0)))
          (if (re-search-forward (or end-regexp start-regexp) end t)
              (goto-char (match-beginning 0))
            (goto-char end))
          (let* ((text (buffer-substring-no-properties text-start (point)))
                 (text (if llm-buffer-comment
                           (replace-regexp-in-string llm-buffer-comment " " text)
                         text))
                 (text (string-trim text)))
            (setq parts (cons (cons role text) parts))))))
    (setq parts (nreverse parts))
    (llm-buffer-alist-to-prompt parts centitemp)))

(defun llm-buffer-comment-chat-to-prompt (&optional centitemp)
  "Form an LLM prompt from the region or buffer by looking for
char-like markers like \"user:\", \"assistant:\" or \"system:\"
embedded in comments, and sending what follows as a conversation.

So an example buffer might look like this:

  /* This part isn't sent. */
  /* system: */
  Your name is Bob.
  /* user: */
  What is your name?
  /* assistant: */
  My name is Bob."
  (llm-buffer-markers-to-prompt
   (concat "^"
           (regexp-quote comment-start)
           ".*\\<\\(system\\|user\\|assistant\\):.*$")
   nil
   centitemp))

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
         (token-count
          (cl-loop
           for i in (llm-chat-prompt-interactions prompt)
           sum (llm-count-tokens llm-buffer-provider
                                 (llm-chat-prompt-interaction-content i))))
         (context (llm-chat-prompt-context prompt))
         (token-count
          (if context
              (+ token-count
                 (llm-count-tokens llm-buffer-provider context))
            token-count))
         ;; TODO: Investigate how to reliably work out which model
         ;; we're talking to by testing with a multi-model server.
         (model-name (or (and (not (string= (llm-name llm-buffer-provider) "unset"))
                              (llm-name llm-buffer-provider))
                         (and (member 'model-list (llm-capabilities llm-buffer-provider))
                              (= (length (llm-models llm-buffer-provider)) 1)
                              (car (llm-models llm-buffer-provider)))
                         "LLM")))
    ;; TODO: Could include provider or model name rather than just "LLM".
    (format "[Sending approx %d tokens from %s%d parts%s.  Waiting for %s...]"
            token-count
            (if (llm-chat-prompt-context prompt)
                "system prompt and "
              "")
            part-count
            (if temperature
                (format " at temperature %g" temperature)
              "")
            model-name)))

(defun llm-buffer-inserter (overlay)
  "Make an insertion callback for llm-chat-streaming that appends
the LLM output to a region.  The first call to the callback
replaces the entire region, including the waiting message, but
subsequent calls only append to the region, so that it can be
edited by the user while the LLM is still generating."
  (let ((prefix ""))
    (lambda (text)
      (let ((start (overlay-start overlay))
            (end (overlay-end overlay)))
        (with-current-buffer (overlay-buffer overlay)
          ;; TODO: prefix should always be a prefix
          (if (and (not (string-empty-p prefix))
                   (string-prefix-p prefix text))
              (save-excursion
                (goto-char end)
                (setq start (point))
                (insert (substring text (length prefix))))
            (replace-region-contents
             start end
             (lambda ()
               (concat llm-buffer-prefix text))))
          (overlay-put overlay 'face 'llm-buffer-partial)
          (setq prefix text))))))

(defun llm-buffer (&optional centitemp)
  "Send the region or buffer to the LLM, scheduling the response to arrive at the point.

If the buffer contains lines like \"---\" then the first part is
sent as the system prompt, and the remaining parts are sent as a
chat conversation.

A prefix argument may be used to specify the LLM temperature for
the request in hundredths, e.g. a prefix argument of 75 is a
temperature of 0.75."
  (interactive "P")
  (llm-buffer-cancel)
  (let* ((prompt (funcall llm-buffer-to-prompt centitemp))
         ;; Use an overlay to remember where to insert the results and
         ;; to highlight them to the user.
         (overlay (make-overlay (point) (point) nil nil t))
         ;; Create a waiting message to be inserted while waiting for
         ;; a response from the LLM.
         (waiting-text (llm-buffer-waiting-text prompt))
         ;; A function to remove the waiting text if it's the only
         ;; thing in the output region, e.g. if the request is
         ;; cancelled before the LLM replies.
         (remove-waiting
          (lambda ()
            (with-current-buffer (overlay-buffer overlay)
              (let* ((start (overlay-start overlay))
                     (end (overlay-end overlay))
                     (text (buffer-substring-no-properties start end)))
                (when (string= text waiting-text)
                  (delete-region start end))))))
         ;; Make a callback to receive results from the LLM via
         ;; llm-chat-streaming.
         (partial-callback (llm-buffer-inserter overlay))
         ;; When the final result arrives, put it in the output region
         ;; and tidy up.
         (response-callback
          (lambda (text)
            (funcall partial-callback (concat text llm-buffer-postfix))
            (with-current-buffer (overlay-buffer overlay)
              (llm-request-mode 0)
              (delete-overlay overlay)
              (setq llm-buffer-overlay nil))))
         ;; If an error occurs, put the message in the output region
         ;; and tidy up.
         (error-callback
          (lambda (_ msg)
            (funcall remove-waiting)
            (with-current-buffer (overlay-buffer overlay)
              (llm-request-mode 0)
              (save-excursion
                (goto-char (overlay-end overlay))
                (insert (concat "[" msg "]"))
                (overlay-put overlay 'face 'llm-buffer-error)
                ;; Ensure the overlay is deleted when the user deletes
                ;; the error message.
                (overlay-put overlay 'evaporate t)))
            ;; Also signal the error clearly to Emacs.
            (error msg))))

    ;; Insert the waiting text
    (replace-region-contents (overlay-start overlay)
                             (overlay-end overlay)
                             (lambda () waiting-text))
    (overlay-put overlay 'face 'llm-buffer-waiting)
    (overlay-put overlay 'remove-waiting remove-waiting)

    ;; Send the request to the LLM.
    (overlay-put
     overlay 'request
     (llm-chat-streaming llm-buffer-provider prompt
                         partial-callback
                         response-callback
                         error-callback))
    
    ;; Cancel the LLM request if the user deletes the whole output
    ;; region.
    (overlay-put
     overlay 'modification-hooks
     (list
      (lambda (overlay after start end &optional length)
        (when (= (overlay-start overlay) (overlay-end overlay))
          (llm-buffer-cancel)))))

    ;; Let the user see the request is in progress and cancel it.
    (setq llm-buffer-overlay overlay)
    (llm-request-mode 1)))

(provide 'llm-buffer)

;; End.
