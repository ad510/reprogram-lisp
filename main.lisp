(defun read-tok (stream)
  (let ((next (read-char stream)))
    (if (char= next #\Space)
      ""
      (concatenate 'string (make-string 1 :initial-element next) (read-tok stream)))))

(defun tokenize (stream)
  (let ((tok (read-tok stream)))
    (cons tok (if (string/= tok "END") (tokenize stream) NIL))))

(set-dispatch-macro-character #\# #\?
  #'(lambda (stream subchar numarg)
    ;(first (multiple-value-list (intern (read-tok stream))))
    (tokenize stream)
))

(print '#?hello world END )
