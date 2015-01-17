(defun read-tok (stream)
  (let ((next (read-char stream)))
    (if (char= next #\Space)
      ""
      (concatenate 'string (make-string 1 :initial-element next) (read-tok stream)))))

(set-dispatch-macro-character #\# #\?
  #'(lambda (stream subchar numarg)
    ;(first (multiple-value-list (intern (read-tok stream))))
    (read-tok stream)
))

(print #?hello )
