(defun transform (code)
  (if (consp code)
    (if (consp (second code))
      (cons (cons (first code) (transform (second code)))
            (transform (cddr code)))
      (cons (first code) (transform (cdr code))))
    code))

(set-dispatch-macro-character #\# #\?
  #'(lambda (stream subchar numarg)
    (transform (read-delimited-list #\# stream))
))

(defun console.log (msg)
  (print msg))

#? progn
console.log("Hello, world!");
#
