(defun transform (code)
  (cond
    ((not (consp code)) code)
    ((eq (car code) '/*) (transform (find-sym '*/ code)))
    ((eq (car code) '{) (cons (cons 'progn (transform (cdr code)))
                              (transform (find-sym '} code))))
    ((eq (car code) '}) NIL)
    ((consp (second code))
      (cons (cons (first code) (transform (second code)))
            (transform (cddr code))))
    (t (cons (car code) (transform (cdr code))))))

(defun find-sym (sym code)
  (if (eq (car code) sym)
    (cdr code)
    (find-sym sym (cdr code))))

(set-dispatch-macro-character #\# #\?
  #'(lambda (stream subchar numarg)
    (transform (read-delimited-list #\# stream))
))

(defun console.log (msg)
  (print msg))

#? progn
console.log("Hello, world!");
{
  console.log("a");
  /* console.log("b") */
  console.log("d");
}
console.log("c");
#
