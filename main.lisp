(defun transform (code)
  (cond
    ((not (consp code)) code)
    ((eq (car code) '/*) (transform (find-sym '*/ code)))
    ((eq (car code) '}) NIL)
    (t (destructuring-bind (a . b) (next code)
         (cons a (transform b))))))

(defun next (code)
  (cond
    ((eq (car code) '{) (cons (cons 'progn (transform (cdr code)))
                              (find-} (cdr code))))
    ((eq (second code) '=) (ifx 'setf code))
    ((eq (second code) '+) (ifx '+ code))
    ((eq (second code) '-) (ifx '- code))
    ((eq (second code) '*) (ifx '* code))
    ((eq (second code) '/) (ifx '/ code))
    ((consp (second code))
      (next (cons (cons (first code) (transform (second code)))
                  (cddr code))))
    (t code)))

(defun ifx (op code)
  (destructuring-bind (a . b) (next (cddr code))
    (next (cons (list op (car code) a) b))))

(defun find-} (code)
  (case (car code)
    ({ (find-} (cdr (find-} (cdr code)))))
    (} (cdr code))
    (otherwise (find-} (cdr code)))))

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
  console.log(10 + 1+(5) + 6);
}
console.log("c");
#
