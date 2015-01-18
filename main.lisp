(defun transform (code)
  (cond
    ((not (consp code)) code)
    ((eq '/* (car code)) (transform (find-sym '*/ code)))
    ((eq '} (car code)) NIL)
    (t (destructuring-bind (a . b) (next code)
         (cons a (transform b))))))

(defun next (code)
  (cond
    ((eq '{ (car code)) (cons (cons 'progn (transform (cdr code)))
                              (find-} (cdr code))))
    ((eq 'if (car code))
      (destructuring-bind (a . b) (next (cddr code))
        (cons (list 'if (car (transform (second code))) a NIL)
              b)))
    ((eq 'while (car code))
      (destructuring-bind (a . b) (next (cddr code))
        (cons `(while ',(car (transform (second code))) ',a)
              b)))
    ((eq '= (second code)) (ifx 'setf code))
    ((eq '+ (second code)) (ifx '+ code))
    ((eq '- (second code)) (ifx '- code))
    ((eq '* (second code)) (ifx '* code))
    ((eq '/ (second code)) (ifx '/ code))
    ((and (symbolp (first code)) (consp (second code)))
      (cons (cons (first code) (transform (second code)))
            (cddr code)))
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

(setf undefined NIL)
(setf false NIL)
(setf true t)

(defun while (condi body)
  (when (eval condi)
    (eval body)
    (while condi body)))

(defun console.log (msg)
  (print msg))

#? progn
console.log("Hello, world!");
a = true;
if (a) {
  console.log(a);
  /* console.log("b") */
  console.log(10 + 1+(5) + 6);
}
console.log("c");
a = 0;
while ((< a 5)) {
  console.log(a);
  a = a + 1;
}
#
