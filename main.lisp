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
    ((eq 'function (car code))
      (destructuring-bind (a . b) (next (cdddr code))
        (cons (list 'defun (second code) (third code) a)
              b)))
    ((eq 'if (car code))
      (destructuring-bind (a . b) (next (cddr code))
        (destructuring-bind (c . d) (if (eq 'else (car b)) (next (cdr b)) (cons NIL b))
          (cons (list 'if (car (transform (second code))) a c)
                d))))
    ((eq 'while (car code))
      (destructuring-bind (a . b) (next (cddr code))
        (cons `(while ',(car (transform (second code))) ',a)
              b)))
    ((eq '! (car code))
      (destructuring-bind (a . b) (next (cdr code))
        (cons (list 'not a) b)))
    ((eq '= (second code)) (ifx 'setf code))
    ((eq '== (second code)) (ifx 'eql code))
    ((eq '!= (second code)) (ifx '!= code))
    ((eq '< (second code)) (ifx '< code))
    ((eq '> (second code)) (ifx '> code))
    ((eq '<= (second code)) (ifx '<= code))
    ((eq '>= (second code)) (ifx '>= code))
    ((eq '+ (second code)) (ifx '+ code))
    ((eq '- (second code)) (ifx '- code))
    ((eq '* (second code)) (ifx '* code))
    ((eq '/ (second code)) (ifx '/ code))
    ((and (cdr code) (symbolp (first code)) (or (consp (second code)) (null (second code))))
      (cons (cons (first code) (transform (second code)))
            (cddr code)))
    (t code)))

(defun ifx (op code)
  (destructuring-bind (a . b) (next (cddr code))
    (next (cons (list op (car code) a) b))))

(defun find-} (code)
  (case (car code)
    ({ (find-} (find-} (cdr code))))
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

(defmacro != (x y)
  `(not (eql ,x ,y)))

(defun while (condi body)
  (when (eval condi)
    (eval body)
    (while condi body)))

(defun console.log (msg)
  (print msg))

#? progn
function testIf(val) {
  if (val) {
    console.log(val);
    /* console.log("b") */
    console.log(10 + 1+(5) + 6);
  } else if (val == false) {
    console.log("not true");
  }
}

function testWhile() {
  a = 0;
  while (a < 5) {
    console.log(a);
    a = a + 1;
  }
}

console.log("Hello, world!");
testIf(false);
testIf(true);
testWhile();
#
