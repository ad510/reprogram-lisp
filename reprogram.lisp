(defun transform (code)
  (cond
    ((not (consp code)) code)
    (t (destructuring-bind (a . b) (next code)
         (cons a (transform b))))))

(defun next (code)
  (cond
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
        (cons (list 'while (car (transform (second code))) a)
              b)))
    ((eq 'for (car code))
      (destructuring-bind (a . b) (next (cddr code))
        (cons (list 'for (second code) a)
              b)))
    ((or (eq 'var (car code)) (eq 'return (car code)))
      (next (cdr code)))
    ((eq '! (car code))
      (destructuring-bind (a . b) (next (cdr code))
        (cons (list 'not a) b)))
    ((eq '++ (second code)) (cons `(setf ,(first code) (1+ ,(first code)))
                                  (cddr code)))
    ((eq '-- (second code)) (cons `(setf ,(first code) (1- ,(first code)))
                                  (cddr code)))
    ((eq '= (second code)) (ifx 'setf code))
    ((eq '== (second code)) (ifx 'eql code))
    ((eq '!= (second code)) (ifx '!= code))
    ((eq '< (second code)) (ifx '< code))
    ((eq '> (second code)) (ifx '> code))
    ((eq '<= (second code)) (ifx '<= code))
    ((eq '>= (second code)) (ifx '>= code))
    ((eq '+ (second code)) (ifx 'js+ code))
    ((eq '- (second code)) (ifx '- code))
    ((eq '* (second code)) (ifx '* code))
    ((eq '/ (second code)) (ifx '/ code))
    ((eq '% (second code)) (ifx 'rem code))
    ((and (cdr code) (symbolp (first code)) (or (consp (second code)) (null (second code))))
      (cons (cons (first code) (transform (second code)))
            (cddr code)))
    (t code)))

(defun ifx (op code)
  (destructuring-bind (a . b) (next (cddr code))
    (next (cons (list op (car code) a) b))))

(defun tokenize (in out str-mode)
  (let ((ch (read-char in)))
    (when (char/= #\# ch)
      (if str-mode
        (write-char ch out)
        (case ch
          (#\/ (case (peek-char NIL in)
                 (#\/ (loop until (eql #\Newline (read-char in))))
                 (#\* (progn (loop until (and (eql #\* (read-char in)) (eql #\/ (peek-char NIL in))))
                             (read-char in)))
                 (otherwise (write-char ch out))))
          ((#\; #\,) (write-char #\Space out))
          (#\{ (write-string " progn(" out))
          (#\} (write-char #\) out))
          (otherwise (write-char ch out))))
      (tokenize in out (if (eql #\" ch) (not str-mode) str-mode)))))

(set-dispatch-macro-character #\# #\?
  #'(lambda (stream subchar numarg)
    (let ((out (make-string-output-stream)))
      (tokenize stream out NIL)
      (write-char #\# out)
      (transform (read-delimited-list #\# (make-string-input-stream (get-output-stream-string out)))))
))

(setf undefined NIL)
(setf false NIL)
(setf true t)

(defmacro != (x y)
  `(not (eql ,x ,y)))

(defmacro while (condi body)
  `(loop while ,condi do ,body))

(defmacro for (condi body)
  (destructuring-bind (a b c) (transform condi)
    `(progn ,a
            (while ,b (progn ,body ,c)))))

(defun js+ (x y)
  (if (or (stringp x) (stringp y))
    (concatenate 'string (if (stringp x) x (write-to-string x))
                         (if (stringp y) y (write-to-string y)))
    (+ x y)))

(defun console.log (msg)
  (print msg))

#? progn
/****************
Quirky JavaScript (i.e. 100% valid Lisp!)
****************/

console.log("Hello, world!");

function factorial(n) {
  if (n == 0) {
    return 1; //quirk: return only works correctly if it's the last statement executed in function
  } else {
    return n * factorial(n - 1); //quirk: all operations are right-associative and ignore precedence
  }
}

function testIf(val) {
  if (val) {
    console.log(val);
  } else if (val == false) {
    console.log("not true");
  }
}

function testWhile() {
  //quirk: all vars are globally scoped, so this overwrites any variable "a" defined elsewhere
  var a = 3;
  while (a > 0) {
    console.log("a = " + a);
    a = a - 1;
  }
}

function testFor(min, max) {
  for (var i = min; i < max; i ++) { //quirk: need space between i and ++
    console.log(i);
  }
}

console.log("5! is " + factorial(5));
testIf(false);
testIf(true);
testWhile();
testFor(0, 5);
#
