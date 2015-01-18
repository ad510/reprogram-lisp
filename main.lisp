(defun transform (code)
  (cond
    ((not (consp code)) code)
    ((eq '/* (car code)) (transform (find-sym '*/ code)))
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

(defun find-sym (sym code)
  (if (eq (car code) sym)
    (cdr code)
    (find-sym sym (cdr code))))

(defun tokenize (in out str-mode)
  (let ((ch (read-char in)))
    (when (char/= #\# ch)
      (if str-mode
        (write-char ch out)
        (case ch
          (#\/ (case (peek-char NIL in)
                 (#\/ (loop until (eql #\Newline (read-char in))))
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

(defun console.log (msg)
  (print msg))

#? progn
function testIf(val) {
  if (val) {
    console.log(val);
    /* console.log("b"); */
    //console.log("c");
    console.log(10 + 1+(5) + 6);
  } else if (val == false) {
    console.log("not true");
  }
}

function testWhile() {
  a = 0;
  while (a < 3) {
    console.log(a);
    a = a + 1;
  }
}

function testFor(min, max) {
  for (i = min; i < max; i = i + 1) {
    console.log(i);
  }
}

console.log("Hello, world!");
testIf(false);
testIf(true);
testWhile();
testFor(0, 5);
#
