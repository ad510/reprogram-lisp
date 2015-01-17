(set-dispatch-macro-character #\# #\?
  #'(lambda (stream subchar numarg)
    (read-delimited-list #\# stream)
))

(print #?list "hello" "world"#)
