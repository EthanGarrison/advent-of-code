(define csort (lambda (fn l) (sort l fn)))

(define file->list
  (lambda (file)
    (letrec ((recurse (lambda (acc f)
                        (cond ((eof-object? (peek-char f)) acc)
                              (else (recurse (cons (read-line f) acc) f))))))
      (recurse '() file))))

(define with-file-as-list
  (lambda (file fn)
    (call-with-input-file file (lambda (f) (fn (file->list f))))))

