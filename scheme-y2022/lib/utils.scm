(define csort (lambda (fn l) (sort l fn)))

; Kind of hot.  Instead of letrec, can define recursive function with defaults
; So, recurse has `acc` and `f`, with `'()` and `file` as the initial values
(define file->list
  (lambda (file)
    (let recurse ((acc '())
                  (f file))
      (if (eof-object? (peek-char f))
        acc
        (recurse (cons (read-line f) acc) f)))))

(define with-file-as-list
  (lambda (file fn)
    (call-with-input-file file (lambda (f) (fn (file->list f))))))

; Super basic fish operator.  Definitely not accurate, but works enough for now
(define >=>
  (lambda (fn list-of-list)
    (map (lambda (inner) (map fn inner)) list-of-list)))

(define id (lambda (i) i))

(define compose
  (lambda (f g)
    (lambda args
      (f (apply g args)))))

