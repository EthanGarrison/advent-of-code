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
    (call-with-input-file file (lambda (f) (fn (reverse (file->list f)))))))

; Originally had this as fish, but that wasn't quite right
; This is closer, though pretty sure it doesn't exist in Haskell.
(define <$$>
  (lambda (fn list-of-list)
    (map (lambda (inner) (map fn inner)) list-of-list)))

(define id (lambda (i) i))

(define compose
  (lambda (f g)
    (lambda args
      (f (apply g args)))))

; Group l into chunks of size i
; Last elem is not guaranteed to be size i
; Not tail-recursive, should fix that
(define grouped
  (lambda (i l)
    (cond ((eq? i 0) '())          ; Empty group size, empty result
          ((eq? i 1) l)            ; Group of one, just return the input
          ((eqv? l '()) '())       ; Given list is empty, return empty
          ((< (length l) i) (list l))  ; If there are not enough elements in l, return l.  Only needed because drop is strict
          (else (cons (take l i) (grouped i (drop l i)))))))
