(define (csort fn l) (sort l fn))

; Kind of hot.  Instead of letrec, can define recursive function with defaults
; So, recurse has `acc` and `f`, with `'()` and `file` as the initial values
(define (file->list file)
  (let recurse ((acc '())
                (f file))
    (if (eof-object? (peek-char f))
      acc
      (recurse (cons (read-line f) acc) f))))

(define (with-file-as-list file fn)
  (call-with-input-file file (lambda (f) (fn (reverse (file->list f))))))

; Originally had this as fish, but that wasn't quite right
; This is closer, though pretty sure it doesn't exist in Haskell.
(define (<$$> fn list-of-list)
  (map (lambda (inner) (map fn inner)) list-of-list))

(define (id i) i)

(define (compose f g)
  (lambda args (f (apply g args))))

; Group l into chunks of size i
; Last elem is not guaranteed to be size i
; Not tail-recursive, should fix that
(define (grouped i l)
  (cond ((eq? i 0) '())          ; Empty group size, empty result
        ((eq? i 1) l)            ; Group of one, just return the input
        ((eqv? l '()) '())       ; Given list is empty, return empty
        ((< (length l) i) (list l))  ; If there are not enough elements in l, return l.  Only needed because drop is strict
        (else (cons (take l i) (grouped i (drop l i))))))

(define (window i l)
  (cond ((eq? i 0) '())
        ((eq? i 1) l)
        ((null? l) '())
        ((< (length l) i) '())
        (else (cons (take l i) (window i (drop l 1))))))

