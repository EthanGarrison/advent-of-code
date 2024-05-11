; Offsets for converting the char to the problem-defined ints
(define lower-offset 96)
(define upper-offset 38)

; Have to use this for the test in the loop, so defined here
(define upper-set (char-set 'upper-case))

; Split the string in half.  Will do weird things on non-even lengths
(define split-string
  (lambda (str)
    (let* ((len (string-length str))
           (split-len (/ len 2)))
      (list (substring str 0 split-len) (substring str split-len len)))))

; Convert chars into the problem-defined priority int.  a-z -> 1-26, A-Z -> 27-52
(define char->priority
  (lambda (c)
    (let ((cdig (char->integer c)))
      (- cdig (if (char-in-set? c upper-set) upper-offset lower-offset)))))

; Convert back to char.  Really only needed for debugging
(define priority->char
  (lambda (p) (integer->char (+ p (if (< p 27) lower-offset upper-offset)))))

; Wrapper around the char->priority
(define string->priority
  (lambda (str) (map char->priority (string->list str))))

; Wrapper around lset-intersection
(define find-duplicate
  (lambda (l r) (car (lset-intersection eq? (string->list l) (string->list r)))))

(define find-duplicate-triple
  (lambda (l m r) (car (lset-intersection eq? (string->list l) (string->list r) (string->list m)))))

(define grouped
  (lambda (i l) (cond ((eq? i 0) '())
                      ((eq? i 1) '())
                      ((eqv? l '()) '())
                      (else (cons (take l i) (grouped i (drop l i)))))))

; For each input, split string and find the intersect.  Then, sum all the results
(define part1
  (lambda (l)
    (fold + 0 (map (lambda (str) (char->priority (apply find-duplicate (split-string str)))) l))))

(define part2
  (lambda (l)
    (fold + 0 (map (lambda (triple) (char->priority (apply find-duplicate-triple triple))) (grouped 3 l)))))

