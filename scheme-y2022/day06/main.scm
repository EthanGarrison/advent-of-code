; Pretty sure this was supposed to be the stress-test day
; Got lucky and the built-in `delete-duplicates` is efficient

(define find-stream-start
  (lambda (str test-len)
    (let ((str-length (string-length str)))
      (let recurse ((i test-len))
        (cond ((>= i str-length) i)
              ((eq? test-len (length (delete-duplicates (string->list (substring str (- i test-len) i))))) i)
              (else (recurse (+ i 1))))))))

(define part1 (lambda (input) (find-stream-start input 4)))

(define part2 (lambda (input) (find-stream-start input 14)))

