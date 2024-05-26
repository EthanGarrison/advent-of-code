
; First up, parse the inputs
; The picture seems fixed with, so we can work with that
; Had to loosen grouped up a bit, as was using drop, which seems to be strict
; For a language that seems to be loose on structure and info about data,
; they really seem to like enforcing strict checks everywhere.  Reminds me a bit
; of Rich Hickey's guardrails joke, though he was making fun of testing suites.
(define parse-chart-boxes
  (lambda (rows)
    (reverse
      (let recurse ((acc '())
                    (l rows))
        (if (null? l)
          acc
          (recurse (cons (grouped 4 (string->list (car l))) acc) (cdr l)))))))

; Copy-pasted from a previous day.  Probably should clean up and move to utils
; Will if I have to do it again
(define split-on-empty-line
  (lambda (file-list)
    (reverse
      (let recurse ((res '())
                    (acc '())
                    (l file-list))
        (cond ((null? l) (if (null? acc) res (cons (reverse acc) res)))
              ((equal? (car l) "") (recurse (cons (reverse acc) res) '() (cdr l)))
              (else (recurse res (cons (car l) acc) (cdr l))))))))

; Double map for the win again.  Basically, extract the value at every part, keeping nulls
(define condense-boxes
  (lambda (box-rows)
    (<$$> (lambda (r) (if (eq? (second r) #\space) '() (second r))) box-rows)))

; This is bad.  I should have come up with something that doesn't just thrash memory.
; Luckily, inputs here were small, so didn't really matter
(define rotate-matrix
  (lambda (m)
    (map (lambda (x) (remove null? x))
         (cond ((null? m) '())
               ((null? (car m)) '())
               (else (cons (map car m) (rotate-matrix (map cdr m))))))))

; I JUST WANT FUNCTION COMPOSITION AND ARG CURRYING
(define parse-chart
  (lambda (rows)
    (rotate-matrix (condense-boxes (parse-chart-boxes (drop-right rows 1))))))

; Too lazy for regex, just split pick out the parts you want
(define parse-move
  (lambda (row)
    (let ((split ((string-splitter) row)))
      (list (second split) (fourth split) (sixth split)))))

; Apply the parsers from above and return the result as a pair
; It is functions like this that work for these small sorts of scripts
; but if I did this in a large codebase, it would be horrible
; I think there are record types in this language, but times like this
; I just miss a good type system to tell me what this thing returns
(define parse-input-file
  (lambda (file)
    (let* ((split-file (split-on-empty-line file))
           (boxes (parse-chart (car split-file)))
           (rest (map parse-move (cadr split-file))))
      (list (list->vector boxes) (<$$> string->number rest)))))

; I finally broke and used mutable methods.  Shame Shame
; Not entirely sure how I would do this as-is without wasting a bunch of memory though
(define apply-move
  (lambda (move boxes)
    (let* ((source-idx (- (second move) 1))
           (dest-idx (- (third move) 1))
           (source-stack (vector-ref boxes source-idx))
           (dest-stack (vector-ref boxes dest-idx))
           (items (take source-stack (first move))))
      (begin
        (vector-set! boxes dest-idx (append (reverse items) dest-stack)) ; Items are moved one at a time, so rev
        (vector-set! boxes source-idx (drop source-stack (first move)))
        boxes))))

; Part two says we don't have to reverse, as it moves the items all at once
(define apply-move-part-2
  (lambda (move boxes)
    (let* ((source-idx (- (second move) 1))
           (dest-idx (- (third move) 1))
           (source-stack (vector-ref boxes source-idx))
           (dest-stack (vector-ref boxes dest-idx))
           (items (take source-stack (first move))))
      (begin
        (vector-set! boxes dest-idx (append items dest-stack))
        (vector-set! boxes source-idx (drop source-stack (first move)))
        boxes))))

(define part1
  (lambda (file)
    (let* ((parsed-file (parse-input-file file))
           (boxes (car parsed-file))
           (moves (cadr parsed-file)))
      (vector-map car (fold apply-move boxes moves)))))

(define part2
  (lambda (file)
    (let* ((parsed-file (parse-input-file file))
           (boxes (car parsed-file))
           (moves (cadr parsed-file)))
      (vector-map car (fold apply-move-part-2 boxes moves)))))

