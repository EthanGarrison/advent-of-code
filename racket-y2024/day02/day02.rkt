#lang racket/base

(require
  racket/port
  racket/string
  racket/list)

(define (parse-row row)
  (let ((split-row (string-split row)))
    (map string->number split-row)))

(define (val-diff-safe prev curr desc?)
  (let ((diff (abs (- prev curr))))
    (and (>= diff 1) (<= diff 3) (if desc? (>= prev curr) (<= prev curr)))))

(define (check-if-safe row)
  (foldl
    (lambda (value acc)
      (let ((safe (first acc))
            (prev (second acc))
            (desc? (third acc)))
        (if (not safe)
          (list 'f '() '())
          (list
            (cond [(null? prev) 't]
                 [(val-diff-safe prev value (if (null? desc?) (< 0 (- prev value)) desc?)) 't]
                 [else 'f])
            value
            (cond [(null? prev) '()]
                  [(null? desc?) (< 0 (- prev value))]
                  [else desc?])))))
    (list 't '() '())
    row))

(define (part-one rows)
  (map (compose1 check-if-safe parse-row) rows))

(call-with-input-file 
  "day02/sample.txt"
  (lambda (f) (part-one (port->lines f))))

,bt
