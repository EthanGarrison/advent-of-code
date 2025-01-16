#lang racket/base

(require
  racket/port
  racket/string
  racket/list)

(define test-input
  (list
    "3   4"
    "4   3"
    "2   5"
    "1   3"
    "3   9"
    "3   3"))

(define (parse-input-list input-list)
  (let 
    ([unsorted-result (foldl
                        (lambda (row acc)
                          (let ([split-row (string-split row)])
                            (list
                              (cons (string->number (first split-row)) (first acc))
                              (cons (string->number (second split-row)) (second acc)))))
                        '(() ())
                        input-list)])
    (list (sort (first unsorted-result) <) (sort (second unsorted-result) <))))

(define (count-occurance input-list)
  (foldl
    (lambda (row acc)
      (hash-set acc row (+ 1 (hash-ref acc row 0))))
    (hasheq)
    input-list))

(define (sum l) (foldl + 0 l))

(define (part-one input-list)
  (let
    ([parsed-list (parse-input-list input-list)])
    (foldl
      (lambda (f s acc)
        (+ (abs (- f s)) acc))
      0
      (first parsed-list)
      (second parsed-list))))

(define (part-two input-list)
  (let*
    ([parse-list (parse-input-list input-list)]
     [lookup (count-occurance (second parse-list))])
    (sum (map
           (lambda (i) (* i (hash-ref lookup i 0)))
           (first parse-list)))))

; (call-with-input-file 
;   "day01.txt"
;   (lambda (f) (part-two (port->lines f))))

