(define take
  (lambda (c l)
    (letrec ((recurse (lambda (cnt ll)
                        (cond ((eqv? cnt 0) '())
                              ((eq? ll '()) '())
                              (else (cons (car ll) (recurse (- cnt 1) (cdr ll))))))))
      (recurse c l))))

(define sum
  (lambda (x)
    (letrec ((recurse (lambda (acc l) 
                        (if (eq? l '()) acc
                          (recurse (+ acc (car l)) (cdr l))))))
      (recurse 0 x))))

(define group-inv-list
  (lambda (file-list)
    (letrec ((recurse (lambda (res acc l)
                        (cond ((eq? l '()) res)
                              ((equal? (car l) "") (recurse (cons acc res) '() (cdr l)))
                              (else (recurse res (cons (car l) acc) (cdr l)))))))
      (recurse '() '() file-list))))



(define file->list
  (lambda (file)
    (letrec ((recurse (lambda (acc f)
                        (cond ((eof-object? (peek-char f)) acc)
                              (else (recurse (cons (read-line f) acc) f))))))
      (recurse '() file))))



(define with-file-as-list
  (lambda (file fn)
    (call-with-input-file file
      (lambda (f) (fn (file->list f))))))

(define string->number?
  (lambda (i) (if (equal? i "") "" (string->number i))))

(define part1
  (lambda (l) (car (csort > (map sum (group-inv-list (map string->number? l)))))))

(define part2
  (lambda (l) (sum (take 3 (csort > (map sum (group-inv-list (map string->number? l))))))))

