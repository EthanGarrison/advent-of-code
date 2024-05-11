(define file->list
  (lambda (file)
    (letrec ((recurse (lambda (acc f)
                        (cond ((eof-object? (peek-char f)) acc)
                              (else (recurse (cons (read-line f) acc) f))))))
      (recurse '() file))))

(define with-file-as-list
  (lambda (file fn)
    (call-with-input-file file (lambda (f) (fn (file->list f))))))

(define map
  (lambda (fn d)
    (letrec ((recurse (lambda (acc x)
			(cond ((eq? x '()) acc)
			      (else (recurse (cons (fn (car x)) acc) (cdr x)))))))
      (recurse '() d))))

(define sum
  (lambda (d)
    (letrec ((recurse (lambda (acc x)
			(cond ((eq? x '()) acc)
			      (else (recurse (+ (car x) acc) (cdr x)))))))
      (recurse 0 d))))

(define move-score
  (lambda (move)
    (case move
      ((#\A #\X) 1)
      ((#\B #\Y) 2)
      ((#\C #\Z) 3))))

(define get-win-move
  (lambda (move)
    (case move
      ((#\A) 2)
      ((#\B) 3)
      ((#\C) 1))))

(define get-loss-move
  (lambda (move)
    (case move
      ((#\A) 3)
      ((#\B) 1)
      ((#\C) 2))))

(define row-score
  (lambda (row)
    (let* ((row-list (string->list row))
	   (lmove (move-score (car row-list)))
	   (rmove (move-score (caddr row-list))))
      (+ rmove 
	 (cond ((and (eqv? 3 lmove) (eqv? 1 rmove)) 6)
	       ((and (eqv? 3 rmove) (eqv? 1 lmove)) 0)
	       ((> lmove rmove) 0)
	       ((< lmove rmove) 6)
	       (else 3))))))

(define row-score-part2
  (lambda (row)
    (let* ((row-list (string->list row))
	   (lmove (car row-list))
	   (rmove (move-score (caddr row-list))))
      (case rmove
	((1) (get-loss-move lmove))
	((2) (+ 3 (move-score lmove)))
	((3) (+ 6 (get-win-move lmove)))))))

(define part1
  (lambda (file)
    (with-file-as-list file (lambda (rows) (sum (map row-score rows))))))

(define part2
  (lambda (file)
    (with-file-as-list file (lambda (rows) (sum (map row-score-part2 rows))))))

