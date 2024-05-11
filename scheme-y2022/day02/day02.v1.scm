

(define move-score
  (lambda (move)
    (case move
      ((#\A #\X) 1)
      ((#\B #\Y) 2)
      ((#\C #\Z) 3)
      (else 0))))

(define round-score
  (lambda (row)
    (let ((lmove (move-score (car row)))
	  (rmove (move-score (caddr row))))
      (cond ((eqv? lmove rmove) (+ 3 lmove))
	    ((or (and (eqv? lmove 3) (eqv? rmove 1)) (> rmove lmove)) (+ 6 rmove))
	    ((or (and (eqv? rmove 3) (eqv? lmove 1)) (> lmove rmove)) rmove)))))

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

