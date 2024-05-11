(define input-file "input.txt")

(define reduce
  (lambda (l i fn)
    (letrec ((loop (lambda (ll acc) 
		     (if (eqv? ll '()) acc 
		       (loop (cdr ll) (fn (car ll) acc))))))
      (if (eqv? l '()) i (loop (cdr l) (fn (car l) i))))))

(define find-max
  (lambda (l)
    (letrec ((loop (lambda (acc rest) 
		     (if (eqv? rest '()) acc
		       (loop (max acc (car rest)) (cdr rest))))))
      (loop 0 l))))

(define sum-num-run
  (lambda (acc file)
    (let* ((next (read-line file)))
      (if (or (eof-object? next) (eqv? (string-length next) 0)) acc
	      (sum-num-run (+ (string->number next) acc) file)))))

(define build-full-vector
  (lambda (file)
    (letrec ((consume (lambda (acc)
      (let ((eof-check (peek-char file)))
	(if (eof-object? eof-check) acc
          (consume (cons (sum-num-run 0 file) acc)))))))
      (consume '()))))

(begin
  (call-with-input-file input-file
    (lambda (i)
      (build-full-vector i))))


