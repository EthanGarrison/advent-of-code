(load "lib/utils.scm")

; Finite state machine, but monkeys this time.  Looks like we can think of this
; as each monkey is a processor, which consumes each item and generates new items
; for other monkeys.  Looking at the input, not bothering with a parser, just
; manually transcribing the inputs.  Input processing would be more of a headache
; than needed, given that they only give us 8 monkeys to work with.

(define (monkey-builder init-items inspect test true false)
  (list
    init-items
    (lambda (x)
      (let ((new (floor-quotient (inspect x) 3)))
        (list (if (eq? 0 (modulo new test)) true false) new)))
    0))

; No more need for the divide be 3 on every inspect
(define (monkey-builder-part2 init-items inspect test true false)
  (list
    init-items
    (lambda (x)
      (let ((new (inspect x)))
        (list (if (eq? 0 (modulo new test)) true false) new)))
    0))

; Oh boy, using eval to read a file with the custom inputs
; Any trick that keeps the inputs from being in the code I guess
; TBH, this stuff is what originally called me to Scheme/Lisp
; Found it interesting to generate data, write out to a file,
; then easily rehydrate later.  No fussing with serialization,
; though that isn't entirely the case with lambdas and such
(define (read-monkeys-from-file file builder)
  (let ((monkey-map (make-strong-eq-hash-table))
        (curr-env (nearest-repl/environment)))
    (let recurse ((info (read file)))
      (if (eof-object? info) monkey-map
        (let ((eval-info (eval info curr-env)))
          (hash-table-set!
            monkey-map
            (car eval-info)
            (apply builder (cdr eval-info)))
          (recurse (read file)))))))

(define (perform-round monkey-map)
  (let recurse ((monkeys (sort (hash-table-keys monkey-map) <)))
    (if (null? monkeys) '()
      (begin
        ; First, we update the "seen" value with the length of the held list
        (hash-table-update!
          monkey-map (car monkeys)
          (lambda (monkey)
            (list
              (first monkey)
              (second monkey)
              (+ (third monkey) (length (first monkey))))))
        ; Now we do the recursive check on each item
        (let* ((curr-monkey (hash-table-ref monkey-map (car monkeys)))
               (monkey-action (cadr curr-monkey)))
          (let run-monkey-action ((monkey-items (car curr-monkey)))
            (if (null? monkey-items)
              (hash-table-set! monkey-map (car monkeys) (cons '() (cdr curr-monkey)))
              (let ((result (monkey-action (car monkey-items))))
                (hash-table-update!
                  monkey-map
                  (car result)
                  (lambda (monkey) (cons (append (car monkey) (cdr result)) (cdr monkey))))
                (run-monkey-action (cdr monkey-items))))))
        (recurse (cdr monkeys))))))

(define (part1 file)
  (let ((input (call-with-input-file file (lambda (f) (read-monkeys-from-file f monkey-builder)))))
    (let recurse ((round 20))
      (if (eq? 0 round)
        (begin
          (hash-table-walk input (lambda (k v) (begin (display k) (display " : ") (display (car v)) (display " ") (display (caddr v)) (newline))))
          (apply * (take (sort (map caddr (hash-table-values input)) >) 2)))
        (begin
          (perform-round input)
          (recurse (- round 1)))))))

; Magic.  Trick according to internet was something about Chinese Remainder theory
; Someone mentioned if you just multiply all the test values and modulo the resulting
; inspection by that number, then it will work as well.  Guess that makes sense, as
; that keeps it from duplicating the required test values.  Funky math stuff.  I didn't
; add to the code directly, as it was based on the input, so values were manually added
; to the inspect function.  If I had wrote a parser for the input file, guess I could
; have handled it there.  Current parser goes through monkey-builder, which consumes
; the test value.  Guess I could update read-monkeys-from-file to do it, but that would
; be a headache, as I would then have to loop over the values again to add the modulo
; to the inspect after doing the parsing.  Overall, faster to manual add then fish it
; out of the inputs and update my code
(define (part2 file)
  (let ((input (call-with-input-file file (lambda (f) (read-monkeys-from-file f monkey-builder-part2)))))
    (let recurse ((round 10000))
      (if (eq? 0 round)
        (begin
          (hash-table-walk input (lambda (k v) (begin (display k) (display " : ") (display (car v)) (display " ") (display (caddr v)) (newline))))
          (apply * (take (sort (map caddr (hash-table-values input)) >) 2)))
        (begin
          (perform-round input)
          (recurse (- round 1)))))))

(part2 "day11/input2.txt")
