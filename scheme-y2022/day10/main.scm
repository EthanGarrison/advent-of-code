(load "lib/utils.scm")

; Another sudo-impl of an instruction set.  This time, we are keeping
; track of cycle time as well.  We only have a single register as well
; so will probably just write the interpreter to output the new cycle
; and value of the register.  The way the talk about it, each instruction
; takes an amount of clock cycles and you just increment up as you
; process each cycle.  I can already feel part two telling us that actually
; we don't do that but instead schedule and interleve the calls.  Could
; lead into tricky situations were we might increment out of order.
; Not going to overthink and just implement the basic set first

(define (parse-row row)
  (let ((split-row ((string-splitter) row)))
    (cond ((string=? "noop" (car split-row)) '(noop))
          ((string=? "addx" (car split-row)) `(addx ,(string->number (cadr split-row))))
          (else (error 'unsupported row)))))

(define (execute-instruction instr x-reg cycle)
  (let ((op (car instr)))
    (cond ((eq? 'noop op) `(,(list x-reg (+ 1 cycle))))
          ((eq? 'addx op) `(,(list (+ x-reg (cadr instr)) (+ 2 cycle)) ,(list x-reg (+ 1 cycle))))
          (else (error 'unsupported op)))))

(define (run-program rows)
  (fold
    (lambda (row prev-states)
      (let* ((instr (parse-row row))
             (prev-state (car prev-states))
             (prev-x-reg (car prev-state))
             (prev-cycle (cadr prev-state)))
        (append (execute-instruction instr prev-x-reg prev-cycle) prev-states)))
    '((1 1)) ; This starts the cycle at 1 instead of 0.  Seems to work, as the way I calculate state is off by one anyways I think
    rows))

(define (part1 rows)
  (fold
    (lambda (state acc) (+ (apply * state) acc))
    0
    (let ((sig-cycles '(20 60 100 140 180 220)))
      (filter
        (lambda (state) (find (lambda (sig) (eq? (cadr state) sig)) sig-cycles))
        (run-program rows)))))

(define (draw-screen rows)
  (let recurse ((curr-cycle 1)
                (rows-left rows))
    (if (eq? 241 curr-cycle)
      '()
      (begin
        (if (eq? 0 (modulo (- curr-cycle 1) 40)) (newline) '()) ; Print a newline on screen width
        ; Cheating a bit, but our rows have a value per cycle, so we don't
        ; need to sync our draw cycle to the program cycle
        (if (null? rows-left)
          (display ".")
          (let* ((curr-pixel (modulo (- curr-cycle 1) 40))
                 (curr-register (caar rows-left))
                 (sprite-lower (+ -1 curr-register))
                 (sprite-upper (+ 1 curr-register)))
            (display
              (if (and (<= curr-pixel sprite-upper) (>= curr-pixel sprite-lower))
                "#"
                "."))))
        (recurse (+ 1 curr-cycle) (cdr rows-left))))))

(define (part2 rows)
  (draw-screen (reverse (run-program rows))))

