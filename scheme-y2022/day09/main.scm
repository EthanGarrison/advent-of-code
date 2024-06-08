(load "lib/utils.scm")

; Another state machine problem.  Looks like tricky part will be figuring
; out when the tail should move diagonal.  Probably going to do a rule like
; If both directions are non-zero and one is bigger than 1, then diagonal
; Not sure I can rely on distance directly, but that might be easier
; End result relies on keeping track of walked positions, but unlike previous
; day, we are random accessing, so looks like I am finally going to need
; hash tables.  I've been waiting to see how long I can go without
; using one.

(define (parse-row row)
  (let ((tokens ((string-splitter) row)))
    (list (string-ref (car tokens) 0) (string->number (cadr tokens)))))

; Second time thinking this, but could I implement some sort of pattern
; matching?  Just kind of annoying to do extracts all the time
; Flooring the result of the distance, as we say that diagonals are equi-distant
; to lateral moves
; We can now say that we need to move tail if distance is gt 1
(define (distance head-pos tail-pos)
  (cond
    ((not-pair? head-pos) (error "Head given was not a pair" head-pos))
    ((not-pair? tail-pos) (error "Tail given was not a pair" tail-pos))
    (else
      (let ((head-x (car head-pos))
            (head-y (cadr head-pos))
            (tail-x (car tail-pos))
            (tail-y (cadr tail-pos)))
        (floor->exact (sqrt (+ (expt (- tail-x head-x) 2) (expt (- tail-y head-y) 2))))))))

; Checks to figure out how the tail is supposed to move.  This will move more
; than a single space if necessary.  Could have done the single step, but didn't
; seem to matter and the logic worked easier this way
(define (move-tail head-pos tail-pos)
  (cond
    ((not-pair? head-pos) (error "Head given was not a pair" head-pos))
    ((not-pair? tail-pos) (error "Tail given was not a pair" tail-pos))
    (else
      (let ((dist (distance head-pos tail-pos))
            (head-x (car head-pos))
            (head-y (cadr head-pos))
            (tail-x (car tail-pos))
            (tail-y (cadr tail-pos)))
        (cond ((<= dist 1) tail-pos)                                  ; Distance of 1 means they are touching
              ((equal? head-pos tail-pos) tail-pos)                   ; If they overlap, no movement
              ((not (or (eq? head-x tail-x) (eq? head-y tail-y)))     ; Diagonals get priority, so have to put this first
               (list
                 (+ tail-x (if (> head-x tail-x) 1 -1))
                 (+ tail-y (if (> head-y tail-y) 1 -1))))
              ((eq? head-x tail-x)                                    ; If we are in the same row.
               (list tail-x (+ tail-y (if (> head-y tail-y) 1 -1))))  ; Move tail to be beside head
              ((eq? head-y tail-y)                                    ; If we are in the same col
               (list (+ tail-x (if (> head-x tail-x) 1 -1)) tail-y)))))))

; Not going to immediately tie head movement with tail movement
; Also, this is only moving a single step at a time.  Probably should setup
; tail movement for incremental as well, but meh.  If it bites me, I'll fix then
(define (move-head head-pos movement)
  (cond
    ((not-pair? head-pos) (error "Head given was not a pair" head-pos))
    (else
      (let ((head-x (car head-pos))
            (head-y (cadr head-pos)))
        (cond
          ((eq? movement #\R) (list (+ 1 head-x) head-y))
          ((eq? movement #\L) (list (+ -1 head-x) head-y))
          ((eq? movement #\U) (list head-x (+ 1 head-y)))
          ((eq? movement #\D) (list head-x (+ -1 head-y)))
          (else (error "Unsupported direction" movement)))))))

(define (execute-instruction-multi instr rope seen)
  (cond
    ((not-pair? instr) (error "Instruction set given was not a pair" instr))
    ((not (hash-table? seen)) (error "Seen values given was not an hash table" seen))
    (else
      (let ((movement (car instr)))
        (let recurse ((cnt (cadr instr))
                      (curr-rope rope))
          (if (eq? 0 cnt)
            curr-rope
            (let* ((next-head (move-head (car curr-rope) movement))
                   (next-tails (fold (lambda (t prev) (cons (move-tail (car prev) t) prev)) `(,next-head) (cdr curr-rope))))
              (begin
                (hash-table-update!/default seen (car next-tails) (lambda (v) (+ 1 v)) 0)
                (recurse (- cnt 1) (reverse next-tails))))))))))

(define (execute-instruction instr head tail seen)
  (cond
    ((not-pair? head) (error "Head given was not a pair" head))
    ((not-pair? tail) (error "Tail given was not a pair" tail))
    ((not-pair? instr) (error "Instruction set given was not a pair" instr))
    ((not (hash-table? seen)) (error "Seen values given was not an hash table" seen))
    (else
      (let ((movement (car instr)))
        (let recurse ((cnt (cadr instr))
                      (curr-head head)
                      (curr-tail tail))
          (if (eq? 0 cnt)
            (list curr-head curr-tail)
            (let* ((next-head (move-head curr-head movement))
                   (next-tail (move-tail next-head curr-tail)))
              (begin
                (hash-table-update!/default seen next-tail (lambda (v) (+ 1 v)) 0) ; Mutation, UCK!  Not sure if better to do the calculate now, or store then reduce
                (recurse (- cnt 1) next-head next-tail)))))))))

(define (part1 rows)
  (let ((intrs (map parse-row rows))
        (seen (make-equal-hash-table)))
    (cons seen (fold
      (lambda (instr acc)
        (let ((head (car acc))
              (tail (cadr acc)))
          (execute-instruction instr head tail seen)))
      '((0 0) (0 0))
      intrs))))

; Welp, part two is annoying.  Luckily, all the pieces are here, just have to
; change up things a bit. Probably will need to adjust execute-instruction?
; It does all the steps at once, but we would need to apply each step to each
; part of the chain.  Luckily, we only have to keep up with the tail, so
; speed-wise, this should still be fast.  Might even be able to drop the
; hash table, as I only really used it to keep count of times visited.
; Another situation of my bad habit to try and predict what the part two
; problem will be.
(define (part2 rows)
  (let ((intrs (map parse-row rows))
        (seen (make-equal-hash-table)))
    (cons
      seen
      (fold
        (lambda (instr rope) (execute-instruction-multi instr rope seen))
        '((0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0))
        intrs))))

(define result (with-file-as-list "day09/input.txt" part2))
(hash-table-size (first result))

