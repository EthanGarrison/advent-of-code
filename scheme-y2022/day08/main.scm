(load "lib/utils.scm")

; Looks like for this one, we can't just work row by row, will need the full file
; Or, wait, maybe we can.  What if we do a sneaky thing?  We can keep the largest
; we have seen for each column.  Then for the E/W checks, we are just counting
; trees until we find one that was bigger than the previous.
; Oh, but that doesn't help the south check.  Can we do some backwards thinking?
; Nope, thought about it, got too complicated, sticking to simple solution.
; When parsing the file, will return each value as a pair of the number and a bool
; We can then do the E/W check, toggle the bool if seen, then at the end, we rotate
; the entire thing and do it again.  Means double walking each row, then a rotate,
; then another double walk.  Will be slow, but not sure of a better way.
; Trying to keep track of the reverse direction was getting complicated
; Oh, guess there will be a final walk as well to count all the trues.
; Yeah, this is going to be slow

(define (parse-input-row row)
  (map (lambda (c) (cons (digit-value c) #f)) (string->list row)))

; Originally considered reversing the output due to cons naturally reversing the input
; but ended up leaving it as I was going to then immediately reverse it again to check
; the opposite direction.  Happy coincidence
(define (mark-seen-trees row)
  (cond ((null? row) '())
        ((pair? row)
         (cdr (fold (lambda (tree acc)
                 (let ((largest-tree (car acc))
                       (marked-trees (cdr acc))
                       (current-tree (car tree))
                       (tree-seen (cdr tree)))
                   (let ((is-biggest (> current-tree largest-tree)))
                     (cons (if is-biggest current-tree largest-tree) ; Find biggest tree
                           (cons (cons current-tree (or tree-seen is-biggest)) marked-trees)))))
               '(-1) row)))
        (else error "Given param was not a list" row)))

; Stole from back in day 5.  Not changing, still think it is bad, but hopefully not
; enough to kill us when combined with the already bad performance from `mark-seen-trees`
(define (rotate-matrix m)
  (map (lambda (x) (remove null? x))
       (cond ((null? m) '())
             ((null? (car m)) '())
             (else (cons (map car m) (rotate-matrix (map cdr m)))))))

; I will forever be surprised by how fast this works.  It has no right to work at all
; Every part of my being looks at this and feels that I have done way too much work
; and that there should be a simpler way than this.  And yet, it runs almost instantly
(define (mark-all-seen-trees rows)
  (let* ((check-both (compose mark-seen-trees mark-seen-trees))
         (post-row-check (map (compose check-both parse-input-row) rows)))
    (map check-both (rotate-matrix post-row-check))))

(define (part1 rows)
  (fold (lambda (row acc) (+ acc (length (filter cdr row)))) 0 (mark-all-seen-trees rows)))

; (for-each (lambda (row) (display row) (newline)) (with-file-as-list "day08/sample.txt" part1))
(with-file-as-list "day08/input.txt" part1)
(RESTART 1)

