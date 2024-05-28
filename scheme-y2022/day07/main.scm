; Ugh, this one is kind of annoying
; I think approach is going to be first write a parser for the file
; Basically treat each line as a command to run
; Will then need to build some tree struct for the commands to run on
; So, 'cd /' would attempt to set '/' as the context for our interpreter
; 'ls' would start adding children
; We would then read lines until we see another command, which all start with $
; I think parser won't be bad, just a question if I parse then run or just all at once
; The question on the tree will be how to build.  Probably bottom up
; The commands are essentially acting as a DFS on the file system, so if I do the same
; I should be able to recreate without having to do any weird mutations
; Wonder if I could do the whole thing as a `fold-right`...

; A little parser for the commands.
; Tags based on if it is data or a cmd, then parses the file size if present
(define (parse-command row)
  (let ((words ((string-splitter) row)))
    (cond ((eq? #\$ (string-ref row 0)) (cons 'cmd (cdr words)))
          ((equal? "dir" (first words)) (cons 'dir words))
          (else (list 'data (string->number (first words)) (second words))))))

; Apply cmds, returning out the unparsed commands and the final tree
(define (apply-commands cmds buf current-dir)
  (if (null? cmds)
    (list '() (list (fold + 0 (map car buf)) current-dir buf))                                                     ; If we are out of cmds, calculate and return
    (let ((cmd (car cmds)))
      (cond ((eq? 'data (first cmd)) (apply-commands (cdr cmds) (cons (cdr cmd) buf) current-dir))                 ; Put data in the buffer for the next dir
            ((eq? 'dir (first cmd)) (apply-commands (cdr cmds) buf current-dir))                                   ; Ignore dirs in the list
            ((eq? 'cmd (first cmd))
             (cond ((string=? "ls" (second cmd)) (apply-commands (cdr cmds) buf current-dir))                      ; Ignore ls command, pointless for parser
                   ((string=? ".." (third cmd))
                    (list (cdr cmds)                                                                               ; If we are moving up, then return what we have
                          (list (fold + 0 (map car buf)) current-dir buf)))
                   ; Double recursion, this basically says to treat the dir as root
                   ; then when it finishes, treat the result as part of our buffer
                   (else
                     (let ((result (apply-commands (cdr cmds) '() (third cmd))))
                       (apply-commands (car result) (cons (cadr result) buf) current-dir)))))
            (else (begin (display cmd) (error "Unsupported cmd type")))))))

; Now for the technically inefficient part.  I wrote the interpreter to build the tree, but not do the problem
; It would have been more efficient if I had done the problem while building the tree, but I know how these problems go
; Part two will probably require some extra logic on how to traverse the tree, so keeping separate for now
; Going to write this one to return all the nodes that pass a condition.  Hopefully, will save work on part two
(define (extract-nodes condition tree)
  (cond ((null? tree) '())
        ((eq? 2 (length tree)) '())
        (else
          (append (if (condition (list (first tree) (second tree))) (list (first tree)) '())
                  (append-map (lambda (child) (extract-nodes condition child)) (third tree))))))

(define (part1 file)
  (fold + 0 (extract-nodes
    (lambda (node) (>= 100000 (car node)))
    (cadr (apply-commands (cdr (map parse-command file)) '() "/")))))

; Justified.  Part two needs the smalles dir that would free up the necessary space.
; Basically another tree walk with a sort on the results
(define (part2 file)
  (car (csort <
              (let* ((fs-tree (cadr (apply-commands (cdr (map parse-command file)) '() "/")))
                     (root-size (first fs-tree))
                     (needed (- 30000000 (- 70000000 root-size)))) ; Late night me can't seem to think of a simpler form of this
                (extract-nodes (lambda (node) (<= needed (car node))) fs-tree)))))

