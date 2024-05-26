; Parse the line into a pair of pairs
(define parse-line
  (lambda (str)
    (let* ((comma-splitter (string-splitter 'delimiter #\,))
           (dash-splitter (string-splitter 'delimiter #\-))
           ; Apply comma split, then dash split
           (full-split-str (map dash-splitter (comma-splitter str))))
      ; Double map string->number through each value.
      (<$$> string->number full-split-str))))

(define nested-pairs?
  (lambda (left-pair right-pair)
    (let ((check (lambda (l r)
                   (and (>= (car l) (car r))
                        (<= (cadr l) (cadr r))))))
      (or (check left-pair right-pair) (check right-pair left-pair)))))

(define overlap-pairs?
  (lambda (left-pair right-pair)
    (let ((check (lambda (point pair)
                   (and (>= point (car pair))
                        (<= point (cadr pair))))))
      (or (or (check (car left-pair) right-pair)
              (check (cadr left-pair) right-pair))
          (or (check (car right-pair) left-pair)
              (check (cadr right-pair) left-pair))))))


(define part1
  (lambda (l)
    (length (filter (lambda (row) (apply nested-pairs? (parse-line row))) l))))


(define part2
  (lambda (l)
    (length (filter (lambda (row) (apply overlap-pairs? (parse-line row))) l))))

