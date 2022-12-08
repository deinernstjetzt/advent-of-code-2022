(import (scheme base))
(import (scheme char))

(define (load-grid filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((result '()) (cur (read-line)))
        (if (eof-object? cur)
            (list->vector (reverse result))
            (loop (cons (string->vector cur) result) (read-line)))))))

(define (grid-at grid row col)
  (digit-value (vector-ref (vector-ref grid row) col)))

(define (valid-grid-idx? grid row col)
  (if (and (< row (vector-length grid)) (>= row 0) (>= col 0))
      (< col (vector-length (vector-ref grid row)))
      #f))

(define (neighbours-in-dir grid row col dir-r dir-c)
  (let ((new-row (+ row dir-r)) (new-col (+ col dir-c)))
    (if (valid-grid-idx? grid new-row new-col)
        (cons (grid-at grid new-row new-col)
              (neighbours-in-dir grid new-row new-col dir-r dir-c))
        '())))

(define (>all n ls)
  (if (null? ls)
      #t
      (if (> n (car ls))
          (>all n (cdr ls))
          #f)))

(define (tree-visible-in-dir? grid row col dir-r dir-c)
  (let ((nei (neighbours-in-dir grid row col dir-r dir-c))
        (self (grid-at grid row col)))
    (>all self nei)))

(define (tree-visible? grid row col)
  (or (tree-visible-in-dir? grid row col 1 0)
      (tree-visible-in-dir? grid row col -1 0)
      (tree-visible-in-dir? grid row col 0 1)
      (tree-visible-in-dir? grid row col 0 -1)))

(define (map-grid-items grid fn)
  (let loop ((i 0) (j 0) (cur (vector-ref grid 0)) (res '()))
    (if (or (>= j (vector-length cur)) (>= i (vector-length grid)))
        (if (>= i (vector-length grid))
            (reverse res)
            (loop (+ i 1) 0 (vector-ref grid i) res))
        (loop i (+ j 1) cur (cons (fn i j (vector-ref cur j)) res)))))

(define (solve-a)
  (let* ((grid (load-grid "input.txt"))
         (fn (lambda (i j item) (if (tree-visible? grid i j) 1 0))))
    (apply + (map-grid-items grid fn))))
