(import (scheme base))
(import (scheme char))

(define (map-lines-of-file filename proc)
  (with-input-from-file filename
    (lambda ()
      (let loop ((lines '()) (cur (read-line)))
	(if (eof-object? cur)
	    (reverse lines)
	    (loop (cons (proc cur) lines) (read-line)))))))

(define (flatten ls)
  (let loop ((res '()) (ls ls))
    (cond
     ((null? ls) (reverse res))
     ((null? (car ls)) (loop res (cdr ls)))
     (else (loop (cons (caar ls) res) (cons (cdar ls) (cdr ls)))))))

(define (distinct ls)
  (let loop ((res '()) (ls ls))
    (if (null? ls)
	(reverse res)
	(if (member (car ls) (cdr ls))
	    (loop res (cdr ls))
	    (loop (cons (car ls) res) (cdr ls))))))

(define (parse-input-line str)
  (let* ((count (string->number (string-copy str 2)))
	 (symbol (string->symbol (string-downcase (string-copy str 0 1)))))
    (make-list count symbol)))

(define (load-input filename)
  (flatten
   (map-lines-of-file filename parse-input-line)))

(define (adjacent? head tail)
  (let* ((diff (map - head tail))
	 (dist (apply max (map abs diff))))
    (<= dist 1)))

(define (sgn n)
  (cond
   ((< n 0) -1)
   ((> n 0) 1)
   (else 0)))

(define (pos-x pos) (car pos))

(define (pos-y pos) (cadr pos))

(define (next-tail-pos head tail)
  (if (adjacent? head tail)
      tail
      (list (+ (pos-x tail)
	       (sgn (- (pos-x head) (pos-x tail))))
	    (+ (pos-y tail)
	       (sgn (- (pos-y head) (pos-y tail)))))))

(define (next-head-pos head move)
  (cond
   ((eq? move 'u) (map - head '(0 1)))
   ((eq? move 'd) (map + head '(0 1)))
   ((eq? move 'l) (map - head '(1 0)))
   ((eq? move 'r) (map + head '(1 0)))))

(define (tail-positions moves)
  (let loop ((head '(0 0))
	     (tail '(0 0))
	     (moves moves)
	     (res '((0 0))))
    (if (null? moves)
	(distinct res)
	(let* ((next-head (next-head-pos head (car moves)))
	       (next-tail (next-tail-pos next-head tail)))
	  (loop next-head next-tail (cdr moves) (cons next-tail res))))))

(let* ((input (load-input "input.txt"))
       (tail-pos (tail-positions input)))
  (display (length tail-pos))
  (newline))
