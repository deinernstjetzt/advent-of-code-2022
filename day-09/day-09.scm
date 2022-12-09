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

(define (last ls)
  (if (null? (cdr ls))
      (car ls)
      (last (cdr ls))))

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

(define (next-tail-pos head tail)
  (if (adjacent? head tail)
      tail
      (map + tail (map sgn (map - head tail)))))

(define (next-head-pos head move)
  (cond
   ((eq? move 'u) (map - head '(0 1)))
   ((eq? move 'd) (map + head '(0 1)))
   ((eq? move 'l) (map - head '(1 0)))
   ((eq? move 'r) (map + head '(1 0)))))

(define (next-chain-position chain move)
  (let ((first-head (next-head-pos (car chain) move)))
    (let loop ((cur-head first-head)
	       (res (list first-head))
	       (chain (cdr chain)))
      (if (null? chain)
	  (reverse res)
	  (let ((cur-tail (next-tail-pos cur-head (car chain))))
	    (loop cur-tail (cons cur-tail res) (cdr chain)))))))

(define (tail-positions-chain moves chain-length)
  (let loop ((chain (make-list chain-length '(0 0)))
	     (moves moves)
	     (res '((0 0))))
    (if (null? moves)
	(distinct res)
	(let* ((next-chain (next-chain-position chain (car moves)))
	       (last-item (last next-chain)))
	  (loop next-chain (cdr moves) (cons last-item res))))))

(let* ((input (load-input "input.txt"))
       (tail-pos (tail-positions-chain input 2))
       (tail-pos-chain (tail-positions-chain input 10)))
  (display (length tail-pos))
  (newline)
  (display (length tail-pos-chain))
  (newline))
