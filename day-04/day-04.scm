(import (scheme base))
(import (scheme char))

(define (map-lines-of-file filename proc)
  (with-input-from-file filename
    (lambda ()
      (let loop ((ls '()) (line (read-line)))
	(if (eof-object? line)
	    (reverse ls)
	    (loop (cons (proc line) ls) (read-line)))))))

(define (parse-number str)
  (if (or (eq? (string-length str) 0)
	  (not (char-numeric? (string-ref str 0))))
      (error "parse-number: string does not begin with a digit" str)
      (let loop ((i 0) (n 0) (chr (string-ref str 0)))
        (cond
	 ((not (char-numeric? chr)) (cons n (string-copy str i)))
	 ((eq? (+ i 1) (string-length str))
	  (cons (+ (* n 10) (digit-value chr)) ""))
	 (else
	  (loop (+ i 1)
		(+ (* n 10) (digit-value chr))
		(string-ref str (+ i 1))))))))

(define (parse-assignment-pair str)
  (let* ((a (parse-number str))
	 (b (parse-number (string-copy (cdr a) 1)))
	 (c (parse-number (string-copy (cdr b) 1)))
	 (d (parse-number (string-copy (cdr c) 1))))
    (cons (cons (car a) (car b))
	  (cons (car c) (car d)))))

(define (load-input filename)
  (map-lines-of-file filename parse-assignment-pair))

(define (subrange-of? a b)
  (and
   (<= (car b) (car a))
   (>= (cdr b) (cdr a))))

(define (does-overlap? a b)
  (or (and (>= (car a) (car b)) (<= (car a) (cdr b)))
      (and (>= (cdr a) (car b)) (<= (cdr a) (cdr b)))
      (subrange-of? a b)
      (subrange-of? b a)))

(define (count-true ls proc)
  (apply + (map (lambda (n) (if (proc n) 1 0)) ls)))

(let ((input (load-input "input.txt")))
  (display (count-true input
		       (lambda (a) (or (subrange-of? (car a) (cdr a))
				       (subrange-of? (cdr a) (car a))))))
  (newline)
  (display (count-true input
		       (lambda (a) (does-overlap? (car a) (cdr a)))))
  (newline))
