(import (scheme base))

(define (char-to-move chr)
  (cond
   ((or (eq? chr #\A) (eq? chr #\X)) 'rock)
   ((or (eq? chr #\B) (eq? chr #\Y)) 'paper)
   ((or (eq? chr #\C) (eq? chr #\Z)) 'scissors)))

(define (line-to-move line)
  (let ((fst (string-ref line 0))
	(snd (string-ref line 2)))
    (cons (char-to-move fst) (char-to-move snd))))

(define (load-input-from-file filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((line (read-line))
		 (ls '()))
	(if (eof-object? line)
	    (reverse ls)
	    (loop (read-line)
		  (cons (line-to-move line) ls)))))))

(define (round-outcome move)
  (cond
   ((eq? (car move) (cdr move)) 3)
   ((and (eq? (cdr move) 'rock) (eq? (car move) 'scissors)) 6)
   ((and (eq? (cdr move) 'paper) (eq? (car move) 'rock)) 6)
   ((and (eq? (cdr move) 'scissors) (eq? (car move) 'paper)) 6)
   (else 0)))

(define (round-bonus move)
  (cond
   ((eq? (cdr move) 'rock) 1)
   ((eq? (cdr move) 'paper) 2)
   ((eq? (cdr move) 'scissors) 3)))

(define (round-score move)
  (+ (round-outcome move) (round-bonus move)))

(let* ((input (load-input-from-file "input.txt"))
       (score (map round-score input))
       (result (apply + score)))
  (display result)
  (display #\newline))
