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

(define (find-move opponent action)
  (cond
   ((eq? action 'win)
    (cond
     ((eq? opponent 'rock) 'paper)
     ((eq? opponent 'paper) 'scissors)
     ((eq? opponent 'scissors) 'rock)))
   ((eq? action 'draw) opponent)
   ((eq? action 'lose)
    (cond
     ((eq? opponent 'rock) 'scissors)
     ((eq? opponent 'paper) 'rock)
     ((eq? opponent 'scissors) 'paper)))))

(define (translate-to-action a)
  (cond
   ((eq? a 'rock) 'lose)
   ((eq? a 'paper) 'draw)
   ((eq? a 'scissors) 'win)))

(define (score-move-part-2 move)
  (let* ((opponent (car move))
	(action (translate-to-action (cdr move)))
	(my-move (find-move opponent action)))
    (round-score (cons opponent my-move))))

(let* ((input (load-input-from-file "input.txt"))
       (score (map round-score input))
       (score-part-2 (map score-move-part-2 input))
       (result (apply + score))
       (result-part-2 (apply + score-part-2)))
  (display result)
  (display #\newline)
  (display result-part-2)
  (display #\newline))
