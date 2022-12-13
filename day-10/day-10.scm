(import (scheme base))

(define (map-lines-of-file filename proc)
  (with-input-from-file filename
    (lambda ()
      (let loop ((lines '()) (cur (read-line)))
	(if (eof-object? cur)
	    (reverse lines)
	    (loop (cons (proc cur) lines) (read-line)))))))

(define (parse-input-line line)
  (if (equal? (string-copy line 0 4) "noop")
      (list 'noop)
      (list 'addx (string->number (string-copy line 5)))))

(define (load-input filename)
  (map-lines-of-file filename parse-input-line))

(define (convert-to-addx inst)
  (let loop ((res '()) (ls inst))
    (cond
     ((null? ls) (reverse res))
     ((eq? (caar ls) 'noop) (loop (cons 0 res) (cdr ls)))
     (else (loop (cons (cadar ls) (cons 0 res)) (cdr ls))))))

(define (signal-strength-at inst at-cycle)
  (let loop ((x 1) (inst inst) (cycle 1))
    (if (eq? cycle at-cycle)
	(* cycle x)
	(loop (+ x (car inst)) (cdr inst) (+ cycle 1)))))

(define (in-sprite sprite-center x)
  (or
   (= (- sprite-center 1) x)
   (= sprite-center x)
   (= (+ sprite-center 1) x)))

(define (exec-draw inst)
  (let loop ((x 1) (inst inst) (cycle 1) (screen '()))
    (if (> cycle 240)
	(list->string (reverse screen))
	(let* ((col (remainder (- cycle 1) 40))
	       (on (in-sprite x col))
	       (chr (if on #\# #\.))
	       (screen (if (= col 39)
			   (cons #\newline (cons chr screen))
			   (cons chr screen))))
	  (loop (+ x (car inst)) (cdr inst) (+ cycle 1) screen)))))

(let* ((input (load-input "input.txt"))
       (inst (convert-to-addx input))
       (sig-at (lambda (at) (signal-strength-at inst at)))
       (fst (apply + (map sig-at (list 20 60 100 140 180 220))))
       (snd (exec-draw inst)))
  (display fst)
  (newline)
  (display snd))
 
