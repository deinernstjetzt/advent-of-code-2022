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
