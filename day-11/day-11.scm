(import (scheme base))
(import (scheme eval))
(import (srfi srfi-17))

(define (read-lines-of-file filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((res '()) (cur (read-line)))
	(if (eof-object? cur)
	    (reverse res)
	    (loop (cons cur res) (read-line)))))))

(define (split-list ls at)
  (let loop ((ls ls) (res '()) (cur '()))
    (cond
     ((and (null? ls) (null? cur)) (reverse res))
     ((and (null? ls) (reverse (cons (reverse cur) res))))
     ((equal? (car ls) at) (loop (cdr ls) (cons (reverse cur) res) '()))
     (else (loop (cdr ls) res (cons (car ls) cur))))))

(define (parse-monkey-id line)
  (string->number (string-copy line 7 (- (string-length line) 1))))

(define (split-string line at)
  (let* ((idx (string-contains line at))
	 (at-len (string-length at)))
    (if idx
	(cons (string-copy line 0 idx)
	      (split-string (string-copy line (+ idx at-len)) at))
	(list line))))

(define (parse-number-list line)
  (let* ((nums (split-string line ", ")))
    (map string->number nums)))

(define (string-after str after)
  (let* ((start (string-contains str after)))
    (if start
	(string-copy str (+ start (string-length after)))
	str)))

(define (parse-starting-items line)
  (parse-number-list
   (string-after line "Starting items: ")))

(define (parse-symbol-or-number str)
  (let ((n (string->number str)))
    (if n n (string->symbol str))))

(define (parse-operation line)
  (let* ((op (string-after line "new = "))
	 (tokens (split-string op " "))
	 (lhs (parse-symbol-or-number (first tokens)))
	 (rhs (parse-symbol-or-number (third tokens)))
	 (operator (string->symbol (second tokens))))
    (list operator lhs rhs)))

(define (compile-operation op)
  (eval
   (quasiquote (lambda (old) (unquote op)))
   (environment '(scheme base))))

(define (parse-divisible-by line)
  (string->number (string-after line "divisible by ")))

(define (parse-throw-cmd line)
  (string->number (string-after line "throw to monkey ")))

(define (parse-test fst snd thd)
  (let* ((div-by (parse-divisible-by fst))
	 (true-case (parse-throw-cmd snd))
	 (false-case (parse-throw-cmd thd)))
    (list div-by true-case false-case)))

(define (parse-monkey lines)
  (let* ((id (parse-monkey-id (first lines)))
	 (items (parse-starting-items (second lines)))
	 (op (compile-operation (parse-operation (third lines))))
	 (test (parse-test (fourth lines) (fifth lines) (sixth lines))))
    (list id items op test)))

(define (load-monkeys filename)
  (let* ((lines (read-lines-of-file filename))
	 (split (split-list lines ""))
	 (monkeys (map parse-monkey split)))
    monkeys))
