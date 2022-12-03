(import (scheme base))

(define (string-contains? str chr)
  (call/cc
   (lambda (early-return)
     (string-for-each (lambda (c) (if (eq? c chr) (early-return #t))) str) #f)))

(define (map-lines-of-file filename proc)
  (with-input-from-file filename
    (lambda ()
      (let loop ((content (read-line))
		 (ls '()))
	(if (eof-object? content)
	    (reverse ls)
	    (loop (read-line) (cons (proc content) ls)))))))

(define (load-input filename)
  (map-lines-of-file filename
    (lambda (line)
      (cons
       (string-copy line 0 (/ (string-length line) 2))
       (string-copy line (/ (string-length line) 2))))))
      
(define (find-duplicate-chr str-1 str-2)
  (call/cc
   (lambda (early-return)
     (string-for-each (lambda (c)
			(if (string-contains? str-2 c)
			    (early-return c))) str-1))))

(define (find-duplicate-chr-3 str-1 str-2 str-3)
  (call/cc
   (lambda (early-return)
     (string-for-each (lambda (c)
			(if (and (string-contains? str-2 c)
				 (string-contains? str-3 c))
			    (early-return c))) str-1))))

(define (item-priority chr)
  (if (char-lower-case? chr)
      (+ (- (char->integer chr) (char->integer #\a)) 1)
      (+ (- (char->integer chr) (char->integer #\A)) 27)))

(define (make-sublists ls sublist-size)
  (let loop ((ls ls) (cur '()) (n 0) (res '()))
    (cond
     ((and (null? ls) (null? cur)) res)
     ((null? ls) (cons cur res))
     ((eq? n sublist-size) (loop ls '() 0 (cons cur res)))
     (else (loop (cdr ls) (cons (car ls) cur) (+ n 1) res)))))
	

(display (apply + (map (lambda (rucksack)
			 (item-priority
			  (find-duplicate-chr (car rucksack)
					      (cdr rucksack))))
		       (load-input "input.txt"))))

(newline)

(display (apply + (map (lambda (group)
			 (item-priority (apply find-duplicate-chr-3 group)))
		       (make-sublists
			(map-lines-of-file "input.txt"
					   (lambda (x) x)) 3))))

(newline)
