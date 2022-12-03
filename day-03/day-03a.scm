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

(define (item-priority chr)
  (if (char-lower-case? chr)
      (+ (- (char->integer chr) (char->integer #\a)) 1)
      (+ (- (char->integer chr) (char->integer #\A)) 27)))

(display (apply + (map (lambda (rucksack)
			 (item-priority
			  (find-duplicate-chr (car rucksack)
					      (cdr rucksack))))
		       (load-input "input.txt"))))

(newline)
