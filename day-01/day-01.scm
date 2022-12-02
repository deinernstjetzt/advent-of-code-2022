(import (scheme base))

(define (load-input-from-file filename)
  (call-with-input-file filename
    (lambda (port)
      (let loop ((current-line (read-line port))
		 (current-list '())
		 (result '()))
        (if (eof-object? current-line)
	    (if (null? current-list)
		result
		(cons current-list result))
	    (let ((cur (string->number current-line)))
	      (if cur
		  (loop (read-line port)
			(cons cur current-list)
			result)
		  (loop (read-line port)
			'()
			(cons current-list result)))))))))

(define (max ls)
  (let loop ((cur (car ls))
	     (ls ls))
    (if (null? ls)
	cur
	(if (> (car ls) cur)
	    (loop (car ls) (cdr ls))
	    (loop cur (cdr ls))))))

(let* ((raw-list (load-input-from-file "input.txt"))
       (cal-list (map (lambda (ls) (apply + ls)) raw-list))
       (sorted-list (sort-list cal-list >)))
  (display (car sorted-list))
  (display "\n")
  (display (+ (car sorted-list) (cadr sorted-list) (caddr sorted-list)))
  (display "\n"))
