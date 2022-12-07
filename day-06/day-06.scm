(import (scheme base))

(define (load-file filename)
  (with-input-from-file filename read-line))

(define (sublist-contains? ls max-len item)
  (if (or (zero? max-len) (null? ls))
      #f
      (if (equal? (car ls) item)
          #t
          (sublist-contains? (cdr ls) (- max-len 1) item))))

(define (first-four-distinct? ls)
  (let loop ((ls ls) (i 0))
    (cond
      ((eq? i 4) #t)
      ((null? ls) #f)
      ((sublist-contains? (cdr ls) (- 3 i) (car ls)) #f)
      (else (loop (cdr ls) (+ i 1))))))

(define (find-marker chars)
  (let loop ((chars chars) (prev-read '()) (i 0))
    (cond
      ((first-four-distinct? prev-read) i)
      ((null? chars) -1)
      (else (loop (cdr chars) (cons (car chars) prev-read) (+ i 1))))))

(let* ((input (load-file "input.txt"))
       (marker (find-marker (string->list input))))
  (display marker)
  (newline))