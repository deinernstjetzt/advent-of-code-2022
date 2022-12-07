(import (scheme base))

(define (load-file filename)
  (with-input-from-file filename read-line))

(define (sublist-contains? ls max-len item)
  (if (or (zero? max-len) (null? ls))
      #f
      (if (equal? (car ls) item)
          #t
          (sublist-contains? (cdr ls) (- max-len 1) item))))

(define (first-n-distinct? ls n)
  (let loop ((ls ls) (i 0))
    (cond
      ((eq? i n) #t)
      ((null? ls) #f)
      ((sublist-contains? (cdr ls) (- n i 1) (car ls)) #f)
      (else (loop (cdr ls) (+ i 1))))))

(define (find-marker chars marker-size)
  (let loop ((chars chars) (prev-read '()) (i 0))
    (cond
      ((first-n-distinct? prev-read marker-size) i)
      ((null? chars) -1)
      (else (loop (cdr chars) (cons (car chars) prev-read) (+ i 1))))))

(let* ((input (load-file "input.txt"))
       (input-ls (string->list input))
       (packet-marker (find-marker input-ls 4))
       (message-marker (find-marker input-ls 14)))
  (display packet-marker)
  (newline)
  (display message-marker)
  (newline))