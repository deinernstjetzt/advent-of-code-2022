(define (load-lists-from-file filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((res '())
                 (line (read)))
        (if (eof-object? line)
            (reverse res)
            (loop (cons line res) (read)))))))

(define (nth ls idx)
  (if (zero? idx)
      (car ls)
      (nth (cdr ls) (- idx 1))))

(define (exec-move stacks count from to)
  (if (zero? count)
      stacks
      (let loop ((ls stacks) (res '()) (i 1) (container (car (nth stacks (- from 1)))))
        (cond
          ((null? ls) (exec-move (reverse res) (- count 1) from to))
          ((eq? i from) (loop (cdr ls) (cons (cdr (car ls)) res) (+ i 1) container))
          ((eq? i to) (loop (cdr ls) (cons (cons container (car ls)) res) (+ i 1) container))
          (else (loop (cdr ls) (cons (car ls) res) (+ i 1) container))))))

(define (exec-moves stacks moves)
  (if (null? moves)
      stacks
      (exec-moves (exec-move stacks (caar moves) (cadar moves) (caddar moves)) (cdr moves))))

(define (stack-tops stacks)
  (if (null? stacks)
      '()
      (cons (caar stacks) (stack-tops (cdr stacks)))))

(let* ((stacks (load-lists-from-file "stacks.txt"))
       (moves (load-lists-from-file "moves.txt"))
       (res (exec-moves stacks moves)))
  (display (stack-tops res))
  (newline))
