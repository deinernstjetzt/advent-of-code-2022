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

(define (move-n count from to)
  (let loop ((ls '()) (i 0) (from from))
    (if (< i count)
        (loop (cons (car from) ls) (+ i 1) (cdr from))
        (let loop2 ((ls ls) (to to))
          (if (null? ls)
              (cons from to)
              (loop2 (cdr ls) (cons (car ls) to)))))))

(define (exec-move-snd stacks count from to)
  (let* ((from-stack (nth stacks (- from 1)))
         (to-stack (nth stacks (- to 1)))
         (res (move-n count from-stack to-stack))
         (new-from (car res))
         (new-to (cdr res)))
    (let loop ((ls '()) (i 1) (stacks stacks))
      (cond
        ((null? stacks) (reverse ls))
        ((eq? i from) (loop (cons new-from ls) (+ i 1) (cdr stacks)))
        ((eq? i to) (loop (cons new-to ls) (+ i 1) (cdr stacks)))
        (else (loop (cons (car stacks) ls) (+ i 1) (cdr stacks)))))))

(define (exec-moves stacks moves)
  (if (null? moves)
      stacks
      (exec-moves (exec-move stacks (caar moves) (cadar moves) (caddar moves)) (cdr moves))))

(define (exec-moves-snd stacks moves)
  (if (null? moves)
      stacks
      (exec-moves-snd (exec-move-snd stacks (caar moves) (cadar moves) (caddar moves)) (cdr moves))))

(define (stack-tops stacks)
  (if (null? stacks)
      '()
      (cons (caar stacks) (stack-tops (cdr stacks)))))

(let* ((stacks (load-lists-from-file "stacks.txt"))
       (moves (load-lists-from-file "moves.txt"))
       (res (exec-moves stacks moves))
       (res-snd (exec-moves-snd stacks moves)))
  (display (stack-tops res))
  (newline)
  (display (stack-tops res-snd))
  (newline))
