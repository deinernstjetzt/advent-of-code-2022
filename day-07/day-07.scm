(import (scheme base))

(define (load-lines-of-file filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((ls '()) (line (read-line)))
        (if (eof-object? line)
            (reverse ls)
            (loop (cons line ls) (read-line)))))))

(define (split-at-char str chr)
  (let loop ((i 0) (len (string-length str)))
    (cond
      ((eq? i len) (cons str ""))
      ((eq? (string-ref str i) chr) (cons (string-copy str 0 i) (string-copy str (+ i 1))))
      (else (loop (+ i 1) len)))))

; parse a single line of ls's output
; the result is a list where the first element is either 'dir or 'file
; the second element is the name and the third element is the size (if applicable)
(define (parse-ls-line str)
  (let* ((split (split-at-char str #\space))
         (sz (car split))
         (name (cdr split)))
    (if (equal? sz "dir")
        (list 'dir name)
        (list 'file name (string->number sz)))))

; parse all of the output of ls
; the result is a pair of a list of parsed ls lines and a list of leftover lines
(define (parse-ls-output lines)
  (let loop ((lines lines) (res '()))
    (if (or (null? lines) (eq? (string-ref (car lines) 0) #\$))
        (cons (reverse res) lines)
        (loop (cdr lines) (cons (parse-ls-line (car lines)) res)))))

(define (parse-cmd-line line)
  (if (not (equal? (string-copy line 0 2 ) "$ "))
      (error "parse-cmd-line: invalid command line string" line))
  (let* ((split (split-at-char (string-copy line 2) #\ ))
         (cmd (car split))
         (param (cdr split)))
    (if (equal? cmd "cd")
        (list 'cd param)
        (list 'ls))))
  
(define (parse-command lines)
  (let* ((command (parse-cmd-line (car lines)))
         (is-ls (eq? (car command) 'ls))
         (output (if is-ls (parse-ls-output (cdr lines)))))
    (if is-ls
        (cons (list 'ls (car output)) (cdr output))
        (cons command (cdr lines)))))

(define (parse-commands lines)
  (let loop ((commands '()) (lines lines))
    (if (null? lines)
        (reverse commands)
        (let* ((res (parse-command lines))
               (cmd (car res))
               (leftover (cdr res)))
          (loop (cons cmd commands) leftover)))))

(define (item-name item) (cadr item))

(define (tree-file name size)
  (list 'file name size))

(define (tree-dir name . items)
  (list 'dir name items))

(define (tree-dir-add dir item path)
  (if (null? path)
      (list 'dir (cadr dir) (cons item (caddr dir)))
      (let loop ((items (caddr dir)) (new-items '()))
        (if (null? items)
            (list 'dir (cadr dir) (reverse new-items))
            (if (equal? (item-name (car items)) (car path))
                (loop (cdr items) (cons (tree-dir-add (car items) item (cdr path)) new-items))
                (loop (cdr items) (cons (car items) new-items)))))))

(define (tree-dir-has? dir name path)
  (if (null? path)
      (let loop ((items (caddr dir)))
        (if (null? items)
            #f
            (if (equal? (item-name (car items)) name)
                #t
                (loop (cdr items)))))
      (let loop ((items (caddr dir)))
        (if (null? items)
            #f
            (if (equal? (item-name (car items)) (car path))
                (tree-dir-has? (car items) name (cdr path))
                (loop (cdr items)))))))

(define (build-tree cmds)
  (let loop ((cmds cmds) (root (tree-dir "/")) (path '()))
    (cond
      ((null? cmds) root)
      ((and (eq? (caar cmds) 'cd) (equal? (cadar cmds) "/")) (loop (cdr cmds) root '()))
      ((and (eq? (caar cmds) 'cd) (equal? (cadar cmds) "..")) (loop (cdr cmds) root (cdr path)))
      ((eq? (caar cmds) 'cd) (loop (cdr cmds) root (cons (cadar cmds) path)))
      (else
        (let loop2 ((items (cadar cmds)) (root root))
          (cond
            ((null? items) (loop (cdr cmds) root path))
            ((eq? (caar items) 'file) (loop2 (cdr items) (tree-dir-add root (car items) (reverse path))))
            (else (loop2 (cdr items) (tree-dir-add root (list 'dir (cadar items) '()) (reverse path))))))))))

(define (dir-size dir)
  (let loop ((items (caddr dir)) (size 0))
    (cond
      ((null? items) size)
      ((eq? (caar items) 'file) (loop (cdr items) (+ size (caddar items))))
      (else (loop (cdr items) (+ size (dir-size (car items))))))))

(define (sum-of-dir-size-below dir limit)
  (let ((own-size (dir-size dir)))
    (let loop ((size (if (<= own-size limit) own-size 0))
               (items (caddr dir)))
      (cond
        ((null? items) size)
        ((eq? (caar items) 'dir) (loop (+ size (sum-of-dir-size-below (car items) limit)) (cdr items)))
        (else (loop size (cdr items)))))))