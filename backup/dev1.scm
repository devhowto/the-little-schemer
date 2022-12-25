;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(define rember*
  (lambda (a l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) a) (rember* a (cdr l)))
       (else (cons (car l) (rember* a (cdr l))))))
     (else
      (cons (rember* a (car l))
            (rember* a (cdr l)))))))
