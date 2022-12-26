
;;;;
;; atom? :: S-Exp -> Bool
;;
;; Checks whether `x` is an atom.
;;
;; The default Chicken Scheme `atom?` implementation returns `#t` for
;; `'()`, but the book wants the empty list to NOT be considered an
;; atom. Therefore, the `atom?` implementation from the book.
;;
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(atom? '())

;;
;; Chapter 1 :: Toys
;; =================
;;

;;
;; Mostly getting to know car, cdr, atom?, null? and eq?.
;;

;;
;; Chapter 2 :: Do It Again
;; ========================
;;

;;;;
;; lat? :: List -> Bool
;;
;; Checks whether `l` is a list of atoms.
;;
(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

;;;;
;; member? :: Atom [Atom] -> Bool
;;
;; Checks whether `a` is member of the `lat`.
;;
(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
               (member? a (cdr lat)))))))

;;;;
;; rember :: Atom [Atom] -> [Atom]
;;
;; Removes the first occurrence of `a` in `lat`.
;;
(define rember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) a) (cdr lat))
     (else (cons (car lat)
                 (rember a (cdr lat)))))))

;;;;
;; firsts :: [List] -> [List]
;;
;; Returns a list of all car's of the sublists.
;;
;; ASSUME: The input list is either an empty list or
;; a list composed of only non-empty lists. That is,
;; it is expected that we can `car` each sub-list.
;;
(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     (else
      (cons (car (car l))
            (firsts (cdr l)))))))

