
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapter 1 :: Toys
;; =================


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

;;
;; Mostly getting to know car, cdr, atom?, null? and eq?.
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapter 2 :: Do It Again
;; ========================


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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapter 3 :: Cons the Magnificent
;; =================================


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


;;;;
;; insertR :: Atom Atom [Atom] -> [Atom]
;;
;; Inserts `new` to the right of the first occurrence of `old`.
;;
;; ASSUME: `old` is a member of `lat`. If `old` is not a member
;; of `lat`, the unchanged list is returned.
;;
(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old)
      (cons (car lat) (cons new (cdr lat))))
     (else
      (cons (car lat)
            (insertR new old (cdr lat)))))))

;;;;
;; insertL :: Atom Atom [Atom] -> [Atom]
;;
;; Inserts `new` to the left of the first occurrence of `old`.
;;
;; NOTE: If `old` is not found, the list is returned unchanged.
;;
(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old)
      (cons new lat))
     (else
      (cons (car lat)
            (insertL new old (cdr lat)))))))

;;;;
;; subst :: Atom Atom [Atom] -> [Atom]
;;
;; Substitute the first occurrence of `old` with `new`.
;;
;; If `old` is not found, the list is returned unchanged.
;;
(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old)
      (cons new (cdr lat)))
     (else (cons (car lat)
                 (subst new old (cdr lat)))))))

;;;;
;; subst2 :: Atom Atom Atom [Atom] -> [Atom]
;;
;; Substitutes the first occurrence of either `o1` or `o2`
;; with `new`. If neither is found, return the list unchanged.
;;
(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) '())
     ((or (eq? (car lat) o1) (eq? (car lat) o2))
      (cons new (cdr lat)))
     (else (cons (car lat)
                 (subst2 new o1 o2 (cdr lat)))))))


;;;;
;; multirember :: Atom [Atom] -> [Atom]
;;
;; Removes all occurrences of `a` from `lat`.
;;
(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) a) (multirember a (cdr lat)))
     (else (cons (car lat) (multirember a (cdr lat)))))))


;;;;
;; multiinsertR :: Atom Atom [Atom] -> [Atom]
;;
;; Inserts `new` to the right of all occurrences of `old`.
;;
(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old)
      (cons old
            (cons new
                  (multiinsertR new old (cdr lat)))))
     (else
      (cons (car lat)
            (multiinsertR new old (cdr lat)))))))


;;;;
;; multiinsertL :: Atom Atom [Atom] -> [Atom]
;;
;; Inserts `new` to the left of all occurrences of `old`.
;;
(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old)
      (cons new
            (cons old
                  (multiinsertL new old (cdr lat)))))
     (else
      (cons (car lat)
            (multiinsertL new old (cdr lat)))))))


;;;;
;; multisubst :: Atom Atom [Atom] -> [Atom]
;;
;; Replaces all occurrences of `old` with `new`.
;;
(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old)
      (cons new (multisubst new old (cdr lat))))
     (else
      (cons (car lat) (multisubst new old (cdr lat)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapter 4 :: Number Games
;; =========================

;;;;
;; o+ :: Natural Natural -> Natural
;;
;; Computes the sum of `n` and `m`.
;;
(define o+
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (o+ (add1 n) (sub1 m))))))



;;;;
;; o- :: Natural Natural -> Natural
;;
;; Computes the difference of `n` and `m` (subtracts `m` from `n`).
;;
(define o-
  (lambda (n m)
    (cond
     [(zero? m) n]
     [else (o- (sub1 n) (sub1 m))])))

;;;;
;; addtup :: [Natural] -> Natural
;;
;; Adds up all elements of `tup`.
;;
(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else (o+ (car tup)
               (addtup (cdr tup)))))))

;;;;
;; o* :: Natural Natural -> Natural
;;
;; Calculates the product of `n` and `m`.
;;
(define o*
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (o+ n (o* n (sub1 m)))))))

;;;;
;; tup+ :: [Natural] [Natural] -> [Natural]
;;
;; tup+ :: Adds pairs of numbers from `tup1` and `tup2`.
;;
;; If one tup is lengthier, appends those elements to the end
;; of the resulting list.
;;
(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else (cons (+ (car tup1) (car tup2))
                 (tup+ (cdr tup1) (cdr tup2)))))))


;;;;
;; o> :: Int Int -> Bool
;;
;; Checks whether `n` is greater than `m`.
;;
;; NOTE: The order of the two `(zero? ...)` conditions matter
;; when `n` and `m` are the same number. We MUST check for
;; `(zero? n)` first.
;;
(define o>
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else (o> (sub1 n) (sub1 m))))))


;;;;
;; o< :: Int Int -> Bool
;;
;; Checks whether `n` is less than `m`.
;;
;; NOTE: The order of the two `(zero? ...)` conditions matter
;; when `n` and `m` are the same number. We MUST check for
;; `(zero? m)` first.
;;
(define o<
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else (o< (sub1 n) (sub1 m))))))


;;;;
;; o= :: Int Int -> Bool
;;
;; Checks whether `n` and `m` are the same value.
;;
(define o=
  (lambda (n m)
    (cond
     ((zero? n) (zero? m))
     ((zero? n) #f)
     (else (o= (sub1 n) (sub1 m))))))


;;;;
;; o** :: Natural Natural -> Natural
;;
;; Raises `b` to the `e`th power.
;;
(define o**
  (lambda (b e)
    (cond
     ((zero? e) 1)
     (else (o* b (o** b (sub1 e)))))))

;;;;
;; o/ :: Int Int -> Int
;;
;; Computes the integer division for `n` and `m`, that is,
;; how many times `m` goes into the first`.
;;
;; NOTE: The remainder is ignored.
;;
(define o/
  (lambda (n m)
    (cond
     ((< n m) 0)
     (else (add1 (o/ (- n m) m))))))

;;;;
;; len :: [Atom] -> Int
;;
;; Computes the length of `lat`.
;;
(define len
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else (add1 (len (cdr lat)))))))


;;;;
;; pick :: Int [Atom] -> Atom
;;
;; Retrieves the atom at index `n`.
;;
;; NOTE: `pick` is defined only for non-empty lists and for
;; non-zero indexes.
;;
;; NOTE: If not found, returns the last atom blindly; that
;; is, assume the atom is present.
;;
(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (car lat))
     (else (pick (sub1 n) (cdr lat))))))


;;;;
;; rempick :: Int [Atom] -> [Atom]
;;
;; Returns the `lat` with element at `n` removed.
;;
;; NOTE: `rempick` is defined only for non-empty lists and for
;; non-zero indexes.
;;
;; ASSUME: The element at index `n` is a member of the `lat`.
;;
(define rempick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (cdr lat))
     (else (cons
            (car lat)
            (rempick (sub1 n) (cdr lat)))))))


;;;;
;; no-nums :: [Atom] -> [Atom]
;;
;; Returns `lat` with all numbers removed.
;;
(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((number? (car lat)) (no-nums (cdr lat)))
     (else (cons (car lat)
                 (no-nums (cdr lat)))))))

;;;;
;; all-nums :: [Atom] -> [Atom]
;;
;; Returns a list containing all numbers in `lat`.
;;
(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((number? (car lat))
      (cons (car lat)
            (all-nums (cdr lat))))
     (else (all-nums (cdr lat))))))

