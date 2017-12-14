(require-extension test) ; chicken-install -s test

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expr -> Bool
;; Takes an expression and produce #t if it is an atom; #f otherwise.
;;
;; NOTE:
;; Chicken has its own implementation of atom?. It produces
;; `#t` for '(). This version (from the book) produces #f for '().

(test-group "`atom?':"
            (test "is 'yoda an atom? Yes, it is."
                  #t
                  (atom? 'yoda))
            (test "'(foo) should not be an atom"
                  #f
                  (atom? '(foo))))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (list-of Atom) -> Bool
;; Produce #t if l is a list of atoms, #f otherwise.

(test-group "`lat?'"
            (test "list with only atoms should be #t"
                  #t
                  (lat? '(a b c)))
            (test "list with lists inside should not be #t"
                  #f
                  (lat? '(a (b) c))))

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Atom (list-of Atom) -> Bool
;; Produce #t if the atom a exists in lat.

(test-group "`member?':"
            (test "element should be in the list of atoms"
                  #t
                  (member? 'force '(may the force be with you)))
            (test "element should not be in the list"
                  #f
                  (member? 'nix '(windows nah sorry))))

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else

      (or (eq? (car lat) a)
          (member? a (cdr lat)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Atom (list-of Atom) -> (list-of Atom)
;; Produce list of atoms with first occurrence of `a' removed.
;; If `a' doesn't exist in `lat', produce the unmodified list.

(test-group "`rember':"
            (test "element should be removed from beginning of lat"
                  '(y z x)
                  (rember 'x '(x y z x)))
            (test "element should be removed from middle of lat"
                  '(x z)
                  (rember 'y '(x z)))
            (test "element should be removed from end of lat"
                  '(x y)
                  (rember 'z '(x y z))))

(define rember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) a) (cdr lat))
     (else
      (cons (car lat) (rember a (cdr lat)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List -> List
;; Produce list with first element in each sub-list.
;; ASSUME: input list can be empty or contain only
;;         non-empty lists.
(test-group "`firsts':"
            (test "should produce '()"
                  '()
                  (firsts '()))

            (test "should get firsts"
                  '(a c y k)
                  (firsts '((a b) (c) (y z) (k t x)))))

(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     (else
      (cons (car (car l))
            (firsts (cdr l)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insertR ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Atom Atom (list-of Atom)
;; Produce list with 'new added to the right of the first occurence of 'old.

(test-group "`insertR':"
            (test "should add 'jalapeño to the right of 'and"
                  '(tacos tamales and jalapeño salsa)
                  (insertR
                   'jalapeño
                   'and
                   '(tacos tamales and salsa)))

            (test "should add 'e to the right of 'd"
                  '(a b c d e f g h)
                  (insertR 'e 'd '(a b c d f g h))))

(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old)
      (cons (car lat)
            (cons new
                  (cdr lat))))
     (else
      (cons (car lat) (insertR new old (cdr lat)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insertL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Atom Atom (list-of Atom)
;; Produce list with 'new added to the left of the first occurence of 'old.

(test-group "`insertL':"
            (test "should add 'jalapeño to the left of 'and"
                  '(tacos tamales jalapeño and salsa)
                  (insertL
                   'jalapeño
                   'and
                   '(tacos tamales and salsa)))

            (test "should add 'e to the left of 'f"
                  '(a b c d e f g h)
                  (insertL 'e 'f '(a b c d f g h))))
#;
(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old)
      (cons new
            (cons (car lat)
                  (cdr lat))))
     (else
      (cons (car lat) (insertL new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old)
      (cons new lat))
     (else
      (cons (car lat) (insertL new old (cdr lat)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subst ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Atom Atom (list-of Atom)
;; Replaces first occurrence of `old' in `lat' with `new'.

(test-group "`subst':"
            (test "should replace first 'fudge with 'topping"
                  '(ice cream with topping for dessert)
                  (subst
                   'topping
                   'fudge
                   '(ice cream with fudge for dessert))))

(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old)
      (cons new (cdr lat)))
     (else
      (cons (car lat)
            (subst new old (cdr lat)))))))






