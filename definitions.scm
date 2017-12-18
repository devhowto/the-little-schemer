(require-extension test) ; chicken-install -s test

;;
;; The tag::foo[]/end::foo[] stuff are used in the asciidoctor document.
;;
;; For predicate functions that end in `?' (lat?, member?), the
;; tag includes a `-p`, like `lat-p' and `member-p'.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `atom?` ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::atom-p[]
;; Expr -> Bool
;; Takes an expression and produce #t if it is an atom; #f otherwise.
;;
;; NOTE:
;; Chicken has its own implementation of atom?. It produces
;; `#t` for '(). This version (from the book) produces #f for '().

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(test-group "`atom?':"
            (test "is 'yoda an atom? Yes, it is."
                  #t
                  (atom? 'yoda))
            (test "'(foo) should not be an atom"
                  #f
                  (atom? '(foo))))
;; end::atom-p[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `lat?' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::lat-p[]
;; (list-of Atom) -> Bool
;; Produce `#t' if `l' is a list of atoms, `#f' otherwise.

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

(test-group "`lat?'"
            (test "list with only atoms should be #t"
                  #t
                  (lat? '(a b c)))
            (test "list with lists inside should not be #t"
                  #f
                  (lat? '(a (b) c))))
;; end::lat-p[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `member?' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::member-p[]
;; Atom (list-of Atom) -> Bool
;; Produce `#t' if the atom a exists in lat, `#f' otherwise.

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else

      (or (eq? (car lat) a)
          (member? a (cdr lat)))))))

(test-group "`member?':"
            (test "element should be in the list of atoms"
                  #t
                  (member? 'force '(may the force be with you)))
            (test "element should not be in the list"
                  #f
                  (member? 'nix '(windows nah sorry))))
;; end::member-p[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `rember' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::rember[]
;; Atom (list-of Atom) -> (list-of Atom)
;; Produce list of atoms with first occurrence of `a' removed.
;; If `a' doesn't exist in `lat', produce the unmodified list.

(define rember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) a) (cdr lat))
     (else
      ;; <1>
      (cons (car lat) (rember a (cdr lat)))))))

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
;; end::rember[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `firsts' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::firsts[]
;; List -> List
;; Produce list with composed by the first element in each sub-list.
;; ASSUME: input list can be empty or contain only
;;         non-empty lists.

(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     (else
      (cons (car (car l))
            (firsts (cdr l)))))))

(test-group "`firsts':"
            (test "should produce '()"
                  '()
                  (firsts '()))

            (test "should get firsts"
                  '(a c y k)
                  (firsts '((a b) (c) (y z) (k t x)))))
;; end::firsts[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `insertR' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::insertR[]
;; Atom Atom (list-of Atom)
;; Produce list with `new' added to the right of the first occurence of `old'.

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
;; end::insertR[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `insertL' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::insertL[]
;; Atom Atom (list-of Atom)
;; Produce list with `new' added to the left of the first occurence of `old'.

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old)
      (cons new lat))
     (else
      (cons (car lat) (insertL new old (cdr lat)))))))

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
;; end::insertL[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `subst` ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::subst[]
;; Atom Atom (list-of Atom)
;; Replaces first occurrence of `old' in `lat' with `new'.

(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old)
      (cons new (cdr lat)))
     (else
      (cons (car lat)
            (subst new old (cdr lat)))))))

(test-group "`subst':"
            (test "should replace first 'fudge with 'topping"
                  '(ice cream with topping for dessert)
                  (subst
                   'topping
                   'fudge
                   '(ice cream with fudge for dessert))))
;; end::subst[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `subst2` ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::subst2[]
;; Atom Atom Atom (list-of Atom) -> (list-of Atom)
;; Replaces either the first occurrenct of `o1' or `o2' by `new'.

(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) '())
     ((or (eq? (car lat) o1)
          (eq? (car lat) o2))
      (cons new (cdr lat)))
     (else (cons (car lat)
                 (subst2 new o1 o2 (cdr lat)))))))

(test-group "`subst2':"
            (test "should replace in the beginning of the list"
                  '(vanilla icecream with chocolate topping)
                  (subst2
                   'vanilla
                   'chocolate
                   'banana
                   '(banana icecream with chocolate topping)))
            (test "should replace in the middle of the list"
                  '(banana icecream with no topping)
                  (subst2
                   'no
                   'strawberry
                   'chocolate
                   '(banana icecream with chocolate topping))))
;; end::subst2[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `multirember' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::multirember[]
;; Atom (list-of Atom) -> (list-of Atom)
;; Produce list with all occurrences of `a' removed from `lat'.

(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) a)
      (multirember a (cdr lat)))
     (else
      (cons (car lat) (multirember a (cdr lat)))))))

(test-group "`multirember'"
            (test "should remove all `a's in `lat'"
                  '(a b c d e f)
                  (multirember 'x '(x a b c d x e f x)))
            (test "should leave `lat' untouched"
                  '(x y z)
                  (multirember 'k '(x y z))))
;; end::multirember[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `multiinsertR' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::multiinsertR[]
;; Atom Atom (list-of Atom) -> (list-of Atom)
;; Produce `lat' with `new' inserted to the right of all
;; occurrences of `old'.

(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old)
      (cons (car lat)
            (cons new
                  (multiinsertR new old (cdr lat)))))
     (else
      (cons (car lat)
            (multiinsertR new old (cdr lat)))))))

(test-group "`multiinsertR'"
            (test "should insert `x' to the right of all `z's"
                  '(z x b z x k y z x)
                  (multiinsertR 'x 'z '(z b z k y z)))
            (test "should leave `lat' untouched, `z' doesn't exist in `lat'"
                  '(k b c y)
                  (multiinsertR 'x 'z '(k b c y))))
;; end::multiinsertR[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `multiinsertL' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::multiinsertL[]
;; Atom Atom (list-of Atom) -> (list-of Atom)
;; Produce `lat' with `new' inserted to the left of all occurrences of `old'.

(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old)
      (cons new
            (cons (car lat)
                  (multiinsertL new old (cdr lat)))))
     (else
      (cons (car lat)
            (multiinsertL new old (cdr lat)))))))

(test-group "`multiinsertL'"
            (test "should insert `x' to the left of all `z's"
                  '(x z b x z k y x z)
                  (multiinsertL 'x 'z '(z b z k y z)))
            (test "should produce the input lits unmodified"
                  '(k l y)
                  (multiinsertL 'x 'z '(k l y))))
;; end::multiinsertL[]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `multisubst' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::multisubst[]
;; Atom Atom (list-of Atom) -> (list-of Atom)
;; Produce list with all occurrences of `new' removed.

(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old)
      (cons new (multisubst new old (cdr lat))))
     (else
      (cons (car lat) (multisubst new old (cdr lat)))))))

(test-group "`multisubst'"
            (test "should replace all occurrences"
                  '(my os is linux and theirs is linux too)
                  (multisubst
                   'linux
                   'win32
                   '(my os is win32 and theirs is win32 too)))
            (test "should not touch original `lat'"
                  '(my os is BSD and theirs is BSD too)
                  (multisubst
                   'linux
                   'win32
                   '(my os is BSD and theirs is BSD too))))
;; end::multisubst[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add1, sub1, zero?
;; These functions exist the chicken scheme implementation.  Adding
;; them here for the sake of completenes as far as the book is concerned.

;; (define add1
;;   (lambda (n)
;;     (+ n 1)))
;;
;; (define sub1
;;   (lambda (n)
;;     (- n 1)))
;;
;; (define zero?
;;   (lambda (n)
;;     (= n 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `o+' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::o+[]
;; Natural Natural -> Natural
;; Add two numbers together.

;; Using `o+' instead of simply `+' in order not to override
;; native implementation of `+'.

(define o+
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else
      (add1 (o+ n (sub1 m)))))))

(test-group "`o+'"
            (test "should add 2 and 5"
                  7
                  (o+ 2 5))
            (test "should add 0 and 13"
                  13
                  (o+ 0 13)))
;; end::o+[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `o-' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::o-[]
;; Natural Natural -> Natural
;; Subtract `m' from `n'.

;; Naming it `o-' instead of simply `-' in order not to override Chicken's
;; own implementation of `-'.

(define o-
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else
      (sub1 (o- n (sub1 m)))))))

(test-group "`o-'"
            (test "should subtract 3 from 14"
                  11
                  (o- 14 3))
            (test "should subtract 0 from 3"
                  3
                  (o- 3 0)))
;; end::o-[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `addtup' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::addtup[]
;; (list-of Tuple) -> Natural
;; Adds/sums all the numbers in the tuple.

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else
      (o+ (car tup) (addtup (cdr tup)))))))

(test-group "`addtup'"
            (test "should add '(1 2 3 4) and produce 10"
                  10
                  (addtup '(1 2 3 4)))
            (test "should produce zero"
                  0
                  (addtup '())))
;; end::addtup[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `o*' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::o*[]
;; Number Number -> Number
;; Multiply `n' by `m' (adds `n' up `m' times).

(define o*
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else
      (o+ n (o* n (sub1 m)))))))

(test-group "`o*'"
            (test "should multiply 3 by 5"
                  15
                  (o* 3 5))
            (test "should multiply 11 by 0"
                  0
                  (o* 11 0)))
;; end::o*[]


