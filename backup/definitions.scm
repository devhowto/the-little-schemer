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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `tup+' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::tup+[]
;; Tuple Tuple -> Tuple
;; Produce tuple with sums of each pair of `tup1' and `tup2'.
;; ASSUME: Both tuples have the same length.

(define tup+
  (lambda (tup1 tup2)
    (cond
     ((and (null? tup1) (null? tup2))
      '())
     (else
      (cons
       (o+ (car tup1)
          (car tup2))
          (tup+ (cdr tup1)
                (cdr tup2)))))))

(test-group "`tup+'"
 (test
  "should produce '() since tuples are empty"
  '()
  (tup+ '() '()))
 (test
  "should sum the tuple pairs"
  '(11 11 11 11 11)
  (tup+ '(3 6 9 11 4)
        '(8 5 2  0 7))))
;; end::tup+[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `tup+v2' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::tup+v2[]
;; Tuple Tuple -> Tuple
;; Produce tuple with sum of pair of elements from `tup1' and `tup2'.
;; The length of the tuples don't have to be the same.

(define tup+v2
  (lambda (tup1 tup2)
    (cond
     ;; We can simplify and remove this check.
     ;;((and (null? tup1) (null? tup2)) '())
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else
      (cons (o+ (car tup1)
                (car tup2))
            (tup+v2 (cdr tup1)
                    (cdr tup2)))))))

(test-group
 "`tup+v2'"
 (test "should add empty tuples"
       '()
       (tup+v2 '() '()))
 (test "should add tuples of same length"
       '(11 3 7)
       (tup+v2 '(4 2 1) '(7 1 6)))
 (test "should add with `tup1' shorter than `tup2'"
       '(7 12 4 9)
       (tup+v2 '(2 3) '(5 9 4 9)))
 (test "should add with `tup2' shorter than `tup1'"
       '(7 12 4 9)
       (tup+v2 '(5 9 4 9) '(2 3))))
;; end::tup+v2[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `o>' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::o>[]
;; Number Number -> Bool
;; Produce `#t' if `n' is greater than `m'; `#f' otherwise.

(define o>
  (lambda (n m)
    (cond
     ;; <1>
     ((zero? n) #f)
     ((zero? m) #t)
     (else
      (o> (sub1 n) (sub1 m))))))

(test-group "`o>'"
            (test "should be greater" #t (o> 13 9))
            (test "should not be greater" #f (o> 13 14))
            (test "should not be greater because it is equal to"
                  #f
                  (o> 3 3)))
;; end::o>[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; o< ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::o<[]
;; Number Number -> Bool
;; Produce `#t' if `n' is less than `m'; `#f' otherwise.

(define o<
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else
      (o< (sub1 n) (sub1 m))))))

(test-group "`o<'"
            (test "should be less than" #t (o< 9 13))
            (test "should not be less than" #f (o< 9 7))
            (test "should not be less than because it is equal to"
                  #f
                  (o< 3 3)))
;; end::o<[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; o= ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::o=[]
;; Number Number -> Bool
;; Produce `#t' if `n' and `m' are equal; `#f' otherwise.

(define o=
  (lambda (n m)
    (cond
     ((zero? m) (zero? n))
     ((zero? n) #f)
     (else
      (o= (sub1 n) (sub1 m))))))

;; Just another implementation using `o>' and `o<'.
(define o==
  (lambda (n m)
    (cond
     ((o> n m) #f)
     ((o< n m) #f)
     (else #t))))

(test-group
 "`o='"
 (test "should be equal" #t (o= 3 3))
 (test "should not be equal, first is less than the second"
       #f
       (o= 3 5))
 (test "should not be equal, first is greater than the second"
       #f
       (o= 5 3)))

(test-group
 "`o=='"
 (test "should be equal" #t (o== 3 3))
 (test "should not be equal, first is less than the second"
       #f
       (o== 3 5))
 (test "should not be equal, first is greater than the second"
       #f
       (o== 5 3)))
;; end::o=[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; o** ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::o**[]
;; Number Number -> Number
;; Produce the result of `n' to the `m'th power.

(define o**
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else
      (o* n (o** n (sub1 m)))))))

(test-group "`o**'"
            (test "should calculate 2 to the 3rd power"
                  27
                  (o** 3 3))
            (test "should calculate 2 to the 5th power"
                  32
                  (o** 2 5)))
;; end::o**[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; o/ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::o/[]
;; Number Number -> Number
;; Counts how many times `m' fits into `n'. It does division.

(define o/
  (lambda (n m)
    (cond
     ((o< n m) 0)
     (else
      (add1 (o/ (o- n m) m))))))

(test-group "`o/'"
            (test "should divide 15 by 4"
                  3
                  (o/ 15 4)))
;; NOTE: division by zero doesn't work. You have been warned.
;; end::o/[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; len ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::len[]
;; (list-of Atom) -> Number
;; Produce the length of `lat'.

(define len
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else
      (add1 (len (cdr lat)))))))

(test-group
 "`len'"
 (test "should produce 0 for empty lat"
       0
       (len '()))
 (test "should count length of non-empty lists"
       6
       (len '(may the force be with you))))
;; end::len[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pick ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::pick[]
;; Number (list-of Atom) -> Atom
;; Produce atom in `n'th position in `lat'.
;; ASSUME: `n' is a valid index in `lat'.

(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

(test-group
 "`pick'"
 (test "should return first element"
       'may
       (pick 1 '(may the source)))
 (test "should return from the middle of list"
       'be
       (pick 4 '(may the source be with you))))
;; end::pick[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rempick ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::rempick[]
;; Number (list-of Atom) -> Atom
;; Produce `lat' with with element in `n'th position removed.
;; ASSUME: `n' is a valid position in `lat'.

(define rempick
  (lambda (n lat)
    (cond
     ((null? lat) '())
     ((zero? (sub1 n))
      (rempick (sub1 n) (cdr lat)))
     (else
      (cons (car lat)
            (rempick (sub1 n) (cdr lat)))))))

(test-group
 "`rempick'"
 (test "should remove first element"
       '(potatoes are hot)
       (rempick 1 '(hot potatoes are hot)))
 (test "should remove element in the middle of lat"
       '(hotdogs with mustard)
       (rempick 3 '(hotdogs with hot mustard))))
;; end::rempick[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; no-nums ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::no-nums[]
;; (list-of Atom) -> (list-of Atom)
;; Produce `lat' with numeric atoms removed from the list.

(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((number? (car lat))
      (no-nums (cdr lat)))
     (else
      (cons (car lat)
            (no-nums (cdr lat)))))))

(test-group
 "`no-nums'"
 (test "should remove numbers"
       '(pears prunes dates)
       (no-nums '(5 pears 6 prunes 9 dates)))
 (test "should produce unmodified lat"
       '(the force is strong with this one)
       (no-nums '(the force is strong with this one))))
;; end::no-nums[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; all-nums ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::all-nums[]
;; (list-of Atom) -> Tuple.
;; Extract all numbers from `lat'.

(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((number? (car lat))
      (cons (car lat)
            (all-nums (cdr lat))))
     (else
      (all-nums (cdr lat))))))

(test-group
 "`all-nums'"
 (test "should produce non-empty tuple"
       '(9 13 22)
       (all-nums '(9 wizards with 13 students and 22 wands)))
 (test "should produce empty list/tuple"
       '()
       (all-nums '(sorry but no nums in this list))))
;; end::all-nums[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eqan? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::eqan?[]
;; Atom Atom -> Bool
;; Produce `#t' if `a1' and `a2' are the same atom; `#f' otherwise.

;; My version. Something might be wrong.
;; (define eqan?
;;   (lambda (a1 a2)
;;     (cond
;;      ((and (number? a1) (number? a2) (= a1 a2)) #t)
;;      ((eq? a1 a2) #t)
;;      (else #f))))

;; Version from the book.
(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2)) (= a1 a2))
     ((or (number? a1) (number? a2)) #f)
     (else (eq? a1 a2)))))

(test-group
 "`eqan?'"
 (test "same two non-numeric atoms should be equal"
       #t
       (eqan? 'yoda (quote yoda)))
 (test "same two numeric atoms should be equal"
       #t
       (eqan? 13 13))
 (test "differnt two non-numeric atoms should not be equal"
       #f
       (eqan? 'yoda 'vader))
 (test "different two numeric atoms should not be equal"
       #f
       (eqan? 13 9)))
;; end::eqan?[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; occur ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::occur[]
;; (list-of Atom) -> Number
;; Count the number of times a given atom `a' appears in `lat'.

(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     ((eqan? (car lat) a)
      (add1 (occur a (cdr lat))))
     (else (occur a (cdr lat))))))

(test-group
 "`occur'"
 (test "given atom appear should occur a few times in lat"
       2
       (occur 'tea '(tea cup full of tea)))
 (test "given numeric atom appears a few times in lat"
       3
       (occur 5 '(5 foo 32 and 5 7 when 5 is 2)))
 (test "given atom should not be found and counted on lat"
       0
       (occur 'win32 '(my favorite OS is *nix-based))))
;; end::occur[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; one? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::one?[]
;; Number -> Bool
;; Produce `#t' if `n' is 1 and `#f' otherwise.

(define one?
  (lambda (n)
    (eqan? n 1)))

(test-group
 "`one?'"
 (test "should be one" #t (one? 1))
 (test "should not be one" #f (one? 0)))
;; end::one?[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rempick-v2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::rempick-v2[]
;; Remove element at `n'th position in `lat'.
;; ASSUME: `n' is a valid index in `lat'.

(define rempick-v2
  (lambda (n lat)
    (cond
     ((one? n) (cdr lat))
     (else
      (cons (car lat) (rempick-v2 (sub1 n) (cdr lat)))))))

(test-group
 "`rempick-v2'"
 (test "should remove from the beginning of lat"
       '(meringue salty pie)
       (rempick-v2 1 '(lemon meringue salty pie)))
 (test "should remove from the middle of lat"
       '(lemon meringue pie)
       (rempick-v2 3 '(lemon meringue salty pie))))
;; end::rempick-v2[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rember* ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::rember*[]
;; Atom List -> List
;; Produce list with all `a's removed from the list.

(define rember*
  (lambda (a l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) a) (rember* a (cdr l)))
       (else (cons (car l) (rember* a (cdr l))))))
     (else (cons (rember* a (car l))
                 (rember* a (cdr l)))))))

(test-group
 "`rember*'"
 (test "should remove from inner lists"
       '((coffee) ((tea)) (and (hick)))
       (rember* 'cup '((coffee) cup ((tea) cup) (and (hick) cup))))
 (test "should produce unmodified input list"

       '((coffee) ((tea) and hick))
       (rember* 'cup '((coffee) ((tea) and hick)))))
;; end::rember*[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insertR* ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::insertR*[]
;; Atom Atom List -> List
;; Insert `new' to the right of every `old' in the list.

(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
        (cons (car l) (cons new (insertR* new old (cdr l)))))
       (else
        (cons (car l) (insertR* new old (cdr l))))))
     (else (cons (insertR* new old (car l))
                 (insertR* new old (cdr l)))))))

(test-group
 "`insertR*'"
 (test "should insert to the right, even on inner lists"
       '(x z (a x z b) (((w x z))))
       (insertR* 'z 'x '(x (a x b) (((w x))))))
 (test "should produce unmodified input"
       '(a b ((p)) k (((y z))))
       (insertR* 'x 'x '(a b ((p)) k (((y z)))))))
;; end::insertR*[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; occur* ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::occur*[]
;; Atom List -> Number
;; Count how many times `a' appear in `l'.

(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l))
      (cond
       ((eq? (car l) a)
        (add1 (occur* a (cdr l))))
       (else (occur* a (cdr l)))))
     ;; Note `+' here!
     (else (+ (occur* a (car l)) ; <1>
              (occur* a (cdr l)))))))

(test-group
 "`occur*'"
 (test "should occurs a few times"
       4
       (occur* 'z '(a z (((z k z))) y (z))))
 (test "should not occur even once"
       0
       (occur* 'z '(a b (((x k y))) p (t)))))
;; end::occur*[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subst* ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::subst*[]
;; Atom Atom List -> List
;; Prodce list with all occurrences of `old' replaced with `new'.

(define subst*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
        (cons new (subst* new old (cdr l))))
       (else (cons (car l) (subst* new old (cdr l))))))
     (else (cons (subst* new old (car l)) ; <1>
                 (subst* new old (cdr l)))))))

(test-group
 "`subst*'"
 (test "should replace all occurrences, even in inner lists"
       '(n y (k n t) (((y p n))))
       (subst* 'n 'i '(i y (k i t) (((y p i))))))
 (test "should produce unmodified input"
       '(k (((p x)) (b)))
       (subst* 'n 'i '(k (((p x)) (b))))))
;; end::subst*[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insertL* ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::insertL*[]

(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
        (cons new (cons old (insertL* new old (cdr l)))))
       (else (cons (car l) (insertL* new old (cdr l))))))
     (else (cons (insertL* new old (car l))
                 (insertL* new old (cdr l)))))))

(test-group
 "`insertL*'"
 (test "should insert to the left of atom, globally"
       '(k x y (((p x y (x y z))) k x y))
       (insertL* 'x 'y '(k y (((p y (y z))) k y))))
 (test "should produce unmodified input"
       '(k y (((p y (y z))) k y))
       (insertL* 'a 'b '(k y (((p y (y z))) k y)))))
;; end::insertL*[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; member* ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::member*[]
;; Atom List -> Bool
;; Produce `#t' if `a' appears on `l', `#f' otherwise.

(define member*
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l))
      (or (eq? (car l) a)
          (member* a (cdr l))))
     (else
      (or (member* a (car l))
          (member* a (cdr l)))))))

(test-group
 "`member*'"
 (test "atom should appear somewhere in the list"
       #t
       (member* 'force '(may ((the (((force)))) be (with)) you)))
 (test "atom should not appear in the list"
       #f
       (member* 'vader '(may ((the (((force)))) be (with)) you))))
;; end::member*[]



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; leftmost ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::leftmost[]
;; List -> Atom
;; Produce the leftmost atom in `l'.
;; ASSUME: `l' is not empty and does not contain the empty list.

(define leftmost
  (lambda (l)
    (cond ; <1>
     ((atom? (car l)) (car l))
     (else (leftmost (car l))))))

(test-group
 "`leftmost'"
 (test "should produce the leftmost atom not inside nested lists"
       'may
       (leftmost '(may the (force) be (with) you)))
 (test "should produce the leftmost atom inside nested lists"
       'may
       (leftmost '(((((((may)))))) the (force) be (with) you))))
;; end::leftmost[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eqlist? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::eqlist?[]
;; List List -> Bool
;; Produce `#t' if both are equal; `#f' otherwise.

;; My solution.
(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t) ; <1>
     ;; If both `l1' and `l2' above were `null?', we would
     ;; not even get to this `or' question. So, we are sure
     ;; that at least one of them is not null. So, if one is
     ;; sure not null, but one happens to be null, them the
     ;; lists are not equal.
     ((or (null? l1) (null? l2)) #f) ; <2>
     ((atom? (car l1))
      (and (atom? (car l2))
           (eqan? (car l1) (car l2))
           (eqlist? (cdr l1) (cdr l2))))
     (else (and (eqlist? (car l1) (car l2))
                (eqlist? (cdr l1) (cdr l2)))))))

;; Book's solution.
;(define eqlist?
;  (lambda (l1 l2)
;    (cond
;     ((and (null? l1) (null? l2)) #t)
;     ((or (null? l1) (null? l2)) #f)
;     ((and (atom? (car l1)) (atom? (car l2)))
;      (and (eqan? (car l1) (car l2))
;           (eqlist? (cdr l1) (cdr l2))))
;     ((or (atom? (car l1)) (atom? (car l2))) #f)
;     (else
;      (and (eqlist? (car l1) (car l2))
;           (eqlist? (cdr l1) (cdr l2)))))))

(test-group
 "`eqlist?'"
 (test "first empty second not empty should not be equal"
       #f
       (eqlist? '() '((f))))
 (test "first not empty second empty should not be equal"
       #f
       (eqlist? '(((g))) '()))
 (test "lists should not be equal"
       #f
       (eqlist? '(beef ((sausage)) (and (soda)))
                '(beef ((salami)) (and (soda)))))
 (test "lists should be equal"
       #t
       (eqlist? '(beef ((sausage)) (and (soda)))
                '(beef ((sausage)) (and (soda))))))
;; end::eqlist?[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my-equal? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::my-equal?[]
;; S-Expr S-Expr -> Bool
;; Produce `#t' if both S-Expressions are equal, and `#f' otherwise.

(define my-equal?
  (lambda (se1 se2)
    (cond
     ((and (atom? se1) (atom? se2))
      (eqan? se1 se2))
     ((or (atom? se1) (atom? se2)) #f)
     (else (eqlist? se1 se2)))))

(test-group
 "`my-equal?'"
 (test "two same atoms should be equal"
       #t
       (my-equal? 'y 'y))
 (test "two same empty lists should be equal"
       #t
       (my-equal? '() '()))
 (test "two same list nested in lists should be equal"
       #t
       (my-equal? '(a ((b (c)))) '(a ((b (c))))))
 (test "two different atoms should not be equal"
       #f
       (my-equal? 'x 'y))
 (test "two different lists should not be equal"
       #f
       (my-equal? '(a b) '(a ((b))))))
;; end::my-equal?[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eqlist? (v2) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::eqlist?-v2[]
;; List List -> Bool
;; Produce `#t' if both lists are equal, and `#f' otherwise.

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else (and (equal? (car l1) (car l2))
                (equal? (cdr l1) (cdr l2)))))))

(test-group
 "`eqlist?'"
 (test "first empty second not empty should not be equal"
       #f
       (eqlist? '() '((f))))
 (test "first not empty second empty should not be equal"
       #f
       (eqlist? '(((g))) '()))
 (test "lists should not be equal"
       #f
       (eqlist? '(beef ((sausage)) (and (soda)))
                '(beef ((salami)) (and (soda)))))
 (test "lists should be equal"
       #t
       (eqlist? '(beef ((sausage)) (and (soda)))
                '(beef ((sausage)) (and (soda))))))
;; end::eqlist?-v2[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rember (v2) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::rember-v2[]
;; S-Exp (list-of S-Exp) -> (list-of S-Exp)
;; Produce list with sexp removed from the input list.
;; NOTE: It doesn't work with sexps inside lists. It can only `rember'
;;       sexps if they are not further nested inside other lists.

(define rember2
  (lambda (s l)
    (cond
     ((null? l) '())
     ((my-equal? (car l) s) (cdr l))
     (else (cons (car l) (rember2 s (cdr l)))))))

(test-group
 "rember (v2)"
 (test "should remove from lat"
       '(a c)
       (rember2 'b '(a b c)))
 (test "should remove from list"
       '((a) (c))
       (rember2 '(b) '((a) (b) (c))))

 ;; Should not remove `(a)' in `((a) b)'. This version of
 ;; `rember' simply doesn't work for this sort of stuff.
 (test "should not remove from nested lists"
       '(x y ((a) b) k (((l))))
       (rember2 '(a) '(x y ((a) b) k (((l)))))))
;; end::rember-v2[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; numbered? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::numbered?[]
;; S-Exp -> Bool
;; Produces `#t' if the representation of the arithmetic expression contains
;; only numbers besides `+`, `*` and `↑`.
;; ASSUME: input is always in the form `number` or `operand1 operand operator2'.

(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     (else
      (and (numbered? (car aexp))
           (numbered? (car (cdr (cdr aexp)))))))))

(test-group
 "`numbered?'"
 (test "should not be a valid arith. expr."
       #f
       (numbered? '(a b c)))
 (test "should be a valid arith. expr."
       #t
       (numbered? '(1 + 2)))
 (test "should also be valid"
       #t
       (numbered? '(1 ↑ 2 * 3)))
 (test "should not be valid because non-numeric operand"
       #f
       (numbered? '(1 * x)))
 (test "only numbers is `#t' as per the book's solution of `numbered?'"
       #t
       (numbered? '(1 2 3))))
;; end::numbered?[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; value ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::value-v1[]
;; QuotedList -> Number
;; Produce the value of the input expression.

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (car (cdr nexp)) '+)
      (+ (value (car nexp))
         (value (car (cdr (cdr nexp))))))
     ((eq? (car (cdr nexp)) '*)
      (* (value (car nexp))
         (value (car (cdr (cdr nexp))))))
     ((eq? (car (cdr nexp)) '↑)
      (o** (value (car nexp))
           (value (car (cdr (cdr nexp)))))))))

(test-group
 "`value' (v1)"
 (test "should add numbers (simple)"
       11
       (value '(7 + 4)))
 (test "should add numbers (nested)"
       7
       (value '(2 + (4 + 1))))
 (test "should multiply numbers"
       9
       (value '(3 * (3 + (5 * 0)))))
 (test "should raise the the power of"
       32
       (value '(2 ↑ (5 ↑ 1)))))
;; end::value-v1[]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; value ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::value-v2[]
;; QuotedList -> Number
;; Produce the value of the input expression.

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (car nexp) '+)
      (+ (value (car (cdr nexp)))
         (value (car (cdr (cdr nexp))))))
     ((eq? (car nexp) '*)
      (* (value (car (cdr nexp)))
         (value (car (cdr (cdr nexp))))))
     ((eq? (car nexp) '↑)
      (o** (value (car (cdr nexp)))
           (value (car (cdr (cdr nexp)))))))))

(test-group
 "`value' (v2)"
 (test "should add numbers (simple)"
       11
       (value '(+ 7 4)))
 (test "should add numbers (nested)"
       7
       (value '(+ 2 (+ 4 1))))
 (test "should multiply numbers"
       9
       (value '(* 3 (+ 3 (* 5 0)))))
 (test "should raise the the power of"
       32
       (value '(↑ 2 (↑ 5 1)))))
;; end::value-v2[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1st-sub-exp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::1st-sub-exp[]
;; QuotedList -> Exp
;; Produce first sub-expression from input quoted list.

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(test-group
 "`1st-sub-exp'"
 (test "should produce 1st sub expression"
       3
       (1st-sub-exp '(+ 3 4))))
;; end::1st-sub-exp[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2nd-sub-exp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::2nd-sub-exp[]
;; QuotedList -> Exp
;; Produce second sub expression in input quoted list.

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(test-group
 "`2nd-sub-exp'"
 (test "should produce 2nd sub expression"
       4
       (2nd-sub-exp '(+ 3 4))))
;; end::2nd-sub-exp[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operator ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::operator[]
(define operator
  (lambda (aexp)
    (car aexp)))

(test-group
 "`operator'"
 (test "should produce the operator"
       '+
       (operator '(+ 3 4))))
;; end::operator[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; value (v3) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::value-v3[]
;; QuotedList -> Number
;; Produce the value of the input expression.

(define value
  (lambda (aexp)
    (cond
     ((atom? aexp) aexp)
     ((eq? (operator aexp) '+)
      (+ (value (1st-sub-exp aexp))
         (value (2nd-sub-exp aexp))))
     ((eq? (operator aexp) '*)
      (* (value (1st-sub-exp aexp))
         (value (2nd-sub-exp aexp))))
     ((eq? (operator aexp) '↑)
      (o** (value (1st-sub-exp aexp))
         (value (2nd-sub-exp aexp)))))))

(test-group
 "`value' (v3)"
 (test "should add numbers (simple)"
       11
       (value '(+ 7 4)))
 (test "should add numbers (nested)"
       7
       (value '(+ 2 (+ 4 1))))
 (test "should multiply numbers"
       9
       (value '(* 3 (+ 3 (* 5 0)))))
 (test "should raise the the power of"
       32
       (value '(↑ 2 (↑ 5 1)))))
;; end::value-v3[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::our-lnums1[]
;; LNum means our representation of number using lists where () is zero,
;; (()) is one, (() ()) is two, etc.

;; LNum -> Bool
;; Produce `#t' if `n' is zero.
(define my-zero?
  (lambda (n)
    (null? n)))

;; LNum -> LNum
;; Adds one to `n'.
(define my-add1
  (lambda (n)
    (cons '() n)))

;; LNum -> LNum
;; Subtracts one from `n`.
;; ASSUME: `n' is not zero (or we get an error). According to the law
;;         `cdr', we won't ask for the `cdr` of an empty list (which
;;         is our zero in this representation of numbers.
(define my-sub1
  (lambda (n)
    (cdr n)))

(define my-o+
  (lambda (n m)
    (cond
     ((my-zero? m) n)
     (else (my-o+ (my-add1 n)
                  (my-sub1 m))))))

(test-group
 "`my-o+'"
 (test "should add using our representation of numbers"
       '(() () () ())
       (my-o+ '(()) '(() () ()))))
;; tag::our-lnums1[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::set?[]
;; (list-of Atom) -> Bool
;; Produce `#t' if lat is a set (no atom appears more than once), and
;; `#f' otherwise.

(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((member? (car lat) (cdr lat)) #f)
     (else (set? (cdr lat))))))

(test-group
 "`set?'"
 (test "empty list should be a set"
       #t
       (set? '()))
 (test "lat with duplicates atoms should not be a set"
       #f
       (set? '(foo bar foo jedi)))
 (test "lat with no duplicate atoms should be a set"
       #t
       (set? '(may the force be with you)))
 (test "lat with numbers should be valid sets too if no duplicates appear"
       #t
       (set? '(one 2 three 4 5))))
;; end::set?[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; makeset ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::makeset[]
;; (list-of Atom) -> Set
;; Produce a valid set out of a list of atoms.
;; NOTE: The order of the members in the produced set is not the same
;;       as the order they appear in lat because `makeset' discards
;;       members from the beginning of input `lat', not from the end.

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((member? (car lat) (cdr lat))
      (makeset (cdr lat)))
     (else (cons (car lat) (makeset (cdr lat)))))))

(test-group
 "`makeset'"
 (test "should produce empty set"
       '() ;; resulting set
       (makeset '())) ;; input lat
 (test "should produce set with one member out of lat with one member"
       '(x) ;; resulting set
       (makeset '(x))) ;; input lat
 (test "should produce set with one member out of many duplicates"
       '(x) ;; resulting set
       (makeset '(x x x))) ;; input lat
 (test "should produce set with a few members"
       ;; Note the output is ordered different than the input `lat'.
       '(c b d a) ;; resulting set
       (makeset '(a b c b d a)))) ;; input lat
;; end::makeset[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; makeset-v2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::makeset-v2[]
;; (list-of Atom) -> Set
;; Produce a valid set from input list of atoms.
;; NOTE: this version should produce resulting in the leaving the
;;       members in the order they appear in input `lat'.

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else
      (cons (car lat)
            (makeset (multirember (car lat) (cdr lat))))))))

(test-group
 "`makeset'"
 (test "should produce empty set"
       '() ;; resulting set
       (makeset '())) ;; input lat
 (test "should produce set with one member out of lat with one member"
       '(x) ;; resulting set
       (makeset '(x))) ;; input lat
 (test "should produce set with one member out of many duplicates"
       '(x) ;; resulting set
       (makeset '(x x x))) ;; input lat
 (test "should produce set with a few members"
       ;; Note the output order of members now match the input.
       '(a b c d) ;; resulting set
       (makeset '(a b c b d a))))
;; end::makeset-v2[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subset? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::subset?[]
;; Set Set -> Bool
;; Produce `#t' if all elements in `set1' appear in `set2', and `#f' otherwise.

;; Works, but let's use the version with `and' below.
; (define subset?
;   (lambda (set1 set2)
;     (cond
;      ((null? set1) #t)
;      ((member? (car set1) set2)
;       (subset? (cdr set1) set2))
;      (else #f))))

(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else
      (and (member? (car set1) set2)
           (subset? (cdr set1) set2))))))

(test-group
 "`subset?'"
 (test "empty set1 should be a subset of empty set2"
       #t
       (subset? '() '()))
 (test "empty set1 should be a subset of set2"
       #t
       (subset? '() '(x y z)))
 (test "non-empty set1 should not be a subset of empty set2"
       #f
       (subset? '(x) '()))
 (test "set1 should be a subset of set2"
       #t
       (subset? '(may force you)
                '(may the force be with you)))
 (test "set1 should not be a subset of set2"
       #f
       (subset? '(may force me)
                '(may the force be with you))))
;; end::subset?[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eqset? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::eqset?[]
;; Set Set -> Bool
;; Produce `#t' if both sets have the same members, and `#f' otherwise.
;; ASSUME: Both sets have the same length.

;; Works.
(define eqset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else
      (and (member? (car set1) set2)
           (eqset? (cdr set1) set2))))))

;; A bit more intersting because it makes use of `subset?'.
(define eqset?
  (lambda (set1 set2)
    (cond
     ((subset? set1 set2)
      (subset? set2 set1))
     (else #f))))

;; The most concise and elegant one. Beautiful.
(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(test-group
 "`eqset?'"
 (test "two empty sets are equal?"
       #t
       (eqset? '() '()))
 (test "two sets with members in the same order should be equal"
       #t
       (eqset? '(x y z) '(x y z)))
 (test "two sets with same members but in different order should also be equal"
       #t
       (eqset? '(y z x) '(z x y)))
 (test "two sets with different members should not be equal"
       #f
       (eqset? '(x y a) '(x y z))))
;; end::eqset?[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; intersect? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::intersect?[]
;; Set Set -> Bool
;; Produce `#t' if both sets have any common member.

;; This version works fine.
(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     ((member? (car set1) set2) #t)
     (else
      (intersect? (cdr set1) set2)))))

;; This looks better... moves `member?' to the else question and uses
;; `or' instead of returning an explicit `#t'.
(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     (else
      (or (member? (car set1) set2)
          (intersect? (cdr set1) set2))))))

(test-group
 "`intersect?'"
 (test "sets should not intersect"
       #f
       (intersect? '(a b c) '(d e f)))
 (test "sets should intersect"
       #t
       (intersect? '(a b c) '(c d e f))))
;; end::intersect?[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; intersect ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::intersect[]
;; Set Set -> Set
;; Produce set with intersecting members from `set1' and `set2'.

(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2)
      (cons (car set1)
            (intersect (cdr set1) set2)))
     (else
      (intersect (cdr set1) set2)))))

(test-group
 "`intersect'"
 (test "should produce empty set because there are no intersections"
       '()
       (intersect '(a b c) '(d e f)))
 (test "should produce set from intersecting members"
       '(b c)
       (intersect '(a b c) '(b c d))))
;; end::intersect[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; union ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::union[]
;; Set Set -> Set
;; Produce the union set from `set1' and `set2'.

(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2)
      (union (cdr set1) set2))
     (else
      (cons (car set1)
            (union (cdr set1) set2))))))

(test-group
 "`union'"
 (test "should produce empty set"
       '()
       (union '() '()))
 (test "should produce union set with some members"
       '(stewed tomatoes casserole macaroni and cheese)
       (union
        '(stewed tomatoes and macaroni casserole)
        '(macaroni and cheese))))
;; end::union[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; difference ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::difference[]
;; Set Set -> Set
;; Produce all atoms in `set1' that are not in `set2'.

(define difference
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2)
      (difference (cdr set1) set2))
     (else
      (cons (car set1)
            (difference (cdr set1) set2))))))

(test-group
 "`difference'"
 (test "should produce empty set from empty input sets"
       '()
       (difference '() '()))
 (test "should produce empty set since there is no difference"
       '()
       (difference '(a b c) '(a k b x d c y z)))
 (test "should produce set with some members arising from the diffrence"
       '(a c)
       (difference '(a b c d) '(b d))))
;; end::difference[]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; intersectall ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::intersectall[]
;; (list-of Set) -> Set
;; Produce set with intersecting elements from input list of sets.
;; members in `set1'.
;; ASSUME: list of sets is not empty.

(define intersectall
  (lambda (l-set)
    (cond
     ((null? (cdr l-set)) (car l-set))
     (else
      (intersect (car l-set)
                 (intersectall (cdr l-set)))))))

(test-group
 "`intersectall'"
 ;; Since there is only one set in the list, there is nothing
 ;; else that it _does not_ intersect with, so, the members
 ;; on this set are considered to intersect.
 (test "should produce first set"
       '(m h z)
       ;; Only 'a appear in all three sets in the list.
       (intersectall '((m h z))))
 (test "should produce set with one member"
       '(a)
       ;; Only `a' appear in all three sets in the list.
       ;; Some other elements appear in two sets, but not in
       ;; all three of them.
       (intersectall '((a b c) (c a d e) (e f g h a b))))
 (test "should produce set with several members"
       ;; `the' and `force' appears in all sets
       '(the force)
       (intersectall '((the force)
                       (the force is strong with this one)
                       (may the force be with you)
                       (see the force)
                       (feel the force)
                       (is force the only thing?)
                       (be the force)))))
;; end::intersectall[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a-pair? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::a-pair?[]
;; List -> Bool
;; Produce `#t' if `l' is a pair, and `#f' otherwise.

(define a-pair?
  (lambda (x)
    (cond
     ((atom? x) #f)
     ((null? x) #f)
     ((null? (cdr x)) #f)
     ((null? (cdr (cdr x))) #t)
     (else #f))))

(test-group
 "`a-pair?'"
 (test "should not be a pair because it is a single atom"
       #f
       (a-pair? 'tomb-raider))
 (test "should not be a pair because it is a list with a single atom"
       #f
       (a-pair? 'lara-croft))
 (test "should be a pair of to atoms"
       #t
       (a-pair? '(lara croft)))
 (test "should be a pair of two numeric atoms"
       #t
       (a-pair? '(3 -9)))
 (test "should be a pair of two more complex sexps"
       #t
       (a-pair? '((tomb) (raider)))))
;; end::a-pair?[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; first ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::first[]
;; List -> S-Exp
;; Produce first sexp from input list.
;; ASSUME: Input list is not empty.

(define first
  (lambda (p)
    (car p)))

(test-group
 "`first'"
 (test "should retrieve first element of the list"
       'may
       (first '(may the force be))))
;; end::first[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; second ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::second[]
;; List -> S-Expr
;; Produce the second element of the list.
;; ASSUME: Input list contains at least two elements.

(define second
  (lambda (p)
    (car (cdr p))))

(test-group
 "`second'"
 (test "should produce second element of the list"
       'force
       (second '(the force is strong with this one))))
;; end::second[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; third ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::third[]
;; List -> S-Exp
;; Produce the third element from the input list.
;; ASSUME: The list is at least three elements long.

(define third
  (lambda (p)
    (car (cdr (cdr p)))))

(test-group
 "`third'"
 (test "should produce the third element"
       'moon
       (third '(to the moon is an awesome game))))
;; end::third[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; build ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::build[]
;; S-Exp S-Exp -> Pair
;; Produce a pair from its two input sexps.

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(test-group
 "`build'"
 (test "should produce a pair of two atoms"
       '(x y)
       (build 'x 'y))
 (test "should produce a pair of two more complex sexps"
       '((feel) (the (force)))
       (build '(feel) '(the (force)))))
;; end::build[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fun? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::fun?[]
;; (list-of Pair) -> Bool
;; Produce `#t' if the firsts in `rel' produce a Set, and `#f' otherwise.

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(test-group
 "`fun?'"
 (test "should not be a function"
       #f
       ;; A appears twice as the car of some pairs.
       (fun? '((a b) (b c) (a d) (x y))))
 (test "should be a function"
       #t
       (fun? '((a b) (b c) (c a) (x y)))))
;; end::fun?[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; revrel ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::revrel[]
;; Relation -> Relation
;; Produce relation with pair elements swapped.

(define revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else
      (cons (build (second (car rel))
                   (first (car rel)))
            (revrel (cdr rel)))))))

(test-group
 "`revrel'"
 (test "should revert a relation"
       '((a 8) (pie pumpkin) (sick got))
       (revrel '((8 a) (pumpkin pie) (got sick)))))
;; end::revrel[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; revpair  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::revpair[]
;; Pair -> Pair
;; Swaps the elements of a pair.

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(test-group
 "`revpair'"
 (test "should reverse a pair"
       '(yoda master)
       (revpair '(master yoda))))
;; end::revpair[]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; revrel (v2) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::revrel-v2[]
;; Relation -> Relation
;; Produce relation with pair elements swapped.

(define revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else
      (cons (revpair (car rel))
            (revrel (cdr rel)))))))

(test-group
 "`revrel'"
 (test "should revert a relation"
       '((a 8) (pie pumpkin) (sick got))
       (revrel '((8 a) (pumpkin pie) (got sick)))))
;; end::revrel-v2[]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `seconds' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::seconds[]
;; List -> List
;; Produce list with composed by the second element in each sub-list.
;; ASSUME: input list can be empty or contain only non-empty lists.
;; ASSUME: sub-lists always have at least two elements.

(define seconds
  (lambda (l)
    (cond
     ((null? l) '())
     ;; This should never come to pass because of the second ASSUME.
     ((null? (cdr (car l)))
      (cons '() (seconds (cdr l))))
     (else
      (cons (car (cdr (car l)))
            (seconds (cdr l)))))))

(test-group "`seconds':"
            (test "should produce '()"
                  '()
                  (seconds '()))
            (test "should get seconds"
                  '(b d z t)
                  (seconds '((a b) (c d) (y z) (k t x)))))
;; end::seconds[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fullfun? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::fullfun?[]
;; (list-of Pair) -> Bool
;; Produce `#t' if both `firsts' and `seconds' produce a Set, and `#f' otherwise.

(define fullfun?
  (lambda (fun)
    (cond
     ((not (fun? fun)) #f)
     (else
      (set? (seconds fun))))))

(test-group
 "`fullfun?`"
 (test "should not be a fullfun, 2 appears twice as second"
       #f
       (fullfun? '((8 3) (4 2) (7 6) (6 2) (3 4))))
 (test "should be a fullfun, no second appears more than once"
       #t
       (fullfun? '((8 3) (4 2) (7 6) (6 9) (3 4)))))
;; end::fullfun?[]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; one-to-one? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::one-to-one?[]
;; (list-of Pair) -> Bool
;; Produce `#t' if both `firsts' and `seconds' produce a Set, and `#f' otherwise.

(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))

(test-group
 "`one-to-one?`"
 (test "should not be a one-to-one, 2 appears twice as second"
       #f
       (one-to-one? '((8 3) (4 2) (7 6) (6 2) (3 4))))
 (test "should be a one-to-one, no second appears more than once"
       #t
       (one-to-one? '((8 3) (4 2) (7 6) (6 9) (3 4)))))
;; end::one-to-one?[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rember-f ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::rember-f[]
;; Predicate S-Exp (list-of S-Exp) -> (list-of S-Exp)
;; Produce list with the first occurrenct of `s' removed.

(define rember-f
  (lambda (pred? s l)
    (cond
     ((null? l) '())
     ((pred? s (car l)) (cdr l))
     (else
      (cons (car l)
            (rember-f pred? s (cdr l)))))))

(test-group
 "`rember-f'"
 (test "should remove the number"
       '(6 2 3)
       (rember-f = '5 '(6 5 2 3)))
 (test "should remove the atom"
       '(beans are good)
       (rember-f eq? 'jelly '(jelly beans are good)))
 (test "should remove the sexp"
       '(lemonade and (cake))
       (rember-f equal?
                 '(pop corn)
                 '(lemonade (pop corn) and (cake)))))
;; end::rember-f[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eq?-c ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::eq?-c[]
;; Atom -> (Procedure Atom) -> Bool
;; Takes 'a and produces a procedure that takes 'x and compares
;; that 'x with 'a, producing a bool.

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(test-group
 "`eq?-c'"
 (test
  "should return a procedure"
  #t
  (procedure? (eq?-c 'foo)))
 (test
  "should return the expected procedure that compares with 'foo"
  #t
  ((eq?-c 'foo) 'foo))
 (test
  "should compare with 'foo, not with 'bar"
  #f
  ((eq?-c 'foo) 'bar)))
;; end::eq?-c[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rember-f (v2, currying) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::rember-f-v2-currying[]
;; (Predicate?) -> (Atom List) -> List
;; Takes a predicate and produces a function that takes `a' and `l' and
;; produces a list with the first occurence of `a' removed from the list.

(define rember-f
  (lambda (pred?)
    (lambda (s l)
      (cond
       ((null? l) '())
       ((pred? s (car l)) (cdr l))
       (else
        (cons (car l)
              ((rember-f pred?) s (cdr l))))))))

(test-group
 "`rember-f' (v2, currying)"
 (test
  "should produce a procedure"
  #t
  (procedure? (rember-f eq?)))
 (test
  "should remove member"
  '(a b c x d)
  ((rember-f eq?) 'x '(a b x c x d)))
 (test
  "should still remove the member"
  '(equal? eqan? eqlist? eqpair?)
  ((rember-f eq?) 'eq? '(equal? eqan? eq? eqlist? eqpair?)))
 (test
  "should remove the list as well"
  '((a b) (e f))
  ((rember-f eqlist?) '(c d) '((a b) (c d) (e f)))))
;; end::rember-f-v2-currying[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `insertL-f' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::insertL-f[]
;; (Predicate?) -> (Atom Atom (list-of Atom)
;; Takes a predicate and produces a function which producees
;; list with `new' added to the left of the first occurence of `old'.

(define insertL-f
  (lambda (pred?)
    (lambda (new old lat)
      (cond
       ((null? lat) '())
       ((pred? (car lat) old)
        (cons new lat))
       (else
        (cons (car lat) ((insertL-f pred?) new old (cdr lat))))))))

(test-group "`insertL-f':"
            (test "should add 'jalapeno to the left of 'and"
                  '(tacos tamales jalapeno and salsa)
                  ((insertL-f eq?)
                     'jalapeno
                     'and
                     '(tacos tamales and salsa)))

            (test "should add 'e to the left of 'f"
                  '(a b c d e f g h)
                  ((insertL-f eq?) 'e 'f '(a b c d f g h)))

            (test "should add '(e f) to the left of '(g h)"
                  '((a b) (c d) (e f) (g h))
                  ((insertL-f eqlist?)
                   '(e f)
                   '(g h)
                   '((a b) (c d) (g h)))))
;; end::insertL-f[]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `insertR-f' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::insertR-f[]
;; (Predicate?) -> Atom Atom (list-of Atom) -> (list-of Atom)
;; Produces a function that produces list with `new' added to
;; the right of the first occurence of `old'.

(define insertR-f
  (lambda (pred?)
    (lambda (new old lat)
      (cond
       ((null? lat) '())
       ((pred? (car lat) old)
        (cons (car lat) (cons new (cdr lat))))
       (else
        (cons (car lat)
              ((insertR-f pred?) new old (cdr lat))))))))

(test-group "`insertR-f':"
            (test "should add 'jalapeño to the right of 'and"
                  '(tacos tamales and jalapeno salsa)
                  ((insertR-f eq?)
                   'jalapeno
                   'and
                   '(tacos tamales and salsa)))

            (test "should add 'e to the right of 'd"
                  '(a b c d e f g h)
                  ((insertR-f eq?) 'e 'd '(a b c d f g h)))

            (test "should add '(e f) to the right of '(c d)"
                  '((a b) (c d) (e f) (g h))
                  ((insertR-f eqlist?)
                   '(e f)
                   '(c d)
                   '((a b) (c d) (g h)))))
;; end::insertR-f[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; seqL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::seqL[]
;; S-Exp S-Exp S-Exp -> (list-of S-Exp)
;; Produce list with `new' inserted to the left of `old'.

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(test-group
 "`seqL'"
 (test "should insert `b' to the left of `c'"
       '(b c d e)
       (seqL 'b 'c '(d e))))
;; end::seqL[]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; seqR ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::seqR[]
;; S-Exp S-Exp S-Exp -> (list-of S-Exp)
;; Produce list with `new' inserted to the right of `old'.

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(test-group
 "`seqR'"
 (test "should insert `c' to the right of `b'"
       '(b c d e)
       (seqR 'c 'b '(d e))))
;; end::seqR[]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert-g ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::insert-g[]
;; Function Atom Atom (list-of Atom) -> (list-of Atom)
;; Produce list with `new' inserted either the left, right, or in place
;; of `old' according to the function it receives.

;; `insert-g' is actually a function which produces a function that
;; is just like `insertL' or `insertR', depending on the `seq'
;; function used.

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((eq? (car l) old)
        (seq new old (cdr l)))
       (else
        (cons (car l)
              ((insert-g seq) new old (cdr l))))))))

(test-group
 "`insert-g':"
 (test "should add 'jalapeno to the left of 'and"
       '(tacos tamales jalapeno and salsa)
       ((insert-g seqL)
        'jalapeno
        'and
        '(tacos tamales and salsa)))

 (test "should add 'f to the right of 'e"
       '(a b c d)
       ((insert-g seqR) 'c 'b '(a b d))))

;;
;; And now we could redefine `insertL' and `insertR' making use
;; of `insert-g', `seqL' and `seqR'.
;;
(define insertL (insert-g seqL))
(define insertR (insert-g seqR))

;;
;; We could also not use `seqL' and `sedR' and use just
;; `insert-g' to define `insertL' and `insertR'.
;;
(define insertL
  (insert-g
   (lambda (new old l)
     (cons new (cons old l)))))

(define insertR
  (insert-g
   (lambda (new old l)
     (cons old (cons new l)))))
;; end::insert-g[]



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; seqS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::seqS[]
;; S-Exp S-Exp S-Exp -> (list-of S-Exp)
;; Produce list with `old' replaced with `new'.

(define seqS
  (lambda (new old l)
    (cons new l)))

(test-group
 "`seqS'"
 (test "should replace x with c"
       '(c d e f)
       (seqS 'c 'x '(d e f))))

;; Now we can redefine `subst' to use `insert-g' and `seqS'.
(define subst (insert-g seqS))
;; Use it just like the old subst:
;;   (subst 'c 'x '(a b x d e f))
;; should produce '(a b c d e f).
;; end::seqS[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; atom-to-function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::atom-to-fn[]
;; Atom -> Function
;; Produce operator as function.
;; ASSUME: 'x is a valid operator.
(define atom-to-fn
  (lambda (x)
    (cond
     ((eq? x (quote +)) +)
     ((eq? x (quote *)) *)
     (else o**))))

(atom-to-fn (operator '(+ 5 3)))
;; Produces the procedure `+', not the atom `+'.

;; Now we can rewrite `value' using `atom-to-fn' to make `value'
;; shorter and simpler.

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     (else
      ((atom-to-fn (operator nexp))
       (value (1st-sub-exp nexp))
       (value (2nd-sub-exp nexp)))))))

(value '(+ 2 3)) ; 5
;; end::atom-to-fn[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multirember-f ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::multirember-f[]
;; Predicate Atom (list-of Atom) -> (list-of Atom)
;; Produce list with all occurrences of `a' removed from `lat'
;; using `pred?' to decide what should be removed.

(define multirember-f
  (lambda (pred? a lat)
    (cond
     ((null? lat) '())
     ((pred? (car lat) a)
      (multirember-f pred? a (cdr lat)))
     (else
      (cons (car lat) (multirember-f pred? a (cdr lat)))))))

(test-group
 "`multirember-f'"
 (test "should remove all `a's in `lat'"
       '(a b c d e f)
       (multirember-f eq? 'x '(x a b c d x e f x)))
 (test "should leave `lat' untouched"
       '(x y z)
       (multirember-f eq?'k '(x y z))))
;; end::multirember-f[]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multirember-eq? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::multirember-eq?[]
;; Atom (list-of Atom) -> (list-of Atom)

(define multirember-eq?
  (lambda (a l)
    (multirember-f eq? a l)))

(test-group
 "`multirember-eq?'"
 (test "should remove all `a's in `lat'"
       '(a b c d e f)
       (multirember-eq? 'x '(x a b c d x e f x)))
 (test "should leave `lat' untouched"
       '(x y z)
       (multirember-eq?'k '(x y z))))
;; end::multirember-eq?[]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiremberT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag::multiremberT[]
;; Predicate List -> List
;; Produce new list with all elements matched by the predicate function
;; removed from the original list.

(define multiremberT
  (lambda (pred? lat)
    (cond
     ((null? lat) '())
     ((pred? (car lat))
      (multiremberT pred? (cdr lat)))
     (else
      (cons (car lat)
            (multiremberT pred? (cdr lat)))))))

(define eq?-x
  (eq?-c 'x))

(test-group
 "`multiremberT'"
 (test"should remove all 'x from input list"
      '(a b c d e f)
      (multiremberT
       eq?-x
       '(a b x c d x e f))))
;; end::multiremberT[]

