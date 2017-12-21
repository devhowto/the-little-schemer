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

