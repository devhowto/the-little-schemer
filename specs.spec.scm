
(import test)

(load "defs.scm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapter 1 :: Toys
;; =================


(test-group "atom?"
  (test "a single symbol"
        #t
        (atom? 'yoda))

  (test "an empty list"
        #f
        (atom? '())))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapter 2 :: Do It Again
;; ========================


(test-group "lat?"
  (test "empty list"
        #t
        (lat? '()))
  (test "list with a few atoms"
        #t
        (lat? '(0 "hey" -42 z))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapter 3 :: Cons the Magnificent
;; =================================

(test-group "member?"
  (test "empty list"
        #f
        (member? 'force '()))

  (test "list not containing the atom"
        #f
        (member? 'force '(Help me Obi-Wan Kenobi)))

  (test "list containing the atom"
        #t
        (member? 'force '(The force is strong with this one))))

(test-group "rember"
  (test "empty-list"
        '()
        (rember 'foo '()))

  (test "atom not present in lat"
        '(foo bar qux)
        (rember 'tux '(foo bar qux)))

  (test "atom present multiple times in lat"
        '(foo bar qux tux qux)
        (rember 'qux '(foo qux bar qux tux qux))))


(test-group "rember"
  (test "empty list"
        '()
        (firsts '()))

  (test "list of non-empty lists"
        '(a b c)
        (firsts '((a x)
                  (b y)
                  (c z))))

  (test "list of lists with list car's"
        '((may the) force (always be with) you)
        (firsts '(((may the))
                  (force be)
                  ((always be with) all of)
                  (you)))))

(test-group "insertR"
  (test "single-element list"
        '(hello world)
        (insertR 'world 'hello '(hello)))

  (test "insert to the middle"
        '(tic tac toe)
        (insertR 'tac 'tic '(tic toe)))

  (test "insert to the end"
        '(may the force be with you)
        (insertR 'you 'with '(may the force be with))))

(test-group "insertL"
  (test "single-element list"
        '(hello world)
        (insertL 'hello 'world '(world)))

  (test "insert to the middle"
        '(tic tac toe)
        (insertL 'tac 'toe '(tic toe)))

  (test "insert just before the last element"
        '(may the force be with you)
        (insertL 'with 'you '(may the force be you))))


(test-group "subst"
  (test "single-element list"
        '(scheme)
        (subst 'scheme 'lisp '(lisp)))

  (test "multi-element list"
        '(haskell lisp racket scheme)
        (subst 'racket 'scala '(haskell lisp scala scheme))))


(test-group "subst2"
  (test "single-element list, replace o1"
        '(hello)
        (subst2 'hello 'hey 'hi '(hey)))

  (test "single-element list, replace o2"
        '(hello)
        (subst2 'hello 'hey 'hi '(hi)))

  (test "multi-element list, replace o1"
        '(may the source be with you)
        (subst2 'source
                'force
                'power
                '(may the force be with you)))

  (test "when o1 appears twice, beginning"
        '(NEW foo bar qux THIS)
        (subst2 'NEW
                'THIS
                'THAT
                '(THIS foo bar qux THIS)))

  (test "when o1 appears twice, not in the beginning"
        '(foo bar NEW qux THIS)
        (subst2 'NEW
                'THIS
                'THAT
                '(foo bar THIS qux THIS)))

  (test "when o2 appears and o1 does not"
        '(NEW foo bar THAT qux)
        (subst2 'NEW
                'THIS
                'THAT
                '(THAT foo bar THAT qux)))

  (test "when o1 and o2, replace only o1"
        '(foo NEW bar THAT qux)
        (subst2 'NEW
                'THIS
                'THAT
                '(foo THIS bar THAT qux))))

(test-group "multirember"
  (test "empty list"
        '()
        (multirember 'qux '()))

  (test "single-element list, atom not found"
        '(foo)
        (multirember 'qux '(foo)))

  (test "single-element list, atom found"
        '()
        (multirember 'qux '(qux)))

  (test "list containing the atom multiple times"
        '()
        (multirember 'qux '(qux qux qux qux)))

  (test "list not containing atom"
        '(foo bar baz tux)
        (multirember 'qux '(foo bar baz tux)))

  (test "list containing a few occurrences of the atom"
        '(foo bar tux)
        (multirember 'qux' (qux foo qux bar tux qux))))

(test-group "multiinsertR"
  (test "empty list"
        '()
        (multiinsertR 'NEW 'qux '()))

  (test "single occurrence"
        '(foo bar qux NEW )
        (multiinsertR 'NEW 'qux '(foo bar qux)))

  (test "multiple occurrences"
        '(qux NEW qux NEW foo qux NEW bar qux NEW baz qux NEW)
        (multiinsertR 'NEW 'qux '(qux qux foo qux bar qux baz qux))))

(test-group "multiinsertL"
  (test "empty list"
        '()
        (multiinsertL 'NEW 'qux '()))

  (test "single occurrence"
        '(foo bar NEW qux)
        (multiinsertL 'NEW 'qux '(foo bar qux)))

  (test "multiple occurrences"
        '(NEW qux NEW qux foo NEW qux bar NEW qux baz NEW qux)
        (multiinsertL 'NEW 'qux '(qux qux foo qux bar qux baz qux))))

(test-group "multisubst"
  (test "empty list"
        '()
        (multisubst 'NEW 'qux '()))

  (test "single occurrence"
        '(foo NEW bar)
        (multisubst 'NEW 'qux '(foo qux bar)))

  (test "multiple occurrences"
        '(foo NEW bar NEW baz NEW NEW)
        (multisubst 'NEW 'qux '(foo qux bar qux baz qux qux))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapter 4 :: Number Games
;; =========================

(test-group "o+"
  (test "0 + 0"
        0
        (o+ 0 0))

  (test "0 + 12"
        12
        (o+ 0 12))

  (test "12 + 0"
        12
        (o+ 12 0))

  (test "100 + 200"
        300
        (o+ 100 200)))


(test-group "o-"
  (test "0 - 0"
        0
        (o- 0 0))

  (test "12 - 6"
        6
        (o- 12 6))

  (test "24 - 24"
        0
        (- 24 24)))


(test-group "addtup"
  (test "empty tup"
        0
        (addtup '()))

  (test "single-element tup"
        3
        (addtup '(3)))

  (test "multiple-element tup"
        26
        (addtup '(10 5 7 3 1))))


(test-group "o*"
  (test "1 * 0"
        0
        (o* 1 0))

  (test "0 * 1"
        0
        (o* 0 1))

  (test "5 * 3"
        15
        (o* 5 3)))


(test-group "tup+"
  (test "empty tups"
        '()
        (tup+ '() '()))

  (test "(0) (0)"
        '(0)
        (tup+ '(0) '(0)))

  (test "(1 2 3) (1 2 3)"
        '(2 4 6)
        (tup+ '(1 2 3) '(1 2 3)))

  (test "when tup1 is shorter"
        '(2 4 3)
        (tup+ '(1 2) '(1 2 3)))

  (test "when tup2 is shorter"
        '(2 4 3)
        (tup+ '(1 2 3) '(1 2))))


(test-group "o>"
  (test "0 o> 0"
        #f
        (o> 0 0))

  (test "3 o> 3"
        #f
        (o> 3 3))

  (test "2 o> 3"
        #f
        (o> 2 3))

  (test "3 o> 2"
        #t
        (o> 3 2)))

(test-group "o<"
  (test "0 o< 0"
        #f
        (o< 0 0))

  (test "3 o< 3"
        #f
        (o< 3 3))

  (test "3 o< 2"
        #f
        (o< 3 2))

  (test "2 o< 3"
        #t
        (o< 2 3)))


(test-group "o="
  (test "1 o= 2"
        #f
        (o= 1 2))

  (test "2 o= 1"
        #f
        (o= 2 1))

  (test "3 o= 3"
        #t
        (= 3 3)))


(test-group "o**"
  (test "o** 0 0"
        1
        (o** 0 0))

  (test "o** 1 1"
        1
        (o** 1 1))

  (test "o** 2 3"
        8
        (o** 2 3))

  (test "o** 2 8"
        256
        (o** 2 8))

  (test "o** 5 3"
        125
        (o** 5 3)))


(test-group "o/"
  (test "o/ 1 2"
        0
        (o/ 1 2))

  (test "o/ 2 1"
        2
        (o/ 2 1))

  (test "o/ 15 3"
        5
        (o/ 15 3))

  (test "o/ 17 3"
        5
        (o/ 17 3)))


(test-group "len"
  (test "empty list"
        0
        (len '()))

  (test "single-element list"
        1
        (len '(may)))

  (test "multiple-element list"
        6
        (len '(may the force be with you.))))


(test-group "pick"
  (test "first idx on single-element list"
        'may
        (pick 1 '(may)))

  (test "first idx on multi-element list"
        'may
        (pick 1 '(may the force be with you)))

  (test "middle element"
        'be
        (pick 4 '(may the force be with you)))

  (test "last element"
        'you
        (pick 6 '(may the force be with you))))


(test-group "rempick"
  (test "single-element list"
        '()
        (rempick 1 '(qux)))

  (test "two-element list when n is 1"
        '(bar)
        (rempick 1 '(foo bar)))

  (test "two-element list when n is 2"
        '(foo)
        (rempick 2 '(foo bar)))

  (test "longer list"
        '(the force is strong with this one)
        (rempick 4 '(the force is very strong with this one))))


(test-group "no-nums"
  (test "empty list"
        '()
        (no-nums '()))

  (test "list of nums only"
        '()
        (no-nums '(1 3 5 7 9)))

  (test "list not containing nums"
        '(use the source luke)
        (no-nums '(use the source luke)))

  (test "mixed list"
        '(one two three)
        (no-nums' (1 one 2 two 3 three))))

