(import test)

(load "defs.scm")

;;
;; Chapter 1
;;

(test-group "atom?"
  (test "a single symbol"
        #t
        (atom? 'yoda))

  (test "an empty list"
        #f
        (atom? '())))

;;
;; Chapter 2
;;

(test-group "lat?"
  (test "empty list"
        #t
        (lat? '()))
  (test "list with a few atoms"
        #t
        (lat? '(0 "hey" -42 z))))


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

