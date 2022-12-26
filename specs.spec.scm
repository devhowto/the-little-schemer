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

