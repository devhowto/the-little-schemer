(import test)

;;;;
;; Checks whether `x` is an atom.
;;
;; The default Chicken Scheme `atom?` `#t` for `'()`, but the book
;; wants the empty list not to be considered an atom. Therefore, we
;; override it with the `atom?` implementation from the book.
;;
;; atom? :: Value -> Bool
;;
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


(test-group "atom?"
  (test "a single symbol"
        #t
        (atom? 'yoda))

  (test "an empty list"
        #f
        (atom? '())))


