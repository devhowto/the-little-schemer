
;;;;
;; atom? :: Value -> Bool
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
;; Chapter 1 :: Toys
;; =================
;;



