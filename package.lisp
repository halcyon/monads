;;;; package.lisp

(defpackage #:monads
  (:use #:cl)
  (:export #:>>=
           #:>>
           #:ret
           #:mdo
           #:mbind
           #:mseq
           #:mreturn
           #:mfail
           #:test))

