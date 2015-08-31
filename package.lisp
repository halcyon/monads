;;;; package.lisp

(defpackage #:monads
  (:use #:cl)
  (:export #:>>=
           #:>>
           #:unit
           #:mdo
           #:mbind
           #:mseq
           #:munit
           #:mfail
           #:test))

