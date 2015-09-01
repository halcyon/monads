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

           #:test

           #:<list>
        
           #:<state>
           #:get-state
           #:get-state-f
           #:put-state
           #:run-state
           #:exec-state
           #:eval-state
           ))

