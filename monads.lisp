;;;; monads.lisp

(in-package #:monads)

(defmacro freeze (form) `(lambda () ,form))

(defun thaw (frozen) (funcall frozen))

;; macro interface for library users
(defmacro >>= (<i> m k)
  `(mbind ,<i> ,m ,k))

(defmacro >> (<i> m k) 
  `(mseq ,<i> ,m (freeze ,k)))

(defmacro ret (<i> x)
  `(mreturn ,<i> ,x))


;; generic method for Monad implementors

(defgeneric mbind (<i> m k))

(defgeneric mseq (<i> m fk))

(defgeneric mreturn (<i> v))

(defgeneric mfail (<i> x))


;; <list> monad implementation

(defvar <list> '<list>)

(defmethod mbind ((<i> (eql <list>)) m k)
  (reduce #'(lambda (ma a)
              (append ma (funcall k a))) m :initial-value '()))

(defmethod mseq ((<i> (eql <list>)) m fk)
  (reduce #'(lambda (ma a)
              (append ma (thaw fk))) m :initial-value '()))

(defmethod mreturn ((<i> (eql <list>)) x) (list x))

(defmethod mfail ((<i> (eql <list>)) x) '())



(defmacro is (expected test)
  `(let ((result ,test)
         (expected ,expected))
     (if (equalp result expected)
       (format t "~a~%Pass: ~a~%"
               (quote ,test) result)
       (format t "~a~%Fail: got ~a expected ~a~%"
               (quote ,test) result expected))))

(defun test ()
  (is (list 2 3 4)
      (>>= <list> '(1 2 3) (lambda (x) (ret <list> (+ 1 x)))))
  (is (list nil nil nil)
      (>> <list> '(1 2 3) (ret <list> (format t "foo"))))
  (is '((1 A) (1 B) (1 C) (2 A) (2 B) (2 C) (3 A) (3 B) (3 C))
      (>>= <list> '(1 2 3) 
           (lambda (x)
             (>>= <list> '(a b c)
                  (lambda (y)
                    (ret <list> (list x y)))))))
  )

