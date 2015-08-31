;;;; monads.lisp

(in-package #:monads)

(defmacro freeze (form) `(lambda () ,form))

(defun thaw (frozen) (funcall frozen))

;; macro interface for library users
(defmacro >>= (<i> m k)
  `(mbind ,<i> ,m ,k))

(defmacro >> (<i> m k) 
  `(mseq ,<i> ,m (freeze ,k)))

(defmacro unit (<i> x)
  `(munit ,<i> ,x))

(defmacro mdo (<i> actions final)
  (assert (listp actions))
  (if actions
    (let* ((action (first actions))
           (l (length action)))
      (assert (member l '(1 2)))
      (if (eql l 2)
        (let ((bind-label (first action))
              (bind-action (second action)))
          (assert (symbolp bind-label))
          `(>>= ,<i> ,bind-action (lambda (,bind-label) (mdo ,<i> ,(rest actions) ,final))))
        `(>> ,<i> ,action (mdo ,<i> ,(rest actions) ,final))))
    `(unit ,<i> ,final)))

;; generic method for Monad implementors

(defgeneric mbind (<i> m k))

(defgeneric mseq (<i> m fk))

(defgeneric munit (<i> v))

(defgeneric mfail (<i> x))

;; default implementations

(defmethod mseq (<i> m fk)
  (mbind <i> m (lambda (x)
                 (declare (ignore x))
                 (thaw fk))))

;; <list> monad implementation

(defvar <list> '<list>)

(defmethod mbind ((<i> (eql <list>)) m k)
  (reduce #'(lambda (ma a)
              (append ma (funcall k a))) m :initial-value '()))

(defmethod munit ((<i> (eql <list>)) x) (list x))

(defmethod mfail ((<i> (eql <list>)) x) '())

;; Some tests.

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
      (>>= <list> '(1 2 3) (lambda (x) (unit <list> (+ 1 x)))))
  (is (list nil nil nil)
      (>> <list> '(1 2 3) (unit <list> (format t "foo"))))
  (is '((1 a) (1 b) (1 c) (2 a) (2 b) (2 c) (3 a) (3 b) (3 c))
      (>>= <list> '(1 2 3) 
           (lambda (x)
             (>>= <list> '(a b c)
                  (lambda (y)
                    (unit <list> (list x y)))))))
  (is '((1 a) (1 b) (1 c) (2 a) (2 b) (2 c) (3 a) (3 b) (3 c))
      (mdo <list>
        ((x '(1 2 3))
         (y '(a b c)))
        (list x y)))
  )


