;;;; test.lisp

(in-package #:destructuring-bind-star)

(defvar *tests* '())

(defun test ()
  (dolist (test *tests*)
    (funcall test)
    (princ ".")))

(defmacro define-test (name &body body)
  `(progn (defun ,name ()
            (flet ((fail () (error "Test failure in ~A" ',name))
                   (pass () (return-from ,name)))
              (declare (ignorable #'fail #'pass))
              ,@body))
          (pushnew ',name *tests*)))

(define-test test-1
  (handler-case (destructuring-bind* () '())
    (error () (fail))))

(define-test test-2
  (handler-case (destructuring-bind* (a) '(2 4)
                  (declare (ignore a)))
    (destructuring-error () (pass))
    ((and error (not destructuring-error)) () (fail))))

(define-test test-3
  (handler-case (destructuring-bind* (&optional (a (error 'error))) '()
                  (declare (ignore a)))
    (destructuring-error () (fail))
    ((and error (not destructuring-error)) () (pass))))

(define-test test-4
  (handler-case (destructuring-bind* (&key (a (error 'error))) '()
                  (declare (ignore a)))
    (destructuring-error () (fail))
    ((and error (not destructuring-error)) () (pass))))

(define-test test-5
  (handler-case (destructuring-bind* () '()
                  (error 'error))
    (destructuring-error () (fail))
    ((and error (not destructuring-error)) () (pass))))

(define-test test-6
  (handler-case (destructuring-bind* (a . b) '()
                  (declare (ignore a b)))
    (error () (fail))))
