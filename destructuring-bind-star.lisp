;;;; destructuring-bind-star.lisp

(uiop:define-package #:destructuring-bind-star
  (:use #:cl)
  (:export #:destructuring-bind*
           #:destructuring-error
           #:lambda-list
           #:expression
           #:reason))

(in-package #:destructuring-bind-star)

(defun print-destructuring-error (condition stream)
  (format stream "Failed to destructure ~S~:[~; against ~:*~S~]~:[.~;:~%~:*~A~]"
          (expression condition) (lambda-list condition) (reason condition)))

(define-condition destructuring-error (error)
  ((%expression :reader expression
                :initarg :expression
                :initform (error "Must provide EXPRESSION."))
   (%lambda-list :reader lambda-list
                 :initarg :lambda-list
                 :initform '())
   (%reason :reader reason
            :initarg :reason
            :initform nil))
  (:documentation "Condition type responsible for destructuring errors signaled
in DESTRUCTURING-BIND*.")
  (:report print-destructuring-error))

(defun bind-modify-lambda-list (lambda-list gensym)
  (flet ((wrap (elt)
           `(,(car elt)
             (prog2 (setf ,gensym t) ,(cadr elt) (setf ,gensym nil))
             ,@(cddr elt))))
    (loop with modifyp = nil
          for thing on lambda-list
          for elt = (car thing)
          if (or (atom elt) (= 1 (length elt)))
            collect elt into result
          else collect (wrap elt) into result
          if (and (member elt lambda-list-keywords)
                  (member elt '(&key &optional)))
            do (setf modifyp t)
          else if (member elt lambda-list-keywords)
                 do (setf modifyp nil)
          unless (listp (cdr thing))
            do (return (nconc result (cdr thing)))
          finally (return result))))

(defun extract-declarations (body)
  (loop for form = (car body)
        while body
        if (and (listp form) (eql (first form) 'declare))
          collect form into declarations and do (pop body)
        else do (loop-finish)
        finally (return (values declarations body))))

(defmacro destructuring-bind* (lambda-list expression &body body)
  "Like DESTRUCTURING-BIND, except destructuring errors are signaled as
instances of condition type DESTRUCTURING-ERROR."
  (let ((in-body (gensym "IN-BODY")) (handle (gensym "HANDLE"))
        (condition (gensym "CONDITION")) (value (gensym "VALUE")))
    (multiple-value-bind (declarations body) (extract-declarations body)
      (let ((new-lambda-list (bind-modify-lambda-list lambda-list in-body)))
        `(let (,in-body)
           (flet ((,handle (,condition)
                    (unless ,in-body
                      (error (make-condition 'destructuring-error
                                             :lambda-list ',lambda-list
                                             :expression ',expression
                                             :reason ,condition)))))
             (declare (ignorable #',handle))
             (let ((,value ,expression))
               (handler-bind ((error #',handle))
                 (destructuring-bind ,new-lambda-list ,value
                   ,@declarations
                   (setf ,in-body t)
                   (locally ,@body))))))))))
