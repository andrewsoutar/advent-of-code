(uiop:define-package #:com.andrewsoutar.aoc/lib
  (:use #:cl #:com.andrewsoutar.brace-lambda #:named-readtables)
  (:use-reexport #:com.andrewsoutar.aoc)
  (:use-reexport #:alexandria #:cl-ppcre #:cl-interpol #:iterate)
  (:export #:in-readtable #:aoc #:iter1 #:gcase #:scase #:flatarr))
(in-package #:com.andrewsoutar.aoc/lib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defreadtable aoc
    (:merge :standard)
    (:merge brace-lambda)
    (:merge :interpol-syntax)))
(in-readtable aoc)

;;; HACK to let me use the COUNT function inside ITER forms
(remprop 'count 'iterate::synonym)

(defmacro iter1 (&body body)
  `(iter ,@body (finish)))

(defmacro gcase ((test keyform) &body cases)
  (once-only (test keyform)
    `(cond ,@(iter (for (key . value) in cases)
               (collect
                   `(,(case key
                        ((t :otherwise) 't)
                        (t `(or ,@(mapcar {`(funcall ,test ,keyform ,%1)}
                                          (ensure-list key)))))
                     ,@value))))))
(defmacro scase (keyform &body cases)
  `(gcase ('string= ,keyform) ,@cases))

(defun flatarr (a)
  (make-array (array-total-size a) :displaced-to a))
