(uiop:define-package #:com.andrewsoutar.aoc/2015/day-2
  (:use #:cl #:com.andrewsoutar.aoc/lib)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2015/day-2)
(in-readtable :interpol-syntax)

(defmacro iter-parsed-input ((input l w h) &body body)
  `(iter1
     (do-register-groups (('parse-integer ,l ,w ,h)) (#?/(?m)^(\d*)x(\d*)x(\d*)$/ ,input)
       ,@body)))

(defun part1 (input)
  (iter-parsed-input (input l w h)
    (let ((areas (iter1
                   (map-combinations
                    (lambda (x) (collect (apply '* x)))
                    (list l w h) :length 2))))
      (sum (+ (* 2 (apply '+ areas)) (apply 'min areas))))))

(defun part2 (input)
  (iter-parsed-input (input l w h)
    (sum (+ (* 2 (apply '+ (subseq (sort (list l w h) '<) 0 2)))
            (* l w h)))))
