(uiop:define-package #:com.andrewsoutar.aoc/2015/day-8
  (:use #:cl #:com.andrewsoutar.aoc/lib)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2015/day-8)
(in-readtable aoc)

(defvar *constituent-regex* #?r"(?:[^\\\"]|\\(?:\"|\\|x[0-9a-f]{2}))")

(defmacro do-strings ((whole-string-var &optional contents-var) input &body body)
  `(do-register-groups (,whole-string-var ,contents-var)
       (#?|("(${*constituent-regex*}*)")| ,input)
     ,@body))

(defun part1 (input)
  (iter1
    (do-strings (whole-string contents) input
      (sum
       (- (length whole-string)
          (iter1
            (do-register-groups () (*constituent-regex* contents)
              (counting t))))))))

(defun part2 (input)
  (iter1
    (do-strings (whole-string) input
      (sum (- (+ 2 (reduce '+ whole-string :key {case %1 ((#\\ #\") 2) (t 1)}))
              (length whole-string))))))
