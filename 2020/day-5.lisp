(uiop:define-package #:com.andrewsoutar.aoc/2020/day-5
  (:use #:cl #:com.andrewsoutar.aoc/lib)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2020/day-5)
(in-readtable aoc)

(defmacro iter-passes ((var input) &body body)
  (with-unique-names (pass)
    `(iter (for ,pass in (split #?/\n/ ,input))
       (let ((,var (parse-integer (map 'string {case %1 ((#\F #\L) #\0) ((#\B #\R) #\1)} ,pass)
                                  :radix 2)))
         ,@body))))

(defun part1 (input)
  (iter-passes (num input)
    (maximizing num)))

(defun part2 (input)
  (let ((nums (sort (iter-passes (num input)
                      (collecting num))
                    '<)))
    (iter
      (for x in nums)
      (for y from (elt nums 0))
      (finding y such-that (/= x y)))))
