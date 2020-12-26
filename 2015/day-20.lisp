(uiop:define-package #:com.andrewsoutar.aoc/2015/day-20
  (:use #:cl #:com.andrewsoutar.aoc/lib)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2015/day-20)
(in-readtable aoc)

(defun part1 (input)
  (reduce 'lcm (iota (1+ (ceiling (/ (+ -1 (sqrt (1+ (* 8/10 (parse-integer input))))) 2)))) :start 1))

(defun part1-other-way (input)
  (iter (with limit = (parse-integer input))
    (for i from 1)
    (summing (* i 10) into presents)
    (reducing i by #'lcm)
    (until (>= presents limit))))
