(uiop:define-package #:com.andrewsoutar.aoc/2015/day-5
  (:use #:cl #:com.andrewsoutar.aoc/lib)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2015/day-5)
(in-readtable aoc)

(defun part1 (input)
  (iter (for line in (split #?/\n/ input))
    (counting
     (and (>= (count-if {find %1 "aeiou"} line) 3)
          (scan #?/(.)\1/ line)
          (not (scan #?r"ab|cd|pq|xy" line))))))

(defun part2 (input)
  (iter (for line in (split #?/\n/ input))
    (counting
     (and (scan #?/(..).*\1/ line)
          (scan #?/(.).\1/ line)))))
