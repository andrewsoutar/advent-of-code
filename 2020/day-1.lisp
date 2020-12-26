(uiop:define-package #:com.andrewsoutar.aoc/2020/day-1
  (:use #:cl #:com.andrewsoutar.aoc/lib)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2020/day-1)
(in-readtable aoc)

(defun find-2020-out-of (n input)
  (iter1
    (map-combinations {finding (apply '* %1) such-that (= 2020 (apply '+ %1))}
                      (mapcar 'parse-integer (split #?/\n/ input)) :length n)))

(defun part1 (input)
  (find-2020-out-of 2 input))

(defun part2 (input)
  (find-2020-out-of 3 input))
