(uiop:define-package #:com.andrewsoutar.aoc/2020/day-6
  (:use #:cl #:com.andrewsoutar.aoc/lib)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2020/day-6)
(in-readtable aoc)

(defun sum-reductions (input fun)
  (iter (for lines in (split #?/\n\n/ input))
    (sum (length (iter (for line in (split #?/\n/ lines))
                   (reducing (coerce line 'list) by fun))))))

(defun part1 (input)
  (sum-reductions input 'nunion))

(defun part2 (input)
  (sum-reductions input 'nintersection))
