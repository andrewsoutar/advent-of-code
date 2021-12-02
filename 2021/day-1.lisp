(uiop:define-package #:com.andrewsoutar.aoc/2021/day-1
  (:use #:cl #:com.andrewsoutar.aoc/lib)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2021/day-1)
(in-readtable aoc)

(defun parse-input (input)
  (iter1
    (do-register-groups ((#'parse-integer num)) (#?/(\d+)\n/ input)
      (collect num))))

(defun part1 (input)
  (iter
    (for num in (parse-input input))
    (for last-num previous num)
    (counting (and last-num (> num last-num)))))

(defun part2 (input)
  (iter
    (for rest on (parse-input input))
    (while (nthcdr 2 rest))
    (for window next (subseq rest 0 3))
    (for window-sum next (apply '+ window))
    (for last-window-sum previous window-sum)
    (counting (and last-window-sum (> window-sum last-window-sum)))))
