(uiop:define-package #:com.andrewsoutar.aoc/2021/day-2
  (:use #:cl #:com.andrewsoutar.aoc/lib)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2021/day-2)
(in-readtable aoc)

(defun parse-input (input)
  (iter1 (do-register-groups (direction (#'parse-integer num)) (#?"([^ ]*) (\\d+)\\n" input)
           (collect (list direction num)))))

(defun part1 (input)
  (let ((input (parse-input input))
        (horiz 0) (depth 0))
    (iter (for (direction num) in input)
      (match-ecase direction
        ("forward" (incf horiz num))
        ("down" (incf depth num))
        ("up" (decf depth num))))
    (* horiz depth)))

(defun part2 (input)
  (let ((input (parse-input input))
        (aim 0) (horiz 0) (depth 0))
    (iter (for (direction num) in input)
      (match-ecase direction
        ("down" (incf aim num))
        ("up" (decf aim num))
        ("forward" (incf horiz num) (incf depth (* aim num)))))
    (* horiz depth)))
