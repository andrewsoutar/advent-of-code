(uiop:define-package #:com.andrewsoutar.aoc/2015/day-1
  (:use #:cl #:com.andrewsoutar.aoc/lib)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2015/day-1)

(defun part1 (input)
  (- (count #\( input :test 'char=)
     (count #\) input :test 'char=)))

(defun part2 (input)
  (iter
    (for position from 1)
    (for c in-string input)
    (sum (if (char= c #\() 1 -1) into floor)
    (until (= floor -1))
    (finally (return position))))
