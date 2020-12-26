(uiop:define-package #:com.andrewsoutar.aoc/2015/day-17
  (:use #:cl #:com.andrewsoutar.aoc/lib)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2015/day-17)
(in-readtable aoc)

(defun part1 (input)
  (let ((sizes (mapcar 'parse-integer (split #?/\s/ input))))
    (iter (for num to (length sizes))
      (map-combinations {counting (= 150 (reduce '+ %1))} sizes :length num))))

(defun part2 (input)
  (let ((sizes (mapcar 'parse-integer (split #?/\s/ input))))
    (iter (for num to (length sizes))
      (finding
       (iter1
         (map-combinations {counting (= 150 (reduce '+ %1))} sizes :length num))
       such-that #'plusp))))
