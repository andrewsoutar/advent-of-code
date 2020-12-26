(uiop:define-package #:com.andrewsoutar.aoc/2020/day-3
  (:use #:cl #:com.andrewsoutar.aoc/lib)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2020/day-3)
(in-readtable aoc)

(defun parse-input (input)
  (let ((splits (split #?/\n/ input)))
    (make-array (list (length splits) (length (elt splits 0)))
                :initial-contents (mapcar {map 'list {char= #\# %2} %1} splits))))

(defun count-trees (down over input)
  (let ((array (parse-input input)))
    (iter (for y from 0 below (array-dimension array 0) by down)
      (for x from 0 by over)
      (counting (aref array y (mod x (array-dimension array 1)))))))

(defun part1 (input)
  (count-trees 1 3 input))

(defun part2 (input)
  (apply '* (mapcar {((over down)) (count-trees down over input)}
                    '((1 1) (3 1) (5 1) (7 1) (1 2)))))
