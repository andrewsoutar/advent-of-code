(uiop:define-package #:com.andrewsoutar.aoc/2020/day-2
  (:use #:cl #:com.andrewsoutar.aoc/lib)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2020/day-2)
(in-readtable aoc)

(defmacro passwords-count ((lo-var hi-var char-var password-var) input &body body)
  `(iter1
     (do-register-groups (('parse-integer ,lo-var ,hi-var) ({char %1 0} ,char-var) ,password-var)
         (#?/(?m)^(\d+)-(\d+) (.): (.*)$/ ,input)
       (counting (progn ,@body)))))

(defun part1 (input)
  (passwords-count (lo hi char password) input
    (<= lo (count char password) hi)))

(defun part2 (input)
  (passwords-count (lo hi char password) input
    (= 1 (count char (mapcar {char password (1- %1)} (list lo hi))))))
