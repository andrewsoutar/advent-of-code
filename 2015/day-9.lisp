(uiop:define-package #:com.andrewsoutar.aoc/2015/day-9
  (:use #:cl #:com.andrewsoutar.aoc/lib)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2015/day-9)
(in-readtable aoc)

(defmacro frob (finder input)
  `(let ((all-cities (make-hash-table :test 'equal))
         (table (make-hash-table :test 'equal)))
     (do-register-groups (a b ('parse-integer distance))
         (#?/(?m)^(\w+) to (\w+) = (\d+)$/ ,input)
       (setf (gethash (sort (list a b) 'string<) table) distance)
       (mapcar {setf (gethash %1 all-cities) t} (list a b)))
     (iter1
       (map-permutations {,finder (iter (for a in %1)
                                    (for b previous a)
                                    (unless (first-iteration-p)
                                      (sum (gethash (sort (list a b) 'string<) table))))}
                         (hash-table-keys all-cities)))))

(defun part1 (input)
  (frob minimize input))

(defun part2 (input)
  (frob maximize input))
