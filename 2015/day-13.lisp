(uiop:define-package #:com.andrewsoutar.aoc/2015/day-13
  (:use #:cl #:com.andrewsoutar.aoc/lib)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2015/day-13)
(in-readtable aoc)

(defun parse-input (input)
  (let ((people (make-hash-table :test 'equal))
        (happiness (make-hash-table :test 'equal)))
    (do-register-groups (person-a gain/loss ('parse-integer num) person-b)
        (#?r"(?m)^(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+).$" input)
      (map () {setf (gethash %1 people) t} (list person-a person-b))
      (setf (gethash (list person-a person-b) happiness)
            (* (scase gain/loss ("gain" 1) ("lose" -1)) num)))
    (values people happiness)))

(defun find-best-layout (people happiness-fn)
  (iter1
    (map-permutations {maximize (iter
                                  (for a in %1)
                                  (for b in (rotate (copy-list %1)))
                                  (map-permutations {sum (apply happiness-fn %1)} (list a b)))}
                      people)))

(defun part1 (input)
  (multiple-value-bind (people happiness) (parse-input input)
    (find-best-layout (hash-table-keys people) {gethash %% happiness})))

(defun part2 (input)
  (multiple-value-bind (people happiness) (parse-input input)
    (find-best-layout (list* 'me (hash-table-keys people))
                      {if (member 'me %%) 0 (gethash %% happiness)})))
