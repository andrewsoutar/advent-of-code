(uiop:define-package #:com.andrewsoutar.aoc/2020/day-7
  (:use #:cl #:com.andrewsoutar.aoc/lib)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2020/day-7)
(in-readtable aoc)

(defun parse-input (input)
  (let ((table (make-hash-table :test 'equal)))
    (do-register-groups (bag contents) (#?/(?m)^(.*) bags contain (.*)\.$/ input)
      (setf (gethash bag table)
            (if (string= contents "no other bags")
                nil
                (mapcar {register-groups-bind (('parse-integer n) bag) (#?/(\d+) (.*) bags?/ %1)
                          (list bag n)}
                        (split #?/, / contents)))))
    table))

(defun part1 (input)
  (let ((table (parse-input input)))
    (labels ((can-hold (bag inner-bag)
               (some {or (string= inner-bag (car %1)) (can-hold (car %1) inner-bag)}
                     (gethash bag table))))
      (iter (for (key) in-hashtable table)
        (counting (can-hold key "shiny gold"))))))

(defun part2 (input)
  (let ((table (parse-input input)))
    (labels ((num-inside (bag1)
               (iter (for (bag num) in (gethash bag1 table))
                 (sum (* num (1+ (num-inside bag)))))))
      (num-inside "shiny gold"))))
