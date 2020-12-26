(uiop:define-package #:com.andrewsoutar.aoc/2020/day-10
  (:use #:cl #:com.andrewsoutar.aoc/lib)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2020/day-10)
(in-readtable aoc)

(defun parse-input (input)
  (sort (mapcar 'parse-integer (split #?/\n/ input)) '<))

(defun part1 (input)
  (iter
    (for current-joltage previous adapter-joltage initially 0)
    (for adapter-joltage in (parse-input input))
    (for difference = (- adapter-joltage current-joltage))
    (assert (<= 1 difference 3))
    (counting (= difference 1) into differ-by-one)
    (counting (= difference 3) into differ-by-three)
    (finally
     ;; Final joltage is three more than the last adapter
     (incf differ-by-three)
     (return (* differ-by-one differ-by-three)))))

(defun part2 (input)
  (iter
    (with joltages = (list* 0 (parse-input input)))
    (with table = (make-list (length joltages) :initial-element 0))
    (initially (setf (elt table 0) 1))

    (for (combinations . remaining-combinations) on table)
    (for (joltage . remaining-joltages) on joltages)
    (iter
      (for next-joltage in remaining-joltages)
      (for combination-cons on remaining-combinations)
      (while (<= next-joltage (+ joltage 3)))
      (incf (car combination-cons) combinations))
    (finally (return (last-elt table)))))
