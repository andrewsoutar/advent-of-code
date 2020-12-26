(uiop:define-package #:com.andrewsoutar.aoc/2015/day-19
  (:use #:cl #:com.andrewsoutar.aoc/lib)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2015/day-19)
(in-readtable aoc)

(defun parse-input (input)
  (destructuring-bind (replacements molecule) (split #?/\n\n/ input)
    (values
     (iter1 (do-register-groups (source target) (#?/(?m)^(.*?) => (.*?)$/ replacements)
              (collect (list source target))))
     molecule)))

(defun map-productions (molecule replacements fn)
  (iter
    (for (source target) in replacements)
    (do-matches (start end `(:sequence ,source) molecule)
      (funcall fn (concatenate 'string (subseq molecule 0 start) target (subseq molecule end))))))

(defun part1 (input)
  (multiple-value-bind (replacements molecule) (parse-input input)
    (let ((table (make-hash-table :test 'equal)))
      (map-productions molecule replacements {setf (gethash %1 table) t})
      (length (hash-table-keys table)))))

(defun part2 (input)
  (multiple-value-bind (replacements molecule) (parse-input input)
    ;; DFS runs out of memory, so use iterative deepening
    ;; FIXME this takes literally forever, find a better way
    (labels ((map-at-depth (n fn)
               (if (= n 0)
                   (funcall fn "e")
                   (map-at-depth (1- n) {map-productions %1 replacements fn}))))
      (iter (for depth from 1)
        (format t "depth=~A~%" depth)
        (map-at-depth depth {finding depth such-that (string= %1 molecule)})))))
