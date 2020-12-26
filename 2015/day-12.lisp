(uiop:define-package #:com.andrewsoutar.aoc/2015/day-12
  (:use #:cl #:com.andrewsoutar.aoc/lib #:cl-json)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2015/day-12)
(in-readtable aoc)

(defvar *accum* 0)
(defun parse-with-value-handler (handler input)
  (flet ((num-ish (x) (if (numberp x) x 0)))
    (bind-custom-vars
        (:boolean {0}
         :beginning-of-array {setf *accum* 0}
         :array-member {incf *accum* (num-ish %1)}
         :end-of-array {*accum*}
         :beginning-of-object {setf *accum* 0}
         :object-key {0}
         :object-value {unless (funcall handler %1) (incf *accum* (num-ish %1))}
         :end-of-object {*accum*}
         :aggregate-scope (list* '*accum* '*red* *aggregate-scope-variables*))
      (num-ish (decode-json-from-string input)))))

(defun part1 (input)
  (parse-with-value-handler {nil} input))

(defvar *red* nil)
(defun part2 (input)
  (let ((*red* nil))
    (parse-with-value-handler {or *red* (when (equal %1 "red") (setf *accum* 0 *red* t))}
                              input)))
