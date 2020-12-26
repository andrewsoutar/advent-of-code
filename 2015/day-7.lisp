(uiop:define-package #:com.andrewsoutar.aoc/2015/day-7
  (:use #:cl #:com.andrewsoutar.aoc/lib)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2015/day-7)
(in-readtable aoc)

(defun get-wire-value (out-wire get-wire-fn input)
  (let ((table (make-hash-table :test 'equal)))
    (labels ((get-wire (x) (funcall get-wire-fn x table))
             (parse-value (x)
               (or (ignore-errors (parse-integer x)) (get-wire x) (throw 'not-found (values)))))
      (iter
        (do-register-groups (expr wire) (#?/(?m)^(.*) -> ([a-z]+)$/ input)
          (unless (get-wire wire)
            (catch 'not-found
              (setf (gethash wire table)
                    (or
                     (register-groups-bind ((#'parse-value x)) (#?/^(\S+)$/ expr)
                       x)
                     (register-groups-bind ((#'parse-value x)) (#?/^NOT (\S+)$/ expr)
                       (lognot x))
                     (register-groups-bind ((#'parse-value x) op (#'parse-value y))
                         (#?r"^(\S+) (AND|OR|LSHIFT|RSHIFT) (\S+)$" expr)
                       (scase op
                         ("AND" (logand x y))
                         ("OR" (logior x y))
                         ("LSHIFT" (ash x y))
                         ("RSHIFT" (ash x (- y)))))
                     (break expr)))))
          (when-let (wire-value (get-wire out-wire))
            (return-from get-wire-value wire-value)))))))

(defun part1 (input)
  (get-wire-value "a" 'gethash input))

(defun part2 (input)
  (let ((part1-value (part1 input)))
    (get-wire-value "a" {if (string= %1 "b") part1-value (gethash %1 %2)} input)))
