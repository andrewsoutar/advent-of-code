(uiop:define-package #:com.andrewsoutar.aoc/2015/day-6
  (:use #:cl #:com.andrewsoutar.aoc/lib)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2015/day-6)
(in-readtable aoc)

(defmacro do-lights (((inst x y) input) &body body)
  (with-unique-names (x1 y1 x2 y2)
    `(do-register-groups (,inst ('parse-integer ,x1 ,y1 ,x2 ,y2))
         (#?/(?m)^(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)$/ ,input)
       (iter (for ,x from ,x1 to ,x2)
         (iter (for ,y from ,y1 to ,y2)
           ,@body)))))

(defun flatten-array (x)
  (make-array (array-total-size x) :displaced-to x))

(defun part1 (input)
  (let ((lights (make-array '(1000 1000) :initial-element nil)))
    (do-lights ((inst x y) input)
      (setf #1=(aref lights x y)
            (scase inst
              ("turn on" t)
              ("turn off" nil)
              ("toggle" (not #1#))
              (t (break)))))
    (iter (for x in-vector (flatten-array lights))
      (counting x))))

(defun part2 (input)
  (let ((lights (make-array '(1000 1000) :initial-element 0)))
    (do-lights ((inst x y) input)
      (setf #1=(aref lights x y)
            (max 0 (+ #1# (scase inst
                            ("turn on" 1)
                            ("turn off" -1)
                            ("toggle" 2))))))
    (iter (for x in-vector (flatten-array lights))
      (sum x))))
