(uiop:define-package #:com.andrewsoutar.aoc/2020/day-14
  (:use #:cl #:com.andrewsoutar.aoc/lib)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2020/day-14)
(in-readtable aoc)

(defun parse-input (input)
  (iter1 (do-register-groups (mask (#'parse-integer addr val)) (#?r"mask = ([^\n]*)|mem\[(.*)\] = ([^\n]*)" input)
           (collect (if mask
                        `(:mask ,(parse-integer (substitute #\1 #\X mask) :radix 2)
                                ,(parse-integer (substitute #\0 #\X mask) :radix 2))
                        `(:mem ,addr ,val))))))

(defun part1 (input)
  (let ((table (make-hash-table :test 'eql))
        (and-mask -1) (or-mask 0))
    (iter (for (type . values) in (parse-input input))
      (if (eql type :mask)
          (setf (values and-mask or-mask) (values-list values))
          (destructuring-bind (addr val) values
            (setf (gethash addr table) (logior (logand val and-mask) or-mask)))))
    (iter (for (nil val) in-hashtable table) (sum val))))

(defun part2 (input)
  (let ((table (make-hash-table :test 'eql))
        (or-mask 0) (floating-mask 1))
    (iter (for (type . values) in (parse-input input))
      (if (eql type :mask)
          (destructuring-bind (and-mask new-or-mask) values
            (setf or-mask new-or-mask floating-mask (logxor and-mask new-or-mask)))
          (destructuring-bind (addr val) values
            (iter (for i initially 0 then (logand floating-mask (1+ (logior i (lognot floating-mask)))))
              (while (or (first-time-p) (not (zerop i))))
              (setf (gethash (logior i (logandc2 addr floating-mask) or-mask) table) val)))))
    (values table (iter (for (nil val) in-hashtable table) (sum val)))))
