(uiop:define-package #:com.andrewsoutar.aoc/2015/day-10
  (:use #:cl #:com.andrewsoutar.aoc/lib)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2015/day-10)
(in-readtable aoc)

;;; A bit messy, but if we do this in the naive way we end up OOM on n=50
(defun look-and-say-iter (str)
  (iter outer
    (with out = (make-array (* 2 (length str)) :adjustable t :fill-pointer 0))
    (generate char in-vector str)
    (when (first-iteration-p) (next char))
    (iter (with init-char = (in outer char))
      (counting t into count)
      (while (eql (in outer (next char)) init-char))
      (finally-protected (iter (for char in-string (write-to-string count))
                           (vector-push-extend char out))
                         (vector-push-extend init-char out)))
    (finally (return-from outer out))))

(defun look-and-say-length (n input)
  (iter (repeat n)
    (for old-str first (string-trim #?"\n" input) then new-str)
    (for new-str = (look-and-say-iter old-str))
    (finally (return (length new-str)))))

(defun part1 (input)
  (look-and-say-length 40 input))

(defun part2 (input)
  (look-and-say-length 50 input))
