(uiop:define-package #:com.andrewsoutar.aoc/2020/day-9
  (:use #:cl #:com.andrewsoutar.aoc/lib)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2020/day-9)
(in-readtable aoc)

(defun part1 (input)
  (let ((nums (mapcar 'parse-integer (split #?/\s/ input))))
    (iter (for s on nums)
      (for subseq = (subseq s 0 25))
      (for n = (elt s 25))
      (finding n such-that (iter1 (map-combinations {always (/= (apply '+ %1) n)} subseq :length 2))))))

(defun part2 (input)
  (let ((invalid (part1 input))
        (nums (mapcar 'parse-integer (split #?/\s/ input))))
    (map-combinations {let ((range (apply 'subseq nums %1)))
                        (when (= (reduce '+ range) invalid)
                          (return-from part2 (+ (reduce 'min range) (reduce 'max range))))}
                      (iota (1+ (length nums))) :length 2)))
