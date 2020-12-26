(uiop:define-package #:com.andrewsoutar.aoc/2015/day-16
  (:use #:cl #:com.andrewsoutar.aoc/lib)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2015/day-16)
(in-readtable aoc)

(defun solve (input fn-fn)
  (do-register-groups (('parse-integer i) desc) (#?/(?m)^Sue (\d+): (.*?)$/ input)
    (when
        (iter1
          (do-register-groups (thing ('parse-integer num)) (#?/([^ :]+?): (\d+)/ desc)
            (block doesnt-count
              (always (funcall (funcall fn-fn thing)
                               num (scase thing
                                     ("children" 3)
                                     ("cats" 7)
                                     ("samoyeds" 2)
                                     ("pomeranians" 3)
                                     ("akitas" 0)
                                     ("vizslas" 0)
                                     ("goldfish" 5)
                                     ("trees" 3)
                                     ("cars" 2)
                                     ("perfumes" 1)
                                     (t (return-from doesnt-count))))))))
      (return i))))

(defun part1 (input)
  (solve input (constantly '=)))

(defun part2 (input)
  (solve input {scase %1 (("cats" "trees") '>) (("pomeranians" "goldfish") '<) (t '=)}))
