(uiop:define-package #:com.andrewsoutar.aoc/2020/day-11
  (:use #:cl #:com.andrewsoutar.aoc/lib)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2020/day-11)
(in-readtable aoc)

(defun parse-input (input)
  (let ((i (split #?/\n/ input)))
    (make-array (list (length i) (length (car i))) :initial-contents i)))

(defun inc-array (arr)
  (let ((ret (make-array (array-dimensions arr))))
    (iter (for i below (array-dimension ret 0))
      (iter (for j below (array-dimension ret 1))
        (flet ((g (a b)
                 (if (and (zerop a) (zerop b))
                     (aref arr i j)
                     (or
                      (ignore-errors
                       (iter (for mul from 1)
                         (let ((r
                                 (aref arr (+ i (* mul a)) (+ j (* mul b)))))
                           (unless (char= r #\.)
                             (return r)))))
                      #\L))))
          (let ((around (iter outer (for a from -1 to 1)
                          (iter (for b from -1 to 1)
                            (in outer (collect (list a b)))))))
            (setf (aref ret i j)
                  (cond ((and (char= #\L (g 0 0))
                              (every {not (char= #\# (apply #'g %1))} around))
                         #\#)
                        ((and (char= #\# (g 0 0))
                              (>= (count-if {char= #\# (apply #'g %1)} around) 6))
                         #\L)
                        ((g 0 0))))))))
    ret))

(defun part1 (input)
  (iter
    (for src previous next initially (parse-input input))
    (for next = (inc-array src))
    (until (equalp (flatarr src) (flatarr next)))
    (finally (return (count #\# (flatarr next))))))
