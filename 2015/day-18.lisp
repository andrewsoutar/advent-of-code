(uiop:define-package #:com.andrewsoutar.aoc/2015/day-18
  (:use #:cl #:com.andrewsoutar.aoc/lib)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2015/day-18)
(in-readtable aoc)

(defun foo (input &optional (stuck-fn (constantly nil)))
  (iter
    (for prev-array initially (make-array '(100 100) :initial-contents
                                          (mapcar {map 'list {char= #\# %*} %*} (split #?/\n/ input)))
         then next-array)
    (repeat 100)
    (for next-array = (make-array (array-dimensions prev-array)))
    (iter (for x below (array-dimension next-array 0))
      (iter (for y below (array-dimension next-array 1))
        (setf (aref next-array x y)
              (or (funcall stuck-fn x y)
                  (let ((count (count-if {ignore-errors
                                          (let ((args (mapcar '+ (list x y) %1)))
                                            (or (apply stuck-fn args) (apply 'aref prev-array args)))}
                                         '((-1 -1) (-1 0) (-1 1)
                                           ( 0 -1)        ( 0 1)
                                           ( 1 -1) ( 1 0) ( 1 1)))))
                    (if (aref prev-array x y)
                        (<= 2 count 3)
                        (= count 3)))))))
    (finally (return
               (let ((flat (make-array (array-total-size next-array) :displaced-to next-array)))
                 (count-if 'identity flat))))))

(defun part1 (input)
  (foo input))

(defun part2 (input)
  (foo input {every {member %1 '(0 99)} %%}))
