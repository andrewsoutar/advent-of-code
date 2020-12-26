(uiop:define-package #:com.andrewsoutar.aoc/2020/day-12
  (:use #:cl #:com.andrewsoutar.aoc/lib)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2020/day-12)
(in-readtable aoc)

(defun part1 (input)
  (iter (for line in (split #?/\n/ input))
    (with x = 0)
    (with y = 0)
    (with dir = 0)
    (let ((c (char line 0))
          (n (parse-integer (subseq line 1))))
      (ecase c
        (#\N (incf y n))
        (#\S (decf y n))
        (#\E (incf x n))
        (#\W (decf x n))
        (#\L (incf dir n))
        (#\R (decf dir n))
        (#\F
         (incf x (* n (cos (* dir (/ pi 180)))))
         (incf y (* n (sin (* dir (/ pi 180))))))))
    (finally (return (+ (abs x) (abs y))))))

(defun part2 (input)
  (iter (for line in (split #?/\n/ input))
    (with ship = 0)
    (with waypoint = #C(10 1))
    (let ((c (char line 0))
          (n (parse-integer (subseq line 1))))
      (ecase c
        (#\N (incf waypoint (* n #C(0 1))))
        (#\S (decf waypoint (* n #C(0 1))))
        (#\E (incf waypoint (* n #C(1 0))))
        (#\W (decf waypoint (* n #C(1 0))))
        (#\L (setf waypoint (* waypoint
                               (case n
                                 (90 #C(0 1))
                                 (180 #C(-1 0))
                                 (270 #C(0 -1))
                                 (t (exp (* #C(0 1) (/ pi 180) n)))))))
        (#\R (setf waypoint (* waypoint
                               (case n
                                 (90 #C(0 -1))
                                 (180 #C(-1 0))
                                 (270 #C(0 1))
                                 (t (exp (* #C(0 1) (/ pi 180) (- n))))))))
        (#\F
         (incf ship (* n waypoint)))))
    (finally (return (+ (abs (realpart ship)) (abs (imagpart ship)))))))
