(uiop:define-package #:com.andrewsoutar.aoc/2020/day-13
  (:use #:cl #:com.andrewsoutar.aoc/lib)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2020/day-13)
(in-readtable aoc)

(defun parse-input (input)
  (destructuring-bind (timestamp-str buses-str)
      (split #?/\n/ input)
    (values (parse-integer timestamp-str)
            (mapcar {unless (string= %1 "x") (parse-integer %1)}
                    (split #?/,/ buses-str)))))

(defun part1 (input)
  (multiple-value-bind (timestamp buses) (parse-input input)
    (iter (for bus in buses)
      (when bus
        (let ((remaining-time (mod (- timestamp) bus)))
          (finding (* bus remaining-time) minimizing remaining-time))))))

(defun extended-euclid (a b)
  ;; Compute bezout coefficients
  ;; Invariant: ax + by = r
  (flet ((check-invariant (eqn)
           (destructuring-bind (r x y) eqn
             (assert (= r (+ (* x a) (* y b)))))))
    (iter
      (for eqn1 previous eqn2 initially (list a 1 0))
      (check-invariant eqn1)
      (for eqn2 previous eqn3 initially (list b 0 1))
      (check-invariant eqn2)
      (for eqn3 = (let ((quotient (floor (first eqn1) (first eqn2))))
                    (mapcar '- eqn1 (mapcar {* quotient %*} eqn2))))
      (check-invariant eqn3)
      (until (zerop (first eqn3)))
      (finally (return (values-list eqn2)))))) ; gcd(a, b) = xa + yb

(defun guts (buses)
  (let ((buses-offs (iter (for i from 0)
                      (for bus in buses)
                      (when bus (collecting (list i bus))))))
    (iter
      (with a = 0)
      (with n = 1)
      (for (off bus) in buses-offs)
      (assert (= 1 (gcd bus n)))
      (multiple-value-bind (m1 m2) (exteuc n bus)
        (setf (values a n)
              (values (+ (* a m2 bus) (* (- off) m1 n))
                      (* n bus)))
        (setf a (mod a n))
        (assert (zerop (mod (+ a off) bus))))
      (finally (return a)))))

(defun part2 (input)
  (destructuring-bind (timestamp-str buses-str) (split #?/\n/ input)
    (declare (ignore timestamp-str))
    (iter
      ;; Have solutions when x \equiv a (mod n)
      (with a = 0)
      (with n = 1)
      (for bus-str in (split #?/,/ buses-str))
      (for offset from 0)
      (unless (string= bus-str "x")
        (let ((bus (parse-integer bus-str)))
          ;; Add additional equation: x \equiv -offset (mod bus)
          (multiple-value-bind (gcd x y) (extended-euclid n bus)
            ;; Make sure they're coprime, to satisfy the Chinese Remainder Theorem
            (assert (= gcd 1))
            ;; Now, we have x*n + y*bus = 1
            ;; So, x*n \equiv 1 (mod bus) and y*bus \equiv 1 (mod n)
            ;; So, -offset*x*n + a*y*bus \equiv -offset (mod bus)
            ;; And -offset*x*n + a*y*bus \equiv a (mod n)
            (setf a (+ (* -1 offset x n) (* a y bus)))
            ;; The above solution holds modulo n*bus
            (setf n (* n bus))
            ;; Reduce to keep numbers smaller
            (setf a (mod a n)))))
      (finally (return a)))))
