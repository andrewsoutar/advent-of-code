(uiop:define-package #:com.andrewsoutar.aoc/2015/day-3
  (:use #:cl #:com.andrewsoutar.aoc/lib)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2015/day-3)

(defun iter-instructions (input init-fn get-fn current-coords-fn update-fn)
  (iter (with hash-table = (make-hash-table :test 'equal))
    (with all-coords = (funcall init-fn (cons 0 0)))

    (mapcar (lambda (coords)
              (setf (gethash (copy-list coords) hash-table) t))
            (funcall get-fn all-coords))

    (for c in-string input)
    (let ((current-coords (funcall current-coords-fn all-coords)))
      (case c
        (#\< (decf (car current-coords)))
        (#\> (incf (car current-coords)))
        (#\^ (decf (cdr current-coords)))
        (#\v (incf (cdr current-coords)))))
    (funcall update-fn all-coords)
    (finally (return (length (hash-table-keys hash-table))))))

(defun part1 (input)
  (iter-instructions input 'identity 'list 'identity (constantly nil)))

(defun part2 (input)
  (iter-instructions input (lambda (x) (cons (copy-list x) (copy-list x)))
                     (lambda (x) (list (car x) (cdr x)))
                     'car
                     (lambda (x) (rotatef (car x) (cdr x)))))
