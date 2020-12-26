(uiop:define-package #:com.andrewsoutar.aoc/2015/day-11
  (:use #:cl #:com.andrewsoutar.aoc/lib)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2015/day-11)
(in-readtable aoc)

(defun next-password (password)
  (iter
    (for c in-string password with-index i downto 0)
    (if (char= c #\z)
        (setf #1=(elt password i) #\a)
        (progn
          (setf #1# (code-char (1+ (char-code c))))
          (return))))
  password)

(defun password-valid-p (password)
  (and (scan (format nil "~{~A~^|~}"
                     (mapcar {map 'string 'code-char (iota 3 :start %1)}
                             (iota 24 :start (char-code #\a))))
             password)
       (not (scan #?/i|o|l/ password))
       (scan #?/(.)\1.*(?!\1)(.)\2/ password)))

(defun find-next-valid-password (password)
  (iter
    (for prev first password then next)
    (for next = (next-password prev))
    (finding next such-that #'password-valid-p)))

(defun part1 (input)
  (find-next-valid-password (string-trim #?"\n" input)))

(defun part2 (input)
  (find-next-valid-password (part1 input)))
