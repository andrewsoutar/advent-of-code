(uiop:define-package #:com.andrewsoutar.aoc/2020/day-8
  (:use #:cl #:com.andrewsoutar.aoc/lib)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2020/day-8)
(in-readtable aoc)

(defun parse-input (input)
  (iter1 (do-register-groups (op ('parse-integer arg))
             (#?/(?m)^(\S+) (\S+)$/ input)
           (collect `(,op ,arg)))))

(defun run-instructions (instructions)
  (iter
    (with seen-instructions = (make-hash-table))
    (with ip = 0)
    (with accum = 0)
    (when (>= ip (length instructions))
      (return (values accum t)))
    (when (gethash ip seen-instructions)
      (return (values accum nil)))
    (setf (gethash ip seen-instructions) t)
    (for (op arg) = (elt instructions ip))
    (incf ip)
    (scase op
      ("nop")
      ("acc" (incf accum arg))
      ("jmp" (incf ip (1- arg))))))

(defun part1 (input)
  (values (run-instructions (parse-input input))))

(defun part2 (input)
  (iter (with instructions = (parse-input input))
    (for inst in instructions)
    (for op = (car inst))
    (setf (car inst) (scase op ("nop" "jmp") ("jmp" "nop") ("acc" (next-iteration))))
    (multiple-value-bind (output terminated-p) (run-instructions instructions)
      (finding output such-that terminated-p))
    (setf (car inst) op)))
