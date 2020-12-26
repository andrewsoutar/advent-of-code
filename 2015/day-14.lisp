(uiop:define-package #:com.andrewsoutar.aoc/2015/day-14
  (:use #:cl #:com.andrewsoutar.aoc/lib)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2015/day-14)
(in-readtable aoc)

(defun parse-input (input)
  (iter (for line in (split #?/\n/ input))
    (register-groups-bind (('parse-integer speed fly-time rest-time))
        (#?/ (\d+) .* (\d+) .* (\d+) / line)
      (collect (list speed fly-time rest-time)))))

(defun advance-reindeer (reindeer states)
  (mapcar {((speed fly-time rest-time) (flying-p distance time))
           (when flying-p (incf distance speed))
           (when (= (if flying-p fly-time rest-time) (incf time))
             (setf time 0)
             (setf flying-p (not flying-p)))
           `(,flying-p ,distance ,time)}
          reindeer states))

(defmacro iter-reindeer ((state-var &optional (reindeer-var (gensym "REINDEER")))
                         input &body body)
  `(iter (with ,reindeer-var = (parse-input ,input))
     (for ,state-var = (advance-reindeer ,reindeer-var
                                         (if-first-time
                                          (make-list (length ,reindeer-var)
                                                     :initial-element '(t 0 0))
                                          ,state-var)))
     (repeat 2503)
     ,@body))

(defun max-distance (state)
  (reduce 'max (mapcar 'second state)))

(defun part1 (input)
  (iter-reindeer (state) input
    (finally (return (max-distance state)))))

(defun part2 (input)
  (declare (optimize debug))
  (iter-reindeer (state reindeer) input
    (with scores = (make-list (length reindeer) :initial-element 0))
    (iter (with max-distance = (max-distance state))
      (for (() distance) in-sequence state with-index i)
      (when (= distance max-distance) (incf (elt scores i))))
    (finally (return (reduce 'max scores)))))
