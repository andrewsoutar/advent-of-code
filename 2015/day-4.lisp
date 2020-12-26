(uiop:define-package #:com.andrewsoutar.aoc/2015/day-4
  (:shadow #:xor)
  (:use #:cl #:com.andrewsoutar.aoc/lib #:flexi-streams #:ironclad)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2015/day-4)

(defmacro iter-nonce ((var key) &body body)
  (once-only ((key `(string-trim '(#\Newline) ,key)))
    (with-unique-names (nonce)
      `(iter
         (for ,nonce from 0)
         (for ,var = (digest-sequence 'md5 (string-to-octets
                                            (format nil "~A~A" ,key ,nonce))))
         (finding ,nonce such-that (progn ,@body))))))

(defun part1 (input)
  (iter-nonce (hash input)
    (and (zerop (elt hash 0))
         (zerop (elt hash 1))
         (zerop (logand (elt hash 2) #xF0)))))

(defun part2 (input)
  (iter-nonce (hash input)
    (and (zerop (elt hash 0))
         (zerop (elt hash 1))
         (zerop (elt hash 2)))))
