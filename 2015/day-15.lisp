(uiop:define-package #:com.andrewsoutar.aoc/2015/day-15
  (:use #:cl #:com.andrewsoutar.aoc/lib)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2015/day-15)
(in-readtable aoc)

(defun parse-input (input)
  (declare (optimize debug))
  (iter1 (do-register-groups (line) (#?/(?m)^.*?: (.*?)$/ input)
           (collect (iter1 (do-register-groups (prop ('parse-integer value))
                               (#?/(\S+) (-?\d+)/ line)
                             (collect (cons prop value))))))))

(defmacro do-bounded-sum ((var sum count) &body body)
  (once-only (sum count)
    (with-gensyms (combo)
      `(progn
         (map-combinations
          (lambda (,combo)
            (let ((,var (mapcar {(x y) (- x (1+ y))}
                                `(,@,combo ,(+ ,count ,sum -1))
                                `(-1 ,@,combo))))
              ,@body))
          (iota (+ ,count ,sum -1)) :length (1- ,count))
         ()))))

(defmacro foo (input
               &optional (amounts-var (gensym "AMOUNTS"))
                 (ingredients-var (gensym "INGREDIENTS"))
               &body test-expr)
  (with-gensyms (ingredients amounts)
    `(iter1 (with ,ingredients = (parse-input ,input))
       (do-bounded-sum (,amounts 100 (length ,ingredients))
         (when ,(if test-expr `(let ((,amounts-var ,amounts) (,ingredients-var ,ingredients))
                                 ,@test-expr)
                    't)
           (maximizing
            (iter (for property in '("capacity" "durability" "flavor" "texture"))
              (multiplying (max 0 (iter
                                    (for ingredient in ,ingredients)
                                    (for amount in ,amounts)
                                    (sum (* (cdr (assoc property ingredient :test 'string=)) amount))))))))))))

(defun part1 (input)
  (foo input))

(defun part2 (input)
  (foo input amounts ingredients
    (= 500 (reduce '+ (mapcar {* (cdr (assoc "calories" %1 :test 'string=)) %2} ingredients amounts)))))
