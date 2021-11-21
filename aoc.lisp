(uiop:define-package #:com.andrewsoutar.aoc
  (:nicknames #:com.andrewsoutar.aoc/aoc)
  (:use #:cl #:alexandria #:asdf #:drakma)
  (:export #:get-input #:run-solution #:today))
(in-package #:com.andrewsoutar.aoc/aoc)

(defparameter *secrets*
  (with-open-file (secrets
                   (asdf:system-relative-pathname #.(string-downcase (package-name *package*)) ".secrets.lisp-expr"))
    (read secrets)))

(defparameter *session* (cdr (assoc 'session *secrets*)))

(defparameter *year* nil)
(defparameter *day* nil)

(defun guess-date ()
  (multiple-value-bind (sec min hr day mon year dow dst tz) (get-decoded-time)
    (declare (ignore sec min mon dow dst))
    (values (or *year* year)
            (or *day* (if (> (+ hr tz) (+ 22 5)) (1+ day) day)))))

(defvar *inputs* (make-hash-table :test 'equal))

(defun get-input-url (year day)
  (format nil "https://adventofcode.com/~A/day/~A/input" year day))
(defun get-input (year day &optional (session *session*))
  (labels ((make-cookie ()
             (make-instance 'drakma:cookie :name "session" :value session
                                           :domain ".adventofcode.com"
                                           :path "/"
                                           :http-only-p t :securep t))
           (make-cookie-jar ()
             (make-instance 'drakma:cookie-jar :cookies (list (make-cookie))))
           (perform-request ()
             (let ((url (get-input-url year day)))
               (multiple-value-bind (body status headers
                                     return-url stream closed-p reason)
                   (http-request (get-input-url year day)
                                 :redirect nil
                                 :cookie-jar (make-cookie-jar))
                 (declare (ignore headers return-url stream closed-p))
                 (if (= status 200)
                     body
                     (error "Error fetching input ~A from ~A: ~A ~A"
                            (list year day) url status reason))))))
    (ensure-gethash (list year day) *inputs* (perform-request))))

(defun run-solution (year day name &optional (input (get-input year day)))
  (let* ((package-name (format nil "~A/~A/~A-~A" (package-name '#.*package*) year 'day day))
         (package (or (find-package package-name)
                      (progn
                        (load-system (string-downcase package-name))
                        (find-package package-name)))))
    (multiple-value-bind (symbol status) (find-symbol (string name) package)
      (unless (eq status :external)
        (cerror "Use anyway" "~A is not external in ~A" name package))
      (funcall (symbol-function symbol) input))))

(defmacro today (form)
  `(multiple-value-call #',(car form) (guess-date) (values ,@(cdr form))))
