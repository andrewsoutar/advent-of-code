(uiop:define-package #:com.andrewsoutar.aoc/2020/day-4
  (:use #:cl #:com.andrewsoutar.aoc/lib)
  (:export #:part1 #:part2))
(in-package #:com.andrewsoutar.aoc/2020/day-4)
(in-readtable aoc)

(defmacro count-for-passports ((var input) &body body)
  `(iter (for passport in (split #?/\n\n/ ,input))
     (let ((,var
             (iter (for group in (split #?/\s/ passport))
               (register-groups-bind (key value) (#?/^(.*?):(.*)$/ group)
                 (collecting (cons key value))))))
       (counting (progn ,@body)))))

(defun part1 (input)
  (count-for-passports (parsed-passport input)
    (every {assoc %1 parsed-passport :test 'string=}
           '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))))

(defun part2 (input)
  (count-for-passports (parsed-passport input)
    (flet ((getp (x) (cdr (assoc x parsed-passport :test 'string=))))
      (ignore-errors
       (and (<= 1920 (parse-integer (getp "byr")) 2002)
            (<= 2010 (parse-integer (getp "iyr")) 2020)
            (<= 2020 (parse-integer (getp "eyr")) 2030)
            (register-groups-bind (('parse-integer num) unit) (#?/(\d+)(.*)/ (getp "hgt"))
              (scase unit
                ("cm" (<= 150 num 193))
                ("in" (<= 59 num 76))))
            (scan #?/^#[0-9a-f]{6}$/ (getp "hcl"))
            (member (getp "ecl") (split " " "amb blu brn gry grn hzl oth") :test 'string=)
            (scan #?/^\d{9}$/ (getp "pid")))))))
