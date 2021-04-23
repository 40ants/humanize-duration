(uiop:define-package #:humanize-duration/ru
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:export
   #:format-part))
(in-package humanize-duration/ru)


(defsection @index (:title "Russian localization")
  "This package includes a single function, useful to display duration in Russian language:"
  
  (format-part function)

  "You can use it as an template, to define other languages.

   Please, don't forget to contribute your solutions!"

  "Here is how you can use it in your code:

   ```
   CL-USER> (humanize-duration:humanize-duration
             (local-time-duration:duration :day 5
                                           :hour 13
                                           :minute 0
                                           :sec 14
                                           :nsec 10042)
             :format-part #'humanize-duration/ru:format-part)
   \"5 дней 13 часов\"
   ```

   Here is how FORMAT-PART function is defined in the code:
"
  (format-part (include (:start (format-part function)
                         :end (%end-of-format-part% variable))
                         :header-nl "```commonlisp"
                         :footer-nl "```"))
  
  "Russian version uses internal helper, to choose a correct word form:"

  (choose-form (include (:start (choose-form function)
                         :end (%end-of-choose-form% variable))
                         :header-nl "```commonlisp"
                         :footer-nl "```"))

  "Applied to a different numbers it produces the following:

   ```
   CL-USER> (flet ((p (n)
                     (format t \"~A ~A~%\"
                             n
                             (choose-form n \"яблоко\" \"яблока\" \"яблок\"))))
              (loop for i upto 12
                    do (p i)))
   0 яблок
   1 яблоко
   2 яблока
   3 яблока
   4 яблока
   5 яблок
   6 яблок
   7 яблок
   8 яблок
   9 яблок
   10 яблок
   11 яблок
   12 яблок
   ```
")


(defun choose-form (n &rest forms)
  "This function is bassed on this gettext formula:

   ```
   Plural-Forms: nplurals=3; plural=n%10==1 && n%100!=11 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2;
   ```
"
  (let ((n%10 (rem n 10))
        (n%100 (rem n 100)))
    (cond
      ((and (= n%10
               1)
            (not (= n%100
                    11)))
       (first forms))
      ((and (>= n%10
                2)
            (<= n%10
                4)
            (or (<= n%100
                    10)
                (>= n%100
                    20)))
       (second forms))
      (t
       (third forms)))))

(defvar %end-of-choose-form%)


(defun format-part (stream part-type part)
  "This is Russian version of part formatter for HUMANIZE-DURATION"
  (format stream "~d ~A"
          part
          (apply #'choose-form
                 part
                 (ecase part-type
                   (:weeks '("неделя" "недели" "недель"))
                   (:days '("день" "дня" "дней"))
                   (:hours '("час" "часа" "часов"))
                   (:minutes '("минута" "минуты" "минут"))
                   (:secs '("секунда" "секунды" "секунд"))
                   (:nsecs '("наносекунда" "наносекунды" "наносекунд"))))))

(defvar %end-of-format-part%)
