(uiop:define-package :humanize-duration
  (:use #:cl)
  (:nicknames :humanize-duration/core)
  (:import-from #:local-time-duration)
  (:import-from #:humanize-duration/ru)
  (:import-from #:40ants-doc
                #:defsection
                #:defsection-copy)
  (:import-from #:with-output-to-stream
                #:with-output-to-stream)
  (:import-from #:humanize-duration/changelog
                #:@changelog)
  (:import-from #:docs-config
                #:docs-config)
  (:export #:@index #:@readme
           #:default-format-part
           #:humanize-duration))
(in-package humanize-duration)


(defmethod docs-config ((system (eql (asdf:find-system "humanize-duration"))))
  ;; 40ANTS-DOC-THEME-40ANTS system will bring
  ;; as dependency a full 40ANTS-DOC but we don't want
  ;; unnecessary dependencies here:
  (ql:quickload :40ants-doc-theme-40ants)
  (list :theme
        (find-symbol "40ANTS-THEME"
                     (find-package "40ANTS-DOC-THEME-40ANTS"))))


(defsection @index (:title "Duration representation for humans!"
                    :ignore-words ("GIT"
                                   "LOCAL-TIME-DURATION:DURATION"
                                   "LOCAL-TIME-DURATION:HUMAN-READABLE-DURATION"))
  "This is a small library usefult for time duration humanization."

  (humanize-duration system)
  (@intro section)
  (@localization section))

(defsection-copy @readme @index)


(defsection @intro (:title "Introduction")
  "It is different from LOCAL-TIME-DURATION:HUMAN-READABLE-DURATION, because allows
   to output only significant parts of a duration object.

   Also it limits a number of part. For example, if there days, hours, minutes and seconds,
   then most probably it is already not important exactly how many minutes and seconds
   passed since the moment.

   Compare these two examples:

   ### local-time-duration:human-readable-duration

   ```
   CL-USER> (local-time-duration:human-readable-duration
             (local-time-duration:duration :day 5
                                           :hour 13
                                           :minute 0
                                           :sec 14
                                           :nsec 10042))
   \" 5 days 13 hours 14 seconds 10042 nsecs\"
   ```

   ### humanize-duration:humanize-duration

   ```
   CL-USER> (humanize-duration:humanize-duration
             (local-time-duration:duration :day 5
                                           :hour 13
                                           :minute 0
                                           :sec 14
                                           :nsec 10042))
   \"5 days 13 hours\"
   ```

   Main job is done at HUMANIZE-DURATION:
"
  (humanize-duration function)

  "HUMANIZE-DURATION accepts :FORMAT-PART argument, which is DEFAULT-FORMAT-PART function by default:
   your own version. This could be useful if you want to support localization to other languages."

  (default-format-part function)

  "Here is how you can localize output for your language")


(defsection @localization (:title "Localization")
  "HUMANIZE-DURATION comes with predefined Russian localization."
  (humanize-duration/ru::@index section))


(defun default-format-part (stream part-type part)
  "This is should return a string with propertly pluralized form.

   - PART-TYPE argument is a member of (list :weeks :days :hours :minutes :secs :nsecs).
   - PART is an integer.

   Here are possible results:

       (t :weeks 1) -> \"1 week\"
       (t :weeks 5) -> \"5 weeks\"
       (t :day 2) -> \"2 days\"
"
  (ecase part-type
    (:weeks (format stream "~d week~:p" part))
    (:days (format stream "~d day~:p" part))
    (:hours (format stream "~d hour~:p" part))
    (:minutes (format stream "~d minute~:p" part))
    (:secs (format stream "~d second~:p" part))
    (:nsecs (format stream "~d nsec~:p" part))))


(defun humanize-duration (duration &key stream (n-parts 2) (format-part #'default-format-part))
  "This is the better version of LOCAL-TIME-DURATION:HUMAN-READABLE-DURATION.

   By default it returns only 2 most significant duration parts.

   If duration is 2 hour, 43 seconds and 15 nanoseconsds, then
   function will return \"2 hours 43 seconds\":

   ```lisp
   CL-USER> (ultralisp/utils/time:humanize-duration
             (local-time-duration:duration :hour 2
                                           :sec 43
                                           :nsec 15))
   \"2 hours 43 seconds\"
   ```

   Also, you can pass a `:format-part` argument.
   It should be a function of three arguments:
   `(stream part-type part)` where `part-type` is a keyword
   from this list:

   ```lisp
   (list :weeks :days :hours :minutes :secs :nsecs)
   ```"
  (check-type duration local-time-duration:duration)
  (check-type n-parts (integer 1 6))
  
  (multiple-value-bind (nsecs secs minutes hours days weeks)
      (local-time-duration::decode-duration duration :weeks t)
    (with-output-to-stream (stream stream)
      (let ((part-types (list :weeks :days :hours :minutes :secs :nsecs))
            (parts (list weeks days hours minutes secs nsecs)))
        (if (every #'zerop parts)
            (format stream "0 length")
            (loop with n-printed = 0
                  for part in parts
                  for part-type in part-types
                  unless (zerop part)
                  do (unless (zerop n-printed)
                       ;; Add a space between parts
                       (format stream " "))
                     (funcall format-part
                              stream
                              part-type
                              part)
                     (incf n-printed)
                  when (>= n-printed n-parts)
                  do (return)))))))
