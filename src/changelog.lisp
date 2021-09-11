(defpackage #:humanize-duration/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package humanize-duration/changelog)


(defchangelog ()
  (0.2.0 2021-09-11
         "Move to a new documentation renderer.")
  (0.1.0 2021-01-25
         "Initial version."))
