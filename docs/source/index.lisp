(defpackage #:humanize-duration-docs/docs
  (:nicknames #:humanize-duration-docs)
  (:use #:cl)
  (:import-from #:mgl-pax)
  (:import-from #:humanize-duration/core
                #:@index)
  (:export
   #:build))
(in-package humanize-duration-docs/docs)


(defun build (&optional (root-section-name '@index))
  (check-type root-section-name symbol)
  
  (let ((root-section (symbol-value root-section-name)))
    (mgl-pax:update-asdf-system-readmes root-section
                                        :humanize-duration)
  
    (mgl-pax:update-asdf-system-html-docs
     root-section :humanize-duration
     :target-dir "docs/build/"
     :pages `((:objects (,root-section)
               :source-uri-fn ,(pax:make-github-source-uri-fn
                                :humanize-duration
                                "https://github.com/40ants/humanize-duration"))))))
