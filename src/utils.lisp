(defpackage #:example/utils
  (:use #:cl)
  (:import-from #:mgl-pax
                #:defsection)
  (:export #:do-the-job)
  (:documentation "The utils"))
(in-package example/utils)


(defsection @utils (:title "EXAMPLE/UTILS Package")
  (do-the-job function))


(defun concat (first second)
  "This function is not exported and should not be showed in the API reference."
  (format nil "~A ~A" first second))


(defun do-the-job (first second)
  "The function does the job.

   It **concatenates** first and second arguments
   calling internal function concat.

   On this multiline we'll check how does documentation
   system processes docstrings.

   By the way, pay attention at the second paragraph where
   I've used [Markdown](https://www.markdownguide.org/basic-syntax/)
   format to make the word \"concatenates\" bold.

   Also, we can reference some parts of the documentation.
   Read more about cross referencing in the EXAMPLE-DOCS:@HANDWRITTEN chapter."
  (concat first second))
