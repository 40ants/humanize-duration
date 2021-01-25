(defpackage #:example/app
  (:use #:cl)
  (:import-from #:example/utils)
  (:import-from #:mgl-pax
                #:defsection)
  (:documentation "This is docstring for the package.

                   The package contains a function which does it's job by
                   applying transformation to the first and second arguments.

                   Note, despite the docstring indentation, it is displayed
                   correctly. And PAX is smart enough to distinguish the common
                   indentation and indentation of a code block below:

                       (apply #'foo 42)
                  "))
(in-package example/app)


(defsection @app (:title "EXAMPLE/APP Package")
  "This is a test package of our example.

   It contains only one function definition:"
  
  (foo function)
  
  "What is interesting is that we can continue writing code for the section.
   Interleaving blocks of text with references to the different Lisp entities.

   At some moment we can reference objects from other packages. But to make them
   cross-referenced, we have to mention them in some section. For example, here FOO
   references EXAMPLE/UTILS:DO-THE-JOB, which is desribed in the EXAMPLE/UTILS:@UTILS.")


(defun foo (first &key (other 100500))
  "This is example function.

   Internally it calls [example/utils:do-the-job][function]
   to do the real job."
  (example/utils:do-the-job first other))
