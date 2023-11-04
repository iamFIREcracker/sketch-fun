(defpackage #:3am
  (:use #:cl)
  (:import-from #:1am #:is #:signals #:run #:*tests*)
  (:export #:test #:examples #:expands1 #:is #:signals #:signals-nothing #:run #:*tests*))
(in-package #:3am)

(defmacro test (name &body body)
  "Define a test function, add it to `*tests*' and then run it immediately.

  Note: the test is immediately run only if the macro is directly evaluated in
  the REPL, i.e., if neither `*compile-file-pathname*' nor `*load-pathname*'
  are set."
  `(prog1 (1am:test ,name ,@body)
    (unless (or *compile-file-pathname* *load-pathname*)
      (,name))))

(defmacro examples (name &body body)
  "Like TEST, except it prepends TEST- to `name'."
  (let ((name (intern (string-upcase (format nil "test-~A" name)))))
    `(test ,name ,@body)))

(defmacro expands1 (form expansion)
  "Assert that `form' expands (in the MACROEXPAND-1 sense) to `expansion'."
 `(is (equal (macroexpand-1 ',form) ',expansion)))

(defun %signals-nothing (fn)
  (handler-case (progn (funcall fn)
                       (1am::passed))
    (condition (c)
      (error "Expexted no signaled condition, but got ~s instead." c))))

(defmacro signals-nothing (&body body)
  "Asserts that `body' does not signal any condition at all."
  `(%signals-nothing (lambda () ,@body)))
