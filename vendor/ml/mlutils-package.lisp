(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (UNLESS (FIND-PACKAGE "MLUTILS")
    (DEFPACKAGE "MLUTILS"
      (:DOCUMENTATION "Package that contains Quickutil utility functions.")
      (:USE :CL))))
(IN-PACKAGE "MLUTILS")

(DEFPARAMETER *UTILITIES* NIL)