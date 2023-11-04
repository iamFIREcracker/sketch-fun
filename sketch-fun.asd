(if (not (asdf:locate-system :ml))
  (pushnew (merge-pathnames (parse-namestring "vendor/ml/")
                            *default-pathname-defaults*)
           asdf:*central-registry*))
(if (not (asdf:locate-system :3am))
  (pushnew (merge-pathnames (parse-namestring "vendor/3am/")
                            *default-pathname-defaults*)
           asdf:*central-registry*))

(defsystem #:sketch-fun
  :description "Fun little Sketch experiments"

  :author "Matteo Landi <matteo@matteolandi.net>"
  :license  "MIT"

  :version "0.0.0"

  :depends-on (
                 #:3am
                 #:ml

                 #:sketch
               )

  :class :package-inferred-system
  :pathname "src")
