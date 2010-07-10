(defsystem :robotime
  :name "robotime"
  :author "acieroid"
  :license "MIT"
  :depends-on (:until-it-dies :alexandria)
  :components ((:file "package")
               (:file "board" :depends-on ("package"))
               (:file "graphic" :depends-on ("package"))
               (:file "entity" :depends-on ("graphic"))
               (:file "engine" :depends-on ("entity" "board"))))