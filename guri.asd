(defsystem "guri"
  :version "0.1.0"
  :author "Dmitrii Kosenkov"
  :license "MIT"
  :depends-on ("cl-glib" "alexandria" "documentation-utils")
  :description "URL Parser/Builder system"
  :homepage "https://github.com/Junker/guri"
  :source-control (:git "https://github.com/Junker/guri.git")
  :serial t
  :components ((:file "package")
               (:file "guri")
               (:file "docs")))
