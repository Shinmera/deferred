(defsystem deferred
  :name "Deferred"
  :version "0.9.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A simple library allowing you to write code using deferred libraries."
  :homepage "https://shinmera.com/docs/deferred//"
  :bug-tracker "https://shinmera.com/project/deferred//issues"
  :source-control (:git "https://shinmera.com/project/deferred/.git")
  :serial T
  :components ((:file "package")
               (:file "deferred"))
  :depends-on (:named-readtables))
