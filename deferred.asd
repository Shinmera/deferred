(defsystem deferred
  :name "Deferred"
  :version "0.9.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A simple library allowing you to write code using deferred libraries."
  :homepage "https://Shinmera.github.io/deferred//"
  :bug-tracker "https://github.com/Shinmera/deferred//issues"
  :source-control (:git "https://github.com/Shinmera/deferred/.git")
  :serial T
  :components ((:file "package")
               (:file "deferred"))
  :depends-on (:named-readtables))
