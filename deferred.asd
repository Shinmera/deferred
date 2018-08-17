#|
 This file is a part of Deferred
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defsystem deferred
  :name "Deferred"
  :version "0.9.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A simple library allowing you to write code using deferred libraries."
  :homepage "https://Shinmera.github.io/deferred//"
  :bug-tracker "https://github.com/Shinmera/deferred//issues"
  :source-control (:git "https://github.com/Shinmera/deferred/.git")
  :serial T
  :components ((:file "package")
               (:file "deferred"))
  :depends-on (:named-readtables))
