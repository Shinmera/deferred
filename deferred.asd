#|
 This file is a part of Deferred
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.tymoonnext.deferred.asdf
  (:use #:cl #:asdf))
(in-package #:org.tymoonnext.deferred.asdf)

(defsystem deferred
  :name "Deferred"
  :version "0.2.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A simple library allowing you to write code using deferred libraries."
  :homepage "https://github.com/Shinmera/deferred/"
  :serial T
  :components ((:file "package")
               (:file "deferred"))
  :depends-on (:named-readtables))
