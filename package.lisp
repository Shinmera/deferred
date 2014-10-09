#|
 This file is a part of Deferred
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:deferred
  (:nicknames #:org.tymoonnext.deferred)
  (:use #:cl)
  ;; deferred.lisp
  (:export
   #:*deferred-package*
   #:process
   #:with-deferred-library
   #:process-compile
   #:when-packages))

