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

