#|
 This file is a part of Deferred
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.deferred)

(defvar *identifier-symbol* (gensym "DEFERRED")
  "Symbol used at the beginning of a list to identify a deferred symbol construct.
READ-DEFERRED and PROCESS use these to handle the transformations.")
(defvar *deferred-package* NIL
  "The default package to use for deferred symbols.")

(defun terminating-p (char)
  "Returns NIL if the character is not considered to be a symbol-terminating character."
  (member char '(NIL #\Space #\Newline #\Return #\Linefeed #\Tab #\Page #\( #\) #\` #\' #\#)))

(defun transform-token (token-stream)
  "Transforms the TOKEN-STREAM into the proper stream, minding the current READTABLE-CASE."
  (let ((s (get-output-stream-string token-stream)))
    (ecase (readtable-case *readtable*)
      (:UPCASE (string-upcase s))
      (:DOWNCASE (string-downcase s))
      (:PRESERVE s)
      ;; If you seriously want this for some reason,
      ;; write this part yourself.
      (:INVERT (error "Screw this.")))))

(defun read-deferred (s a c)
  "Reads a deferred symbol from the reader stream and outputs the appropriate data structure."
  (declare (ignore a c))
  (let ((token (make-string-output-stream))
        (results ()))
    ;; Read tokens delimited by colons until some kind of end is reached.
    (loop  for peek = (peek-char NIL s NIL NIL)
           until (terminating-p peek)
           do (read-char s) ;; consume it
              (if (char= peek #\:)
                  (progn (push (transform-token token) results)
                         (setf token (make-string-output-stream)))
                  (write-char peek token)))
    (cons *identifier-symbol* (cons (transform-token token) results))))

(set-dispatch-macro-character #\# #\^ #'read-deferred)
(named-readtables:defreadtable :deferred
  (:merge :standard)
  (:dispatch-macro-char #\# #\^ #'read-deferred))

(defgeneric process (thing)
  (:documentation "Processes THING to transform all deferred symbols within into their proper representations.")
  (:method (thing)
    thing)
  (:method ((list list))
    (cond
      ((and (listp (first list)) (eql (caar list) *identifier-symbol*))
       ;; We got us a function call!
       (destructuring-bind (ident name &optional (package *deferred-package*)) (car list)
         (declare (ignore ident))
         `(funcall (symbol-function (find-symbol ,name ,package))
                   ,@(mapcar #'process (rest list)))))
      ((eql (car list) *identifier-symbol*)
       ;; We got us a value reference!
       (destructuring-bind (ident name &optional (package *deferred-package*)) list
         (declare (ignore ident))
         `(symbol-value (find-symbol ,name ,package))))
      ((and (eql (first list) 'FUNCTION) (listp (second list)) (eql (caadr list) *identifier-symbol*))
       ;; We got us a function reference!
       (destructuring-bind (ident name &optional (package *deferred-package*)) (second list)
         (declare (ignore ident))
         `(symbol-function (find-symbol ,name ,package))))
      ((and (eql (first list) 'QUOTE) (listp (second list)) (eql (caadr list) *identifier-symbol*))
       ;; We got us a symbol reference!
       (destructuring-bind (ident name &optional (package *deferred-package*)) (second list)
         (declare (ignore ident))
         `(find-symbol ,name ,package)))
      ((eql (first list) 'SETF)
       ;; You could approximate the proper behaviour somewhat by at least searching for
       ;; (SETF FOO) function definitions, but if it were a full setf expander macro,
       ;; there's not much we could do anyway. We'll just error instead.
       (mapcar #'(lambda (item)
                   (when (and (listp item) (listp (first item)) (eql (caar item) *identifier-symbol*))
                     (error "SETF-ing of deferred accessors is not supported."))
                   (process item)) list))
      (T
       (mapcar #'process list)))))

(defmacro with-deferred-library ((asdf-system &key (package asdf-system) (if-not-loaded :auto)) &body body)
  "Turns the body into one capable of using deferred symbols.

Specifying deferred symbols works just like other symbols, except prefixing them with #^ .
If the #^ reader macro has been overwritten in the standard readtable, please use
 (named-readtables:in-readtable :deferred)

ASDF-SYSTEM   --- An ASDF system designator.
PACKAGE       --- A package designator to use as the default package for deferred symbols.
IF-NOT-LOADED ::= NIL | :QUICKLOAD | :LOAD | :AUTO | T
                  NIL will simply skip executing the body if the library has not been loaded.
                  :LOAD will try to use ASDF:LOAD-SYSTEM and :QUICKLISP QL:QUICKLOAD.
                  :AUTO or T will try to do the right thing by checking for Quicklisp and ASDF
                  presence first. If neither can be found, an error is signalled. If the loading
                  operation fails, a warning is signalled and the body is NOT executed.

The only place I can think of that deferred symbols are currently not supported for is SETF
accessors. This is left out since there's no way to defer SETF-expanders to runtime.
You can still SETF symbol-values (special variables f.e.), but an error is signalled if
a deferred function is used within SETF."
  (let ((*deferred-package* (string package))
        (block (gensym "BLOCK"))
        (asdf (gensym "ASDF")))
    `(block ,block
       (let ((,asdf ,asdf-system))
         (unless (asdf:component-loaded-p ,asdf)
           ,(ecase if-not-loaded
              (NIL `(return-from ,block))
              (:load `(asdf:load-system ,asdf))
              (:quickload `(ql:quickload ,asdf))
              ((:auto T)
               `(handler-case
                    ,(cond ((member :quicklisp *features*)
                            `(ql:quickload ,asdf))
                           ((member :asdf *features*)
                            `(asdf:load-system ,asdf))
                           (T `(error "ASDF not present.")))
                  (error (err)
                    (warn "Could not load optional library system ~a (~a)" ,asdf err)
                    (return-from ,block)))))))
       ,@(process body))))
