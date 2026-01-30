(in-package #:mfiano.lib.base.util)

(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(defmacro define-ftype (name args values)
  `(declaim (ftype (function ,args ,values) ,name)))

(defmacro define-printer ((object stream &key type identity) &body body)
  `(defmethod print-object ((,object ,object) ,stream)
     (print-unreadable-object (,object ,stream :type ,type :identity ,identity)
       ,@body)))

(defmacro do-each ((var sequence) &body body)
  `(map nil (lambda (,var) ,@body) ,sequence))

(defmacro while (predicate &body body)
  `(loop :while ,predicate :do (progn ,@body)))

(defmacro until (predicate &body body)
  `(loop :until ,predicate :do (progn ,@body)))
