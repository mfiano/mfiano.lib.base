(in-package #:mfiano.lib.base.util)

(defmacro do-hash-table ((key value table) &body body)
  `(block nil
     (maphash
      (lambda (,key ,value)
        ,@body)
      ,table)))

(defmacro do-hash-keys ((key table) &body body)
  (with-gensyms (value)
    `(do-hash-table (,key ,value ,table)
       (declare (ignore ,value))
       ,@body)))

(defmacro do-hash-values ((value table) &body body)
  (with-gensyms (key)
    `(do-hash-table (,key ,value ,table)
       (declare (ignore ,key))
       ,@body)))

(define-ftype hash-table->alist (hash-table) list)
(defun hash-table->alist (table)
  (declare (optimize (speed 3)))
  (let ((alist nil))
    (do-hash-table (k v table)
      (push (cons k v) alist))
    alist))

(define-ftype hash-table->plist (hash-table) list)
(defun hash-table->plist (table)
  (declare (optimize (speed 3)))
  (let ((plist nil))
    (do-hash-table (k v table)
      (setf plist (list* k v plist)))
    plist))

(define-ftype merge-hash-tables (hash-table &rest hash-table) hash-table)
(defun merge-hash-tables (table &rest tables)
  (declare (optimize (speed 3)))
  (reduce
   (lambda (x y)
     (unless (eql (hash-table-test x) (hash-table-test y))
       (error "Hash tables must have the same test function."))
     (do-hash-table (k v y)
       (setf (gethash k x) v))
     x)
   tables
   :initial-value (copy-hash-table table)))

(define-ftype hash-table-keys (hash-table) list)
(defun hash-table-keys (table)
  (let ((keys nil))
    (a:maphash-keys (lambda (x) (push x keys)) table)
    keys))

(define-ftype hash-table-values (hash-table) list)
(defun hash-table-values (table)
  (let ((values nil))
    (a:maphash-values (lambda (x) (push x values)) table)
    values))
