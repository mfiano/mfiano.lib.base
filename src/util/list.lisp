(in-package #:mfiano.lib.base.util)

(defmacro do-alist ((key value alist) &body body)
  `(loop :for (,key . ,value) :in ,alist :do ,@body))

(defun alist-keys (alist)
  (let ((keys nil))
    (do-alist (k v alist)
      (push k keys))
    (nreverse keys)))

(defun alist-values (alist)
  (let ((values nil))
    (do-alist (k v alist)
      (push v values))
    (nreverse values)))

(defun alist->hash-table (alist &rest args)
  (apply #'a:alist-hash-table alist args))

(defun alist->plist (alist)
  (a:alist-plist alist))

(defun alist? (x)
  (and (typep x 'a:proper-list) (every #'consp x)))

(defmacro do-plist ((key value plist) &body body)
  `(loop :for (,key ,value) :on ,plist :by #'cddr :do ,@body))

(defun plist-keys (plist)
  (let ((keys nil))
    (do-plist (k v plist)
      (push k keys))
    (nreverse keys)))

(defun plist-values (plist)
  (let ((values nil))
    (do-plist (k v plist)
      (push v values))
    (nreverse values)))

(defun plist->hash-table (plist &rest args)
  (apply #'a:plist-hash-table plist args))

(defun plist->alist (plist)
  (a:plist-alist plist))

(defun plist-remove (plist &rest keys)
  (loop :for (key value) :on plist :by #'cddr
        :unless (member key keys :test #'equal)
          :collect key :into result
          :and :collect value :into result
        :finally (return result)))

(defun plist? (x)
  (and (typep x 'a:proper-list) (evenp (length x)) (every (lambda (x) (eq x x)) (plist-keys x))))
