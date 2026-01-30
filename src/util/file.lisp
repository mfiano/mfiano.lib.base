(in-package #:mfiano.lib.base.util)

(defmacro with-file-input ((stream path) &body body)
  `(with-open-file (,stream ,path :direction :input :if-does-not-exist :error)
     ,@body))

(defmacro with-file-output ((stream path &optional append-p) &body body)
  `(with-open-file (,stream
                    ,path
                    :direction :output
                    :if-exists ,(if append-p :append :supersede)
                    :if-does-not-exist :create)
     ,@body))

(defmacro with-binary-input ((stream file) &body body)
  `(with-open-file (,stream ,file :direction :input :if-does-not-exist :error :element-type 'u8)
     ,@body))

(defmacro with-binary-output ((stream file &optional append-p) &body body)
  `(with-open-file (,stream
                    ,file
                    :direction :output
                    :if-exists ,(if append-p :append :supersede)
                    :if-does-not-exist :create
                    :element-type 'u8)
     ,@body))

(defun map-files (path func &key test (recursive t))
  (let ((test (or test (constantly t))))
    (flet ((process (dir)
             (dolist (x (uiop:directory-files dir))
               (when (funcall test x)
                 (funcall func x)))))
      (let ((path (uiop:ensure-directory-pathname path)))
        (uiop:collect-sub*directories path t recursive #'process)))))

(defun collect-files (path &key test (recursive t))
  (let ((files nil))
    (map-files path (lambda (x) (push x files)) :test test :recursive recursive)
    files))
