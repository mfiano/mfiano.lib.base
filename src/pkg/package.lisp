(defpackage #:mfiano.lib.base.pkg
  (:use #:cl)
  #+(or acl allegro ccl clasp ecl lispworks sbcl)
  (:import-from
   #+acl #:ext
   #+allegro #:excl
   #+ccl #:ccl
   #+clasp #:ext
   #+ecl #:ext
   #+lispworks #:hcl
   #+sbcl #:sb-ext
   #:add-package-local-nickname
   #:package-local-nicknames
   #:remove-package-local-nickname)
  (:export #:define-package))

(in-package #:mfiano.lib.base.pkg)

(define-condition define-package-error (error)
  ((%package-name :reader %package-name :initarg :package)
   (%option-name :reader option-name :initarg :option)
   (%message :reader message :initarg :message))
  (:report
   (lambda (condition stream)
     (format stream
             "Error in option ~s of package definition ~a:~%~%~a"
             (option-name condition)
             (%package-name condition)
             (message condition)))))

(defun assert-package-exists (from to option)
  (unless (find-package from)
    (let ((message (format nil "Package ~a is not defined." from)))
      (error 'define-package-error :package to :option option :message message))))

(defun assert-symbol-accessible (from to name option)
  (unless (nth-value 1 (find-symbol (string name) from))
    (let ((message (format nil "~a is not accessible in ~a." name from)))
      (error 'define-package-error :package to :option option :message message))))

(defun assert-symbol-external (from to name option)
  (unless (eq (nth-value 1 (find-symbol (string name) from)) :external)
    (let ((message (format nil "~a is not external to ~a." name from)))
      (error 'define-package-error :package to :option option :message message))))

(defgeneric option (option package &rest args)
  (:method (option package &rest args)
    (declare (ignore args))
    (error "Invalid package option ~s for package ~a." option package)))

(defmethod option ((option (eql :documentation)) package &rest args)
  (setf (documentation (find-package package) t) (first args)))

(defmethod option ((option (eql :nicknames)) package &rest args)
  (rename-package package package args))

#+(or acl allegro ccl clasp ecl lispworks sbcl)
(defmethod option ((option (eql :local-nicknames)) package &rest args)
  (dolist (old-mapping (package-local-nicknames package))
    (remove-package-local-nickname (first old-mapping) package))
  (dolist (new-mapping args)
    (destructuring-bind (local-nickname actual-package) new-mapping
      (assert-package-exists actual-package package option)
      (add-package-local-nickname local-nickname actual-package package))))

(defmethod option ((option (eql :shadow)) package &rest args)
  (shadow args package))

(defmethod option ((option (eql :intern)) package &rest args)
  (dolist (name args)
    (intern (string name) package)))

(defmethod option ((option (eql :size)) package &rest args)
  (declare (ignore args)))

(defmethod option ((option (eql :use)) package &rest args)
  (let* ((to-use (mapcar #'find-package args))
         (to-unuse (set-difference (package-use-list package) to-use)))
    (dolist (from args)
      (assert-package-exists from package option))
    (unuse-package to-unuse package)
    (use-package args package)))

(defmethod option ((option (eql :export)) package &rest args)
  (export (mapcar (lambda (x) (intern (string x) package)) args) package))

(defmethod option ((option (eql :import-from)) package &rest args)
  (destructuring-bind (from . names) args
    (assert-package-exists from package option)
    (dolist (name names)
      (assert-symbol-accessible from package name option))
    (import (mapcar (lambda (x) (find-symbol (string x) from)) names) (find-package package))))

(defmethod option ((option (eql :import)) package &rest args)
  (dolist (from args)
    (let ((names nil))
      (do-external-symbols (name from)
        (push name names))
      (apply #'option :import-from package from names))))

(defmethod option ((option (eql :shadowing-import-from)) package &rest args)
  (destructuring-bind (from . names) args
    (assert-package-exists from package option)
    (dolist (name names)
      (assert-symbol-accessible from package name option))
    (shadowing-import (mapcar (lambda (x) (find-symbol (string x) from)) names) package)))

(defmethod option ((option (eql :inherit-from)) package &rest args)
  (apply #'option :shadowing-import-from package args)
  (apply #'option :export package (cdr args)))

(defmethod option ((option (eql :inherit)) package &rest args)
  (dolist (from args)
    (let ((names nil))
      (do-external-symbols (name from)
        (push name names))
      (apply #'option :inherit-from package from names))))

(defmacro define-package (name &body options)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (unless (find-package ',name)
       (make-package ',name :use nil))
     ,@(mapcar
        (lambda (x)
          (destructuring-bind (option . args) x
            `(option ',option ',name ,@(mapcar (lambda (x) `',x) args))))
        options)))
