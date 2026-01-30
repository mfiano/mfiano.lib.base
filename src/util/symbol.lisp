(in-package #:mfiano.lib.base.util)

(defun non-nil-symbol? (x)
  (typep x 'non-nil-symbol))

(defun make-keyword (name)
  (values (intern (string name) :keyword)))

(define-ftype last-symbol-char? (symbol character) boolean)
(defun last-symbol-char? (symbol char)
  (declare (optimize (speed 3)))
  (last-char? (symbol-name symbol) char))

(define-ftype symbol-accessible? (a:string-designator package-designator) boolean)
(defun symbol-accessible? (name package)
  (when (nth-value 1 (find-symbol (string name) package))
    t))

(define-ftype symbol-external? (a:string-designator package-designator) boolean)
(defun symbol-external? (name package)
  (eq (nth-value 1 (find-symbol (string name) package)) :external))

(defun collect-symbols (package &key key test)
  (let ((key (or key #'identity))
        (test (or test (constantly t)))
        (symbols nil))
    (do-symbols (symbol package)
      (when (funcall test symbol)
        (push (funcall key symbol) symbols)))
    (nreverse symbols)))

(defun collect-external-symbols (package &key key test)
  (let ((key (or key #'identity))
        (test (or test (constantly t)))
        (symbols nil))
    (do-external-symbols (symbol package)
      (when (funcall test symbol)
        (push (funcall key symbol) symbols)))
    (nreverse symbols)))
