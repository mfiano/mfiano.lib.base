(in-package #:mfiano.lib.base.util)

(define-ftype last-char? (string character) boolean)
(defun last-char? (string char)
  (char= (char string (1- (length string))) char))

(define-ftype string-merge (&rest string) string)
(defun string-merge (&rest strings)
  (declare (optimize (speed 3)))
  (apply #'concatenate 'string strings))

(define-ftype string-empty? (string-designator) boolean)
(defun string-empty? (string)
  (declare (optimize (speed 3)))
  (and (stringp string) (string= string "")))

(define-ftype string-non-empty? (string-designator) boolean)
(defun string-non-empty? (string)
  (declare (optimize (speed 3)))
  (and (stringp string) (not (string-empty? string))))

(define-ftype string-split (string) list)
(defun string-split (string)
  (declare (optimize (speed 3)))
  (remove "" (uiop:split-string string :separator '(#\space)) :test #'string=))

(define-ftype string-trim-whitespace (string) string)
(defun string-trim-whitespace (string)
  (let ((chars '(#\space #\tab #\linefeed #\return #\newline #\page)))
    (string-trim chars string)))

(define-ftype string-starts-with? (string string) boolean)
(defun string-starts-with? (string prefix)
  (let ((prefix-length (length prefix)))
    (when (<= prefix-length (length string))
      (string= prefix (subseq string 0 prefix-length)))))

(define-ftype string-ends-with? (string string) boolean)
(defun string-ends-with? (string suffix)
  (let ((length (length string))
        (suffix-length (length suffix)))
    (when (<= suffix-length length)
      (string= suffix (subseq string (- length suffix-length) length)))))
