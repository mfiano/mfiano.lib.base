(in-package #:mfiano.lib.base.util)

(declaim (inline f32))
(defun f32 (x)
  (float x 1f0))

(declaim (inline f64))
(defun f64 (x)
  (float x 1d0))
