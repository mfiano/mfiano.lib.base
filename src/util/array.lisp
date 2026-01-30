(in-package #:mfiano.lib.base.util)

(declaim (inline make-bit-array))
(defun make-bit-array (dims &optional (value 0))
  (make-array dims :element-type 'bit :initial-element value))

(declaim (inline make-u8-array))
(defun make-u8-array (dims &optional (value 0))
  (make-array dims :element-type 'u8 :initial-element value))

(declaim (inline make-u16-array))
(defun make-u16-array (dims &optional (value 0))
  (make-array dims :element-type 'u16 :initial-element value))

(declaim (inline make-u24-array))
(defun make-u24-array (dims &optional (value 0))
  (make-array dims :element-type 'u24 :initial-element value))

(declaim (inline make-u32-array))
(defun make-u32-array (dims &optional (value 0))
  (make-array dims :element-type 'u32 :initial-element value))

(declaim (inline make-u64-array))
(defun make-u64-array (dims &optional (value 0))
  (make-array dims :element-type 'u64 :initial-element value))

(declaim (inline make-i8-array))
(defun make-i8-array (dims &optional (value 0))
  (make-array dims :element-type 'i8 :initial-element value))

(declaim (inline make-i16-array))
(defun make-i16-array (dims &optional (value 0))
  (make-array dims :element-type 'i16 :initial-element value))

(declaim (inline make-i24-array))
(defun make-i24-array (dims &optional (value 0))
  (make-array dims :element-type 'i24 :initial-element value))

(declaim (inline make-i32-array))
(defun make-i32-array (dims &optional (value 0))
  (make-array dims :element-type 'i32 :initial-element value))

(declaim (inline make-i64-array))
(defun make-i64-array (dims &optional (value 0))
  (make-array dims :element-type 'i64 :initial-element value))

(declaim (inline make-fixnum-array))
(defun make-fixnum-array (dims &optional (value 0))
  (make-array dims :element-type 'fixnum :initial-element value))

(declaim (inline make-f32-array))
(defun make-f32-array (dims &optional (value 0f0))
  (make-array dims :element-type 'f32 :initial-element value))

(declaim (inline make-f64-array))
(defun make-f64-array (dims &optional (value 0d0))
  (make-array dims :element-type 'f64 :initial-element value))

(declaim (inline to-bit-array))
(defun to-bit-array (x)
  (replace (make-bit-array (length x)) x))

(declaim (inline to-i8-array))
(defun to-i8-array (x)
  (replace (make-i8-array (length x)) x))

(declaim (inline to-i16-array))
(defun to-i16-array (x)
  (replace (make-i16-array (length x)) x))

(declaim (inline to-i24-array))
(defun to-i24-array (x)
  (replace (make-i24-array (length x)) x))

(declaim (inline to-i32-array))
(defun to-i32-array (x)
  (replace (make-i32-array (length x)) x))

(declaim (inline to-i64-array))
(defun to-i64-array (x)
  (replace (make-i64-array (length x)) x))

(declaim (inline to-u8-array))
(defun to-u8-array (x)
  (replace (make-u8-array (length x)) x))

(declaim (inline to-u16-array))
(defun to-u16-array (x)
  (replace (make-u16-array (length x)) x))

(declaim (inline to-u24-array))
(defun to-u24-array (x)
  (replace (make-u24-array (length x)) x))

(declaim (inline to-u32-array))
(defun to-u32-array (x)
  (replace (make-u32-array (length x)) x))

(declaim (inline to-u64-array))
(defun to-u64-array (x)
  (replace (make-u64-array (length x)) x))

(declaim (inline to-fixnum-array))
(defun to-fixnum-array (x)
  (replace (make-fixnum-array (length x)) x))

(declaim (inline to-f32-array))
(defun to-f32-array (x)
  (replace (make-f32-array (length x)) x))

(declaim (inline to-f64-array))
(defun to-f64-array (x)
  (replace (make-f64-array (length x)) x))
