(in-package #:mfiano.lib.base.util)

;;; Aliases

(define-ftype shl (int u8) int)
(declaim (inline shl))
(defun shl (x count)
  (ash x count))

(define-ftype shr (int u8) int)
(declaim (inline shr))
(defun shr (x count)
  (ash x (- count)))

;;; Integer wrapping

(define-ftype u8 (int) u8)
(declaim (inline u8))
(defun u8 (x)
  (logand x #.max/u8))

(define-ftype u16 (int) u16)
(declaim (inline u16))
(defun u16 (x)
  (logand x #.max/u16))

(define-ftype u24 (int) u24)
(declaim (inline u24))
(defun u24 (x)
  (logand x #.max/u24))

(define-ftype u32 (int) u32)
(declaim (inline u32))
(defun u32 (x)
  (logand x #.max/u32))

(define-ftype u64 (int) u64)
(declaim (inline u64))
(defun u64 (x)
  (logand x #.max/u64))

;;; Split high/low bits

(declaim (inline split-bits))
(defun split-bits (x size low)
  (values (ldb (byte low 0) x)
          (ldb (byte (- size low) low) x)))

;;; Sign extension

(define-ftype u8->i8 (u8) i8)
(declaim (inline u8->i8))
(defun u8->i8 (x)
  (- (u8 (+ x #1=#.(1+ max/i8))) #1#))

(define-ftype u16->i16 (u16) i16)
(declaim (inline u16->i16))
(defun u16->i16 (x)
  (- (u16 (+ x #1=#.(1+ max/i16))) #1#))

(define-ftype u24->i24 (u24) i24)
(declaim (inline u24->i24))
(defun u24->i24 (x)
  (- (u24 (+ x #1=#.(1+ max/i24))) #1#))

(define-ftype u32->i32 (u32) i32)
(declaim (inline u32->i32))
(defun u32->i32 (x)
  (- (u32 (+ x #1=#.(1+ max/i32))) #1#))

(define-ftype u64->i64 (u64) i64)
(declaim (inline u64->i64))
(defun u64->i64 (x)
  (- (u64 (+ x #1=#.(1+ max/i64))) #1#))

;;; Integer addition modulo bit width

(define-ftype u8+ (int int) u8)
(declaim (inline u8+))
(defun u8+ (x y)
  (u8 (+ x y)))

(define-modify-macro u8-incf (&optional (delta 1)) u8+)

(define-ftype i8+ (int int) i8)
(declaim (inline i8+))
(defun i8+ (x y)
  (u8->i8 (u8+ x y)))

(define-modify-macro i8-incf (&optional (delta 1)) i8+)

(define-ftype u16+ (int int) u16)
(declaim (inline u16+))
(defun u16+ (x y)
  (u16 (+ x y)))

(define-modify-macro u16-incf (&optional (delta 1)) u16+)

(define-ftype i16+ (int int) i16)
(declaim (inline i16+))
(defun i16+ (x y)
  (u16->i16 (u16+ x y)))

(define-modify-macro i16-incf (&optional (delta 1)) i16+)

(define-ftype u24+ (int int) u24)
(declaim (inline u24+))
(defun u24+ (x y)
  (u24 (+ x y)))

(define-modify-macro u24-incf (&optional (delta 1)) u24+)

(define-ftype i24+ (int int) i24)
(declaim (inline i24+))
(defun i24+ (x y)
  (u24->i24 (u24+ x y)))

(define-modify-macro i24-incf (&optional (delta 1)) i24+)

(define-ftype u32+ (int int) u32)
(declaim (inline u32+))
(defun u32+ (x y)
  (u32 (+ x y)))

(define-modify-macro u32-incf (&optional (delta 1)) u32+)

(define-ftype i32+ (int int) i32)
(declaim (inline i32+))
(defun i32+ (x y)
  (u32->i32 (u32+ x y)))

(define-modify-macro i32-incf (&optional (delta 1)) i32+)

(define-ftype u64+ (int int) u64)
(declaim (inline u64+))
(defun u64+ (x y)
  (u64 (+ x y)))

(define-modify-macro u64-incf (&optional (delta 1)) u64+)

(define-ftype i64+ (int int) i64)
(declaim (inline i64+))
(defun i64+ (x y)
  (u64->i64 (u64+ x y)))

(define-modify-macro i64-incf (&optional (delta 1)) i64+)

;;; Integer subtraction modulo bit width

(define-ftype u8- (int int) u8)
(declaim (inline u8=))
(defun u8- (x y)
  (u8 (- x y)))

(define-modify-macro u8-decf (&optional (delta 1)) u8-)

(define-ftype i8- (int int) i8)
(declaim (inline i8-))
(defun i8- (x y)
  (u8->i8 (u8- x y)))

(define-modify-macro i8-decf (&optional (delta 1)) i8-)

(define-ftype u16- (int int) u16)
(declaim (inline u16-))
(defun u16- (x y)
  (u16 (- x y)))

(define-modify-macro u16-decf (&optional (delta 1)) u16-)

(define-ftype i16- (int int) i16)
(declaim (inline i16-))
(defun i16- (x y)
  (u16->i16 (u16- x y)))

(define-modify-macro i16-decf (&optional (delta 1)) i16-)

(define-ftype u24- (int int) u24)
(declaim (inline u24-))
(defun u24- (x y)
  (u24 (- x y)))

(define-modify-macro u24-decf (&optional (delta 1)) u24-)

(define-ftype i24- (int int) i24)
(declaim (inline i24-))
(defun i24- (x y)
  (u24->i24 (u24- x y)))

(define-modify-macro i24-decf (&optional (delta 1)) i24-)

(define-ftype u32- (int int) u32)
(declaim (inline u32-))
(defun u32- (x y)
  (u32 (- x y)))

(define-modify-macro u32-decf (&optional (delta 1)) u32-)

(define-ftype i32- (int int) i32)
(declaim (inline i32-))
(defun i32- (x y)
  (u32->i32 (u32- x y)))

(define-modify-macro i32-decf (&optional (delta 1)) i32-)

(define-ftype u64- (int int) u64)
(declaim (inline u64-))
(defun u64- (x y)
  (u64 (- x y)))

(define-modify-macro u64-decf (&optional (delta 1)) u64-)

(define-ftype i64- (int int) i64)
(declaim (inline i64-))
(defun i64- (x y)
  (u64->i64 (u64- x y)))

(define-modify-macro i64-decf (&optional (delta 1)) i64-)

;;; Integer multiplication modulo bit width

(define-ftype u8* (int int) u8)
(declaim (inline u8*))
(defun u8* (x y)
  (u8 (* x y)))

(define-modify-macro u8-mulf (&optional (delta 1)) u8*)

(define-ftype i8* (int int) i8)
(declaim (inline i8*))
(defun i8* (x y)
  (u8->i8 (u8* x y)))

(define-modify-macro i8-mulf (&optional (delta 1)) i8*)

(define-ftype u16* (int int) u16)
(declaim (inline u16*))
(defun u16* (x y)
  (u16 (* x y)))

(define-modify-macro u16-mulf (&optional (delta 1)) u16*)

(define-ftype i16* (int int) i16)
(declaim (inline i16*))
(defun i16* (x y)
  (u16->i16 (u16* x y)))

(define-modify-macro i16-mulf (&optional (delta 1)) i16*)

(define-ftype u24* (int int) u24)
(declaim (inline u24*))
(defun u24* (x y)
  (u24 (* x y)))

(define-modify-macro u24-mulf (&optional (delta 1)) u24*)

(define-ftype i24* (int int) i24)
(declaim (inline i24*))
(defun i24* (x y)
  (u24->i24 (u24* x y)))

(define-modify-macro i24-mulf (&optional (delta 1)) i24*)

(define-ftype u32* (int int) u32)
(declaim (inline u32*))
(defun u32* (x y)
  (u32 (* x y)))

(define-modify-macro u32-mulf (&optional (delta 1)) u32*)

(define-ftype i32* (int int) i32)
(declaim (inline i32*))
(defun i32* (x y)
  (u32->i32 (u32* x y)))

(define-modify-macro i32-mulf (&optional (delta 1)) i32*)

(define-ftype u64* (int int) u64)
(declaim (inline u64*))
(defun u64* (x y)
  (u64 (* x y)))

(define-modify-macro u64-mulf (&optional (delta 1)) u64*)

(define-ftype i64* (int int) i64)
(declaim (inline i64*))
(defun i64* (x y)
  (u64->i64 (u64* x y)))

(define-modify-macro i64-mulf (&optional (delta 1)) i64*)

;;; Bitwise logical XOR place-modifying macro

(define-modify-macro logxorf (x) logxor)

;;; Bitwise logical negation modulo bit width

(define-ftype u8~ (int) u8)
(declaim (inline u8~))
(defun u8~ (x)
  (u8 (lognot x)))

(define-ftype i8~ (int) i8)
(declaim (inline i8~))
(defun i8~ (x)
  (u8->i8 (u8~ x)))

(define-ftype u16~ (int) u16)
(declaim (inline u16~))
(defun u16~ (x)
  (u16 (lognot x)))

(define-ftype i16~ (int) i16)
(declaim (inline i16~))
(defun i16~ (x)
  (u16->i16 (u16~ x)))

(define-ftype u24~ (int) u24)
(declaim (inline u24~))
(defun u24~ (x)
  (u24 (lognot x)))

(define-ftype i24~ (int) i24)
(declaim (inline i24~))
(defun i24~ (x)
  (u24->i24 (u24~ x)))

(define-ftype u32~ (int) u32)
(declaim (inline u32~))
(defun u32~ (x)
  (u32 (lognot x)))

(define-ftype i32~ (int) i32)
(declaim (inline i32~))
(defun i32~ (x)
  (u32->i32 (u32~ x)))

(define-ftype u64~ (int) u64)
(declaim (inline u64~))
(defun u64~ (x)
  (u64 (lognot x)))

(define-ftype i64~ (int) i64)
(declaim (inline i64~))
(defun i64~ (x)
  (u64->i64 (u64~ x)))

;;; Left arithmetic shift modulo bit width

(define-ftype u8-shl (int u8) u8)
(declaim (inline u8-shl))
(defun u8-shl (x count)
  (u8 (shl x count)))

(define-ftype i8-shl (int u8) i8)
(declaim (inline i8-shl))
(defun i8-shl (x count)
  (u8->i8 (u8-shl x count)))

(define-ftype u16-shl (int u8) u16)
(declaim (inline u16-shl))
(defun u16-shl (x count)
  (u16 (shl x count)))

(define-ftype i16-shl (int u8) i16)
(declaim (inline i16-shl))
(defun i16-shl (x count)
  (u16->i16 (u16-shl x count)))

(define-ftype u24-shl (int u8) u24)
(declaim (inline u24-shl))
(defun u24-shl (x count)
  (u24 (shl x count)))

(define-ftype i24-shl (int u8) i24)
(declaim (inline i24-shl))
(defun i24-shl (x count)
  (u24->i24 (u24-shl x count)))

(define-ftype u32-shl (int u8) u32)
(declaim (inline u32-shl))
(defun u32-shl (x count)
  (u32 (shl x count)))

(define-ftype i32-shl (int u8) i32)
(declaim (inline i32-shl))
(defun i32-shl (x count)
  (u32->i32 (u32-shl x count)))

(define-ftype u64-shl (int u8) u64)
(declaim (inline u64-shl))
(defun u64-shl (x count)
  (u64 (shl x count)))

(define-ftype i64-shl (int u8) i64)
(declaim (inline i64-shl))
(defun i64-shl (x count)
  (u64->i64 (u64-shl x count)))

;;; Left bitwise rotation modulo bit width

(define-ftype u8-rotl (int u8) u8)
(declaim (inline u8-rotl))
(defun u8-rotl (x count)
  (logior (u8-shl x count) (shr x (- 8 count))))

(define-modify-macro u8-rotlf (count) u8-rotl)

(define-ftype i8-rotl (int u8) i8)
(declaim (inline i8-rotl))
(defun i8-rotl (x count)
  (u8->i8 (u8-rotl x count)))

(define-modify-macro i8-rotlf (count) i8-rotl)

(define-ftype u16-rotl (int u8) u16)
(declaim (inline u16-rotl))
(defun u16-rotl (x count)
  (logior (u16-shl x count) (shr x (- 16 count))))

(define-modify-macro u16-rotlf (count) u16-rotl)

(define-ftype i16-rotl (int u8) i16)
(declaim (inline i16-rotl))
(defun i16-rotl (x count)
  (u16->i16 (u16-rotl x count)))

(define-modify-macro i16-rotlf (count) i16-rotl)

(define-ftype u24-rotl (int u8) u24)
(declaim (inline u24-rotl))
(defun u24-rotl (x count)
  (logior (u24-shl x count) (shr x (- 24 count))))

(define-modify-macro u24-rotlf (count) u24-rotl)

(define-ftype i24-rotl (int u8) i24)
(declaim (inline i24-rotl))
(defun i24-rotl (x count)
  (u24->i24 (u24-rotl x count)))

(define-modify-macro i24-rotlf (count) i24-rotl)

(define-ftype u32-rotl (int u8) u32)
(declaim (inline u32-rotl))
(defun u32-rotl (x count)
  (logior (u32-shl x count) (shr x (- 32 count))))

(define-modify-macro u32-rotlf (count) u32-rotl)

(define-ftype i32-rotl (int u8) i32)
(declaim (inline i32-rotl))
(defun i32-rotl (x count)
  (u32->i32 (u32-rotl x count)))

(define-modify-macro i32-rotlf (count) i32-rotl)

(define-ftype u64-rotl (int u8) u64)
(declaim (inline u64-rotl))
(defun u64-rotl (x count)
  (logior (u64-shl x count) (shr x (- 64 count))))

(define-modify-macro u64-rotlf (count) u64-rotl)

(define-ftype i64-rotl (int u8) i64)
(declaim (inline i64-rotl))
(defun i64-rotl (x count)
  (u64->i64 (u64-rotl x count)))

(define-modify-macro i64-rotlf (count) i64-rotl)

;;; Wide multiplication, returning high and low bits as separate values

(declaim (inline wide*))
(defun wide* (x y size)
  (multiple-value-bind (low-size rem) (floor size 2)
    (multiple-value-bind (low1 high1) (split-bits x size low-size)
      (multiple-value-bind (low2 high2) (split-bits y size low-size)
        (let ((mid1 (* high1 low2))
              (mid2 (* high2 low1))
              (high-size (+ (- size low-size) rem)))
          (values (ldb (byte size 0)
                       (+ (* low1 low2)
                          (ldb (byte size 0) (shl mid1 low-size))
                          (ldb (byte size 0) (shl mid2 low-size))))
                  (ldb (byte size 0)
                       (+ (* high1 high2)
                          (ldb (byte high-size low-size) mid1)
                          (ldb (byte high-size low-size) mid2)))))))))
