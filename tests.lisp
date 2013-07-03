(defpackage :name.hijarian.point-of-sale
  (:use :common-lisp :stefil))

(in-package :name.hijarian.point-of-sale)

(defparameter *emitted-string* nil)

(defun emitted-string ()
  "Checks what string was emitted to external display"
  (if (null *emitted-string*)
      "Invalid barcode!"
      *emitted-string*))

(defun on-barcode (point-of-sale barcode)
  "Event handler which handles on the occasion of the barcode being sent to the point of sale"
  (if (equal barcode "123456")
      (progn
        (setf *emitted-string* "$10.35")
        "$10.35")
      nil))

(defun make-point-of-sale ()
  "Factory function to properly create a point of sale"
  nil)

(defmacro cost-of (barcode is-keyword price in-keyword point-of-sale)
  "Pretty macro to associate a price with a given barcode. is-keyword and in-keyword are a syntax sugar."
  t)

(defsuite all-tests)
(in-suite all-tests)

(deftest empty-string-is-invalid ()
  (let ((point-of-sale (make-point-of-sale)))
    (on-barcode point-of-sale "") 
    (is (equal (emitted-string) "Invalid barcode!"))))

(deftest need-proper-barcode-and-item-price-to-emit-price ()
  (let ((point-of-sale (make-point-of-sale)))
    (cost-of "123456" is "$10.35" in point-of-sale)
    (on-barcode point-of-sale "123456")
    (is (equal (emitted-string) "$10.35"))))

