(defpackage :name.hijarian.point-of-sale
  (:use :common-lisp :stefil))

(in-package :name.hijarian.point-of-sale)

(defparameter *emitted-string* nil)

(defparameter *prices-source* (make-hash-table :test #'equal))

(defun emitted-string ()
  "Checks what string was emitted to external display"
  (if (null *emitted-string*)
      "Invalid barcode!"
      *emitted-string*))

(defun on-barcode (point-of-sale barcode)
  "Event handler which handles on the occasion of the barcode being sent to the point of sale"
  (let ((price (gethash barcode *prices-source*)))
    (setf *emitted-string* price)))

(defun make-point-of-sale ()
  "Factory function to properly create a point of sale"
  nil)

(defmacro cost-of (barcode is-keyword price in-keyword point-of-sale)
  "Pretty macro to associate a price with a given barcode. is-keyword and in-keyword are a syntax sugar."
  `(setf (gethash ,barcode *prices-source*) ,price))

(defsuite all-tests)
(in-suite all-tests)

(defmacro check-point-of-sale-output (barcode price &body body)
  `(let ((point-of-sale (make-point-of-sale)))
     ,@body
     (on-barcode point-of-sale ,barcode)
     (is (equal (emitted-string) ,price))))

(deftest empty-string-is-invalid ()
  (check-point-of-sale-output "" "Invalid barcode!"))

(deftest need-proper-barcode-and-item-price-to-emit-price ()
  (check-point-of-sale-output "123456" "$10.35"
    (cost-of "123456" is "$10.35" in point-of-sale)))

(deftest different-barcode-different-price ()
  (check-point-of-sale-output "666777" "$23.13"
    (cost-of "666777" is "$23.13" in point-of-sale)))

