(defpackage :name.hijarian.point-of-sale
  (:use :common-lisp :stefil))

(in-package :name.hijarian.point-of-sale)

(defun emitted-string ()
  "Checks what string was emitted to external display"
  "Invalid barcode!")

(defun on-barcode (point-of-sale barcode)
  "Event handler which handles on the occasion of the barcode being sent to the point of sale"
  nil)

(defun make-point-of-sale ()
  "Factory function to properly create a point of sale"
  nil)

(defsuite all-tests)
(in-suite all-tests)

(deftest empty-string-is-invalid ()
  (let ((point-of-sale (make-point-of-sale)))
    (on-barcode point-of-sale "") 
    (is (equal (emitted-string) "Invalid barcode!"))))


