(defpackage :name.hijarian.point-of-sale
  (:use :common-lisp :stefil))

(in-package :name.hijarian.point-of-sale)

;-CODE------------------------------------------------------------------------------

(defparameter *emitted-string* nil)

(defparameter *prices-source* (make-hash-table :test #'equal))

(defun emitted-string ()
  "Checks what string was emitted to external display"
  *emitted-string*)

(defun ask-for-price (barcode)
  (gethash barcode *prices-source*))

(defun emit-string (string) 
  (setf *emitted-string* string))

(defun on-barcode (barcode)
  "Event handler which handles on the occasion of the barcode being sent to the point of sale"
  (let ((price (ask-for-price barcode)))
    (emit-string 
     (if (null price)
         (format nil "No price for barcode: '~a'" barcode)
         price))))

(defmacro cost-of (barcode is-keyword price)
  "Pretty macro to associate a price with a given barcode. is-keyword is a syntax sugar."
  `(setf (gethash ,barcode *prices-source*) ,price))

;-TESTS------------------------------------------------------------------------------

(defsuite all-tests)
(in-suite all-tests)

(defixture clear-prices
  (:setup (clrhash *prices-source*)))

(defmacro check-point-of-sale-output (barcode price &body body)
  `(with-fixture clear-prices
     ,@body
     (on-barcode ,barcode)
     (is (equal (emitted-string) ,price))))

(deftest empty-string-is-invalid ()
  (check-point-of-sale-output "" "No price for barcode: ''"))

(deftest need-proper-barcode-and-item-price-to-emit-price ()
  (check-point-of-sale-output "123456" "$10.35"
    (cost-of "123456" is "$10.35")))

(deftest different-barcode-different-price ()
  (check-point-of-sale-output "666777" "$23.13"
    (cost-of "666777" is "$23.13")))

(deftest no-prices-defined-emit-no-price-message ()
  (check-point-of-sale-output "666777" "No price for barcode: '666777'"))

(deftest no-such-barcode-recorded-no-price-message ()
  (check-point-of-sale-output "666777" "No price for barcode: '666777'"
    (cost-of "111111" is "$10.01")
    (cost-of "222222" is "$20.02")))
    
(deftest emits-correct-prices-when-several-defined ()
  (check-point-of-sale-output "333333" "$30.03"
    (cost-of "111111" is "$10.01")
    (cost-of "222222" is "$20.02")
    (cost-of "333333" is "$30.03")
    (cost-of "444444" is "$40.04")))
    
