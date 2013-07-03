; Tests for the Point of Sale exercise
;
; Note that they use some inner knowledge of the code.lisp
; `cost-of` and `emitted-string` are missing parts of the UI of prices source and string emitter, really,
; but in our case they are just a helpers for tests, so until we need the real objects representing
; this concepts, this two functions will better not pollute the code.lisp

(in-package :name.hijarian.point-of-sale)

(defmacro cost-of (barcode is-keyword price)
  "Pretty macro to associate a price with a given barcode. is-keyword is a syntax sugar."
  `(setf (gethash ,barcode *prices-source*) ,price))

(defun emitted-string ()
  "Checks what string was emitted to external display"
  *emitted-string*)

(defsuite all-tests)
(in-suite all-tests)

(defixture clear-prices
  (:setup (clrhash *prices-source*)))

(defmacro check-point-of-sale-output (barcode price &body setup)
  "This is the encapsulation of a routine: clear prices possibly defined in previous test, 
call whatever specific setup is needed for this test, trigger the event under test 
and check the string emitted by point of sale"
  `(with-fixture clear-prices
     ,@setup
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
    
