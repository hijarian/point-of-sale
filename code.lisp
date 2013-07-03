; Code for the Point of Sale exercise
;
; It's totally mind-blowing, but until we need to coexist several points of sale 
; each having different sources of prices 
; or until we need to really attach the LCD display,
; we don't need object abstractions neither on string emitter, 
; nor on prices source, nor on point of sale itself!

(in-package :name.hijarian.point-of-sale)


(defparameter *emitted-string* nil)
(defun emit-string (string) 
  (setf *emitted-string* string))


(defparameter *prices-source* (make-hash-table :test #'equal))
(defun ask-for-price (barcode)
  (gethash barcode *prices-source*))


(defun on-barcode (barcode)
  "Event handler which handles on the occasion of the barcode being sent to the point of sale"
  (let ((price (ask-for-price barcode)))
    (emit-string 
     (if (null price)
         (format nil "No price for barcode: '~a'" barcode)
         price))
    nil))
  

