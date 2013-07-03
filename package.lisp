;; It's a definition of a `point-of-sale` package.
;; USER-LEVEL:
; Here we define symbols from which other packages we want to use directly (without FQN)
; also define shorter name for ourself, to easily enter the package in REPL
; also define what our public interface is 
; (trying to call non-exported symbols will emit an error condition in most of modern CL implementations)
;
;; TECHNICAL DETAILS:
; You need to load this file first, to define where our symbols will be held
; All source files which will use symbols from this package directly (without FQN)
; and which want to define symbols in this package
; need to evaluate (in-package :point-of-sale) as a very first form.

(defpackage :name.hijarian.point-of-sale
  (:use :common-lisp :cl-user :stefil)
  (:nicknames :point-of-sale)
  (:export :all-tests :on-barcode :cost-of :emitted-string))
