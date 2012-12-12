(defpackage :turipong.asdf
  (:use #:cl #:asdf))

(in-package :turipong.asdf)

(defsystem :turipong
  :description "Turipong in CL"
  :author "Ryan Pavlik"
  :license "LLGPL"
  :version "0.0"

  :depends-on ()
  :pathname "src"
  :serial t

  :components
  ((:file "package")
   (:file "turipong")
   (:file "visual")))
