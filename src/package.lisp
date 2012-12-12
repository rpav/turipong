(defpackage :turipong
  (:use #:cl #:alexandria)
  (:export #:run #:run-iteration #:run-program-file #:runningp
           #:program-width #:program-height #:program-char
           #:print-program #:read-program
           #:read-program-file))

(defpackage :turipong.visual
  (:use #:cl #:alexandria)
  (:export #:run #:run-program-file))
