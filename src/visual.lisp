(in-package :turipong.visual)

(defvar *max-display-width* 79)
(defvar *max-display-height* 15)
(defvar *iteration-wait* 0.3)

(defun clear ()
  (format t "[2J"))

(defun move-to (row col)
  (format t "[~A;~AH" row col))

(defun cleol ()
  (format t "[K"))

(defun run (program)
  (clear)
  (loop while (turipong:runningp program) do
    (let ((output
            (with-output-to-string (stream)
              (turipong:run-iteration program))))
      (display-program program)
      (move-to *max-display-height* 0)
      (princ output)
      (force-output)
      (sleep *iteration-wait*)))
  (move-to *max-display-height* 0)
  (format t "All done!~%"))

(defun display-program (program)
  (let* ((height (min *max-display-height*
                      (turipong:program-height program)))
         (width (min *max-display-width*
                     (turipong:program-width program)))
         (string (make-string width)))
    (loop for i from 0 below height do
      (move-to (1+ i) 0)
      (loop for j from 0 below width do
        (setf (aref string j) (turipong:program-char program i j)))
      (format t "~A" string))
    (force-output)))

(defun read-display-program (file)
  (let ((program (turipong:read-program-file file)))
    (display-program program)))

(defun run-program-file (file)
  (let ((program (turipong:read-program-file file)))
    (run program)))
