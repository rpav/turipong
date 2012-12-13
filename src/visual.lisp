(in-package :turipong.visual)

(defparameter *max-display-width* 79)
(defparameter *max-display-height* 15)
(defparameter *iteration-wait* 0.2)

(defun clear ()
  (format t "[2J"))

(defun move-to (row col)
  (format t "[~A;~AH" row col))

(defun cleol ()
  (format t "[K"))

(defun run (program)
  (clear)
  (let ((chars-printed 0))
    (labels ((move-to-end ()
               (move-to (+ (1+ *max-display-height*)
                           (truncate chars-printed *max-display-width*))
                        (1+ (mod chars-printed *max-display-width*))))
             (incr-position (output)
               (incf chars-printed (length output))
               (when (and (= (length output) 1)
                          (char= #\Newline (aref output 0)))
                 (incf chars-printed (- *max-display-width*
                                        (mod chars-printed *max-display-width*)))))
             (dump-program (&rest r)
               (declare (ignore r))
               (move-to-end)
               (turipong:dump-program program)))
      (handler-bind (#+sbcl (sb-sys:interactive-interrupt #'dump-program)
                     #+sbcl (sb-int:simple-parse-error #'dump-program)
                     (error #'dump-program))
        (loop while (turipong:runningp program) do
          (let ((output
                  (with-output-to-string (*standard-output*)
                    (turipong:run-iteration program))))
            (display-program program)
            (move-to-end)
            (princ output)
            (finish-output)
            (incr-position output)
            (sleep *iteration-wait*))))
      (move-to-end)
      (format t "All done!~%"))))

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
    (finish-output)))

(defun read-display-program (file)
  (let ((program (turipong:read-program-file file)))
    (display-program program)))

(defun run-program-file (file &optional (input ""))
  (let ((program (turipong:read-program-file file input)))
    (run program)))
