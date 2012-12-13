(in-package :turipong)

(defstruct (program (:constructor %make-program))
  code state (input "") (input-pos 0))

(defun dump-program (program)
  (format t "PROGRAM:~%~2,0TINPUT = ~A~%~2,0TINPUT-POS = ~A~%~2,0TSTATES:~%"
          (program-input program) (program-input-pos program))
  (loop for ball across (program-state program) do
        (format t "~4,0T~A = ~S~%"
                (ball-pid ball)
                (ball-val ball))))

(defun program-width (program)
  (array-dimension (program-code program) 1))

(defun program-height (program)
  (array-dimension (program-code program) 0))

(defun program-char (program row col)
  (let ((val (aref (program-code program) row col)))
    (etypecase val
      (character val)
      (ball #\@))))

(defun program-read-input (program)
  (if (< (program-input-pos program) (length (program-input program)))
      (prog1 (aref (program-input program)
                   (program-input-pos program))
        (incf (program-input-pos program)))
      ""))

(defstruct ball
  pid row col direction
  (on-char #\Space :type character)
  (val nil :type list))

(defun ball-top (ball)
  (car (ball-val ball)))

(defun (setf ball-top) (val ball)
  (setf (car (ball-val ball)) val))

(defun ball-push-new (ball)
  (push (make-array 0 :element-type 'character
                      :adjustable t
                      :fill-pointer t)
        (ball-val ball)))

(defun ball-push (val ball)
  (ball-push-new ball)
  (ball-append val ball))

(defun ball-append (val ball)
  (if (arrayp val)
      (loop for i across val do
        (vector-push-extend i (ball-top ball)))
      (vector-push-extend val (ball-top ball))))

(defun ball-pop (ball)
  (let ((val (pop (ball-val ball))))
    (unless (ball-val ball)
      (ball-push-new ball))
    val))

(defun ball-integer-p (val)
  (every #'digit-char-p val))

(defun ball-top-is-integer-p (ball)
  (ball-integer-p (ball-top ball)))

(defun ball-integer (ball)
  (parse-integer (ball-top ball)))

(defun ball-false-p (val)
  (cond
    ((= 0 (length val)) t)
    ((string= " " val) t)
    ((and (ball-integer-p val)
          (= 0 (parse-integer val))) t)
    (t nil)))

(defun ball-top-false-p (ball)
  (ball-false-p (ball-top ball)))

(defun make-program (&key code (input ""))
  (let ((program (%make-program :code code
                                :state (make-array 0 :adjustable t
                                                     :fill-pointer t)
                                :input input)))
    (initialize-state program)
    program))

(defun flat-array (a)
  (make-array (apply #'* (array-dimensions a))
              :element-type (array-element-type a)
              :displaced-to a))

(defun substr (string &optional (start 0) end)
  (let ((end (or end (length string))))
    (make-array (- end start)
                :element-type 'character
                :displaced-to string
                :displaced-index-offset start)))

(defun initialize-state (program)
  (let* ((code (program-code program))
         (line-length (array-dimension code 1))
         (linear (flat-array code)))
    (loop for i from 0
          for char across linear do
            (when (eq char #\@)
              (let ((ball (make-ball :pid (length (program-state program))
                                     :row (truncate i line-length)
                                     :col (mod i line-length)
                                     :direction #(1 1))))
                (ball-push-new ball)
                (vector-push-extend ball (program-state program))
                (setf (aref linear i) ball))))))

(defun make-code (lines)
  (let* ((max-line (reduce (lambda (a b) (max a (length b))) lines
                           :initial-value 0))
         (code (make-array (list (length lines) max-line)
                           :initial-element #\Space)))
    (loop for line in lines
          for i from 0
          as displaced = (make-array (length line)
                                     :displaced-to code
                                     :displaced-index-offset (* max-line i))
          do (replace displaced line))
    code))

(defun read-program (stream &optional (input ""))
  (loop as line = (read-line stream nil)
        while line
        collecting line into lines
        finally
           (return (make-program :code (make-code lines)
                                 :input input))))

(defun read-program-file (file &optional (input ""))
  (with-open-file (stream file)
    (read-program stream input)))

(defun run (program)
  (loop while (runningp program) do
    (run-iteration program)))

(defun run-program-file (file &optional (input ""))
  (let ((program (read-program-file file input)))
    (run program)))

(defun runningp (program)
  (loop for state across (program-state program) do
    (unless (exitedp state)
      (return-from runningp t))))

(defun run-iteration (program)
  (loop for ball across (program-state program)
        collect (next-state program ball) into vectors
        finally (update-states program vectors)))

(defun exitedp (ball)
  (equalp #(0 0) (ball-direction ball)))

(defun next-state (program ball)
  "This does the collision and state update for BALL.  Note that state
is updated, but actual position updates are held off, as they happen
in parallel.  This function could in theory be run in parallel."
  (if (exitedp ball)
      #(0 0)
      (let* ((random (random 1.0))
             (vector (copy-seq (ball-direction ball)))
             (verticalp (/= 0 (aref vector 0)))
             (horizontalp (/= 0 (aref vector 1))))
        (if (< random 0.5)
            (progn
              (collide program ball vector verticalp nil)
              (collide program ball vector nil horizontalp))
            (progn
              (collide program ball vector nil horizontalp)
              (collide program ball vector verticalp nil)))
        (collide program ball vector verticalp horizontalp)
        vector)))


(defun collide (program ball vector verticalp horizontalp)
  (when (or verticalp horizontalp)
    (let ((row (+ (ball-row ball) (if verticalp (aref vector 0) 0)))
          (col (+ (ball-col ball) (if horizontalp (aref vector 1) 0))))
      (unless (out-of-bounds-p program row col)
        (let ((val (aref (program-code program) row col)))
          (when (ball-interact program ball val)
            (when verticalp (setf (aref vector 0) (- (aref vector 0))))
            (when horizontalp (setf (aref vector 1) (- (aref vector 1))))
            t))))))

(defun ball-interact (program ball val)
  (cond
    ((eq val #\Space) nil)
    ((and (characterp val)
          (alphanumericp val)) (ball-append val ball) t)
    ((eq #\_ val) (ball-append #\Space ball) t)
    ((eq #\, val)
     (when (not (equalp "" (ball-top ball)))
       (ball-push-new ball))
     t)
    ((eq #\` val) (ball-pop ball) t)
    ((eq #\: val) (ball-push (ball-top ball) ball) t)
    ((eq #\; val)
     (let ((val1 (ball-pop ball))
           (val2 (ball-pop ball)))
       (ball-push val1 ball)
       (ball-push val2 ball))
     t)
    ((eq #\> val)
     (let ((val (ball-pop ball)))
      (if (string= "" val)
          (princ #\Newline)
          (princ val)))
     t)
    ((eq #\< val) (ball-append (program-read-input program) ball) t)
    ((eq #\? val) (not (ball-top-false-p ball)))
    ((eq #\! val)
     (if (ball-top-false-p ball)
         (progn
           (ball-pop ball)
           (ball-push #\Space ball))
         (progn
           (ball-pop ball)
           (ball-push #\1 ball))))
    ((eq #\+ val)
     (let ((v1 (ball-pop ball))
           (v2 (ball-pop ball)))
       (ball-push-new ball)
       (if (and (ball-integer-p v1)
                (ball-integer-p v2))
           (ball-append (write-to-string (+ (parse-integer v2) (parse-integer v1)))
                        ball)
           (progn
             (ball-append v1 ball)
             (ball-append v2 ball))))
     t)
    ((eq #\- val)
     (let ((v1 (ball-pop ball))
           (v2 (ball-pop ball)))
       (ball-push-new ball)
       (when (and (ball-integer-p v1)
                  (ball-integer-p v2))
         (ball-append (write-to-string (- (parse-integer v2) (parse-integer v1)))
                      ball)))
     t)
    ((ball-p val)
     ;; Don't double-swap
     (when (< (ball-pid ball) (ball-pid val))
       (let ((v1 (ball-pop ball))
             (v2 (ball-pop val)))
         (ball-push v1 val)
         (ball-push v2 ball)))
     t)
    (t t)))

(defun update-states (program vectors)
  (loop with code = (program-code program)
        for ball across (program-state program)
        for vector in vectors do
          (unless (out-of-bounds-p program (ball-row ball) (ball-col ball))
            (setf (aref code (ball-row ball) (ball-col ball))
                  (ball-on-char ball)))
          (incf (ball-row ball) (aref vector 0))
          (incf (ball-col ball) (aref vector 1))
          (if (out-of-bounds-p program (ball-row ball) (ball-col ball))
              (setf (ball-direction ball) #(0 0))
              (progn
                (when (ball-p (aref code (ball-row ball) (ball-col ball)))
                  ;; Abort! Abort!
                  (decf (ball-row ball) (aref vector 0))
                  (decf (ball-col ball) (aref vector 1)))
                (setf (ball-on-char ball)
                      (aref code (ball-row ball) (ball-col ball)))
                (setf (aref code (ball-row ball) (ball-col ball)) ball)
                (setf (ball-direction ball) vector)))))

(defun out-of-bounds-p (program row col)
  (or (< row 0) (< col 0)
      (>= row (array-dimension (program-code program) 0))
      (>= col (array-dimension (program-code program) 1))))

(defun print-program (program)
  (let* ((code (program-code program))
         (width (array-dimension code 1))
         (height (array-dimension code 0))
         (string (map 'string (lambda (x)
                                (if (characterp x) x #\@))
                      (flat-array code))))
    (format t "~&")
    (loop for row from 0 below height
          as start = (* row width)
          do (let ((substr (substr string start (+ start width))))
               (format t "~A~%" substr)))))
