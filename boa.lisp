;;; boa -- My tron player

;;; Copyright (C) 2011 David Vazquez

(defpackage :boa
  (:use :cl)
  (:shadow #:log)
  (:export #:main))

(in-package :boa)

;;; Size of board
(defconstant +width+ 100)
(defconstant +height+ 100)

(defvar *map*
  (make-array (list +height+ +width+) :initial-element nil))

(defun valid-cell-p (i j)
  (and (<= 0 i (1- +height+))
       (<= 0 j (1- +width+))))

(defun busy-cell-p (i j)
  (aref *map* i j))

(defun free-cell-p (i j)
  (not (busy-cell-p i j)))

(defun set-cell-as-busy (i j)
  (setf (aref *map* i j) t))

;;; Logs, useful for debugging
(defvar *logfile* #P"boa.log")

;;; Append a formatted string to the end of a file
(defmacro with-append-file ((var path) &body body)
  `(with-open-file (,var ,path
                    :direction :output
                    :if-exists :append
                    :if-does-not-exist :create)
     ,@body))

(defun log (fmt &rest args)
  (with-append-file (out *logfile*)
    (format out "~a: " (get-universal-time))
    (apply #'format out fmt args)
    (terpri out)))

;;; Input / output
(defun read-cords ()
  (list (read) (read)))

(defun write-cords (i j)
  (format t "~d ~d~%" i j)
  (finish-output))

;;; Players
(defstruct player
  i j)

(defvar *me*)
(defvar *prey*)

;;; Return a scrambled copy of a list.
(defun scramble (list)
  (let ((list (copy-list list)))
    (loop for tail on list
          for l from (length tail) downto 0
          for i = (random l)
          do (rotatef (car tail) (nth i tail))
          finally (return list))))

;;; Move in the direction (DI, DJ).
(defvar *last-direction* nil)
(defun move (di dj)
  (incf (player-i *me*) di)
  (incf (player-j *me*) dj)
  (let ((i (player-i *me*))
        (j (player-j *me*)))
    (write-cords i j)
    (set-cell-as-busy i j)
    (setf *last-direction* (list (list di dj)))))

(defun move-to-first-free-cell ()
  (loop for (di dj) in (append *last-direction* '((-1 0) (0 -1) (1 0) (0 1) (0 0)))
        for i = (+ (player-i *me*) di)
        for j = (+ (player-j *me*) dj)
        until (and (valid-cell-p i j) (free-cell-p i j))
        finally
           (cond
             ((= 0 di dj)
              ;; Boa does not want to be a cheater
              (log "  No possible movements!")
              (move 1 0))
             (t
              (move di dj)))))

(defun main ()
  (log "------------------------------------------------------------")
  (log "New play started.")
  (destructuring-bind (i j) (read-cords)
    (set-cell-as-busy i j)
    (setf *me* (make-player :i i :j j)))
  (destructuring-bind (i j) (read-cords)
    (set-cell-as-busy i j)
    (setf *prey* (make-player :i i :j j)))
  (log "  initial i: ~d: " (player-i *me*))
  (log "  initial j: ~d: " (player-j *me*))
  (log "  prey i: ~d: "    (player-i *prey*))
  (log "  prey j: ~d: "    (player-j *prey*))
  ;; Read walls
  (destructuring-bind (n &optional ign) (read-cords)
    (declare (ignore ign))
    (loop repeat n do (apply #'set-cell-as-busy (read-cords))))
  (loop
    (move-to-first-free-cell)
    (apply #'set-cell-as-busy (read-cords))))

;;; boa ends here
