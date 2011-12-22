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
(defun move (di dj)
  (incf (player-i *me*) di)
  (incf (player-j *me*) dj)
  (let ((i (player-i *me*))
        (j (player-j *me*)))
    (write-cords i j)
    (set-cell-as-busy i j)))

(defun move-to-first-free-cell ()
  (loop for (di dj) in '((-1 0) (1 0) (0 -1) (0 1) (0 0))
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


;;; FIFO
(defstruct (queue (:constructor make-queue%))
  start
  end)

(defun make-queue (&optional list)
  (make-queue% :start list :end (last list)))

(defun null-queue-p (queue)
  (null (queue-start queue)))

(defun enqueue (x queue)
  (let ((node (cons x nil)))
    (unless (null (queue-end queue))
      (setf (cdr (queue-end queue)) node))
    (when (null (queue-start queue))
      (setf (queue-start queue) node))
    (setf (queue-end queue) node)
    x))

(defun dequeue (queue)
  (let ((node (queue-start queue)))
    (prog1 (car node)
      (setf (queue-start queue) (cdr node)))))

(defun enqueue-list (list queue)
  (dolist (x list)
    (enqueue x queue)))


(defmacro until (cond &body body)
  `(do nil (,cond) ,@body))

(defmacro while (cond &body body)
  `(until (not ,cond) ,@body))

(defun make-initialized-array (dimensions element)
  (make-array dimensions :initial-element element))

(defun make-array-as (array element)
  (make-array (array-dimensions array) :initial-element element))

(defun set-array-elements (list-nodes array value)
  (loop for (i j) in list-nodes do (setf (aref array i j) value)))


(defun list-gradient-neighbours (gradient i j)
  (let ((value (aref gradient i j)))
    (loop for (di dj) in '((-1 0) (1 0) (0 -1) (0 1))
          for ni = (+ i di)
          for nj = (+ j dj)
          when (and (valid-cell-p ni nj) (free-cell-p ni nj))
          when (> (aref gradient ni nj) (1+ value))
          collect (list ni nj))))

;;; Return an array with the distances to the point I,J in *MAP*.
(defun compute-gradient (i j)
  (let ((gradient (make-array-as *map* (array-total-size *map*)))
        (frontier (make-queue)))
    (setf (aref gradient i j) 0)
    (enqueue (list i j) frontier)
    (until (null-queue-p frontier)
      (destructuring-bind (i j) (dequeue frontier)
        (let ((value (aref gradient i j)))
          (let ((neighbours (list-gradient-neighbours gradient i j)))
            (set-array-elements neighbours gradient (1+ value))
            (enqueue-list neighbours frontier)))))
    gradient))


(defun list-movements (i j)
  (loop for (di dj) in '((-1 0) (1 0) (0 -1) (0 1))
        for ni = (+ i di)
        for nj = (+ j dj)
        when (and (valid-cell-p ni nj) (free-cell-p ni nj))
        collect (list di dj)))

(defun list-neighbours (i j)
  (loop for (di dj) in '((-1 0) (1 0) (0 -1) (0 1))
        for ni = (+ i di)
        for nj = (+ j dj)
        when (and (valid-cell-p ni nj) (free-cell-p ni nj))
        collect (list ni nj)))

(defun minimize (list predicate &key (key #'identity))
  ;; Aqui nos podemos ahorrar bastante computación
  (reduce (lambda (a b)
            (if (funcall predicate (funcall key a) (funcall key b))
                a
                b))
          list))


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
  (let ((grad (compute-gradient 0 0)))
    (loop
      (apply #'move
       (minimize (scramble (or (list-movements (player-i *me*) (player-j *me*)) '((1 0)))) #'<
                 :key (lambda (mov)
                        (destructuring-bind (di dj) mov
                          (aref grad
                                (+ di (player-i *me*))
                                (+ dj (player-j *me*)))))))
      (apply #'set-cell-as-busy (read-cords)))))

;;; boa ends here
