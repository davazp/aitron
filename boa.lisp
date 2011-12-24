;;; boa -- My tron player

;;; Copyright (C) 2011 David Vazquez

(defpackage :boa
  (:use :cl)
  (:shadow #:log)
  (:export #:main))

(in-package :boa)

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

;;; Utilities

;;; Execute body until cond to be true.
(defmacro until (cond &body body)
  `(do nil (,cond) ,@body))

(defun compare (a b)
  (cond
    ((< a b) -1)
    ((= a b)  0)
    ((> a b)  1)))

;;; Return a cicular list
(defun circular (&rest list)
  (let ((result (copy-list list)))
    (nconc result result)))

;;; As mapcar, but the first element is fixed, not a list.
(defun map1 (function arg &rest lists)
  (apply #'mapcar function (circular arg) lists))

;;; Return a random element of SEQUENCE.
(defun random-choice (sequence)
  (let ((l (length sequence)))
    (elt sequence (random l))))

;;; Return a scrambled copy of a list.
(defun scramble (list)
  (let ((list (copy-list list)))
    (loop for tail on list
          for l from (length tail) downto 0
          for i = (random l)
          do (rotatef (car tail) (nth i tail))
          finally (return list))))

;;; Return a symbol whose name is the concatenation of PREFIX to the
;;; name of SYMBOL.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun prefix-symbol (prefix symbol)
    (intern (concatenate 'string prefix (string symbol)))))

;;; Define a simple accessor.
(defmacro define-accessor (name args form)
  (let ((valuevar (gensym))
        (writer (prefix-symbol "SET-" name)))
    `(progn
       (defun ,name ,args ,form)
       (defun ,writer (,@args ,valuevar)
         (setf ,form ,valuevar))
       (defsetf ,name ,writer))))


;;; Array functions

;;; Return an array of dimensions DIM initialized to the element X. It
;;; is a shortcut to make-array.
(defun make-initialized-array (dim x)
  (make-array dim :initial-element x))

;;; Return an array of the same dimensions as array. It also accepts
;;; any argument as make-array does, but you have not to give the
;;; dimensions.
(defun make-array-as (array &rest args &key &allow-other-keys)
  (apply #'make-array (array-dimensions array) args))

(defun linearize-array (array)
  (make-array (array-total-size array) :displaced-to array))

;;; Map a function to the elements of an array.
(defun maparray (f array &rest others)
  (let* ((dimensions (array-dimensions array))
         (result (make-array dimensions))
         (arrays (cons array others)))
    (dotimes (i (array-total-size array) result)
      (let ((value (apply f (mapcar (rcurry #'row-major-aref i) arrays))))
        (setf (row-major-aref result i) value)))))


;;; A simple implementations of queues,
;;; FIFO data structure.
(defstruct (queue (:constructor make-queue%))
  start
  end)

;;; Create a new queue, if the optional argument is given, it is a
;;; list, which will be the initial content of the queue.
(defun make-queue (&optional list)
  (make-queue% :start list :end (last list)))

;;; Check if QUEUE is empty.
(defun null-queue-p (queue)
  (null (queue-start queue)))

;;; Add a new element to the queue.
(defun enqueue (x queue)
  (let ((node (cons x nil)))
    (unless (null (queue-end queue))
      (setf (cdr (queue-end queue)) node))
    (when (null (queue-start queue))
      (setf (queue-start queue) node))
    (setf (queue-end queue) node)
    x))

;;; Get the older element from the queue.
(defun dequeue (queue)
  (let ((node (queue-start queue)))
    (prog1 (car node)
      (setf (queue-start queue) (cdr node)))))

;;; Add a list of elements to the queue.
(defun enqueue-list (list queue)
  (dolist (x list)
    (enqueue x queue)))


;;; Board and cells

;;; Size of board
(defconstant +width+ 100)
(defconstant +height+ 100)

(defvar *map*
  (make-array (list +height+ +width+) :initial-element nil))

(defun cell (i j)
  (list i j))

(define-accessor cell-i (cell)
  (first cell))

(define-accessor cell-j (cell)
  (second cell))

(defun cell+ (pos mov)
  (let ((i (cell-i pos))
        (j (cell-j pos)))
    (let ((di (cell-i mov))
          (dj (cell-j mov)))
      (cell (+ i di) (+ j dj)))))

(defun valid-cell-p (cell)
  (and (<= 0 (cell-i cell) (1- +height+))
       (<= 0 (cell-j cell) (1- +width+))))

(defun busy-cell-p (cell)
  (aref *map* (cell-i cell) (cell-j cell)))

(defun free-cell-p (cell)
  (not (busy-cell-p cell)))

(defun available-cell-p (cell)
  (and (valid-cell-p cell) (free-cell-p cell)))

(defun set-cell-as-busy (cell)
  (let ((i (cell-i cell))
        (j (cell-j cell)))
    (setf (aref *map* i j) t)))

(defun set-cells-as-busy (list-of-cells)
  (dolist (cell list-of-cells)
    (set-cell-as-busy cell)))

;;; Input / output
(defun read-cords (&optional stream)
  (let ((line (read-line stream)))
    (multiple-value-bind (i j-start)
        (parse-integer line :junk-allowed t)
      (let ((j (parse-integer line :start j-start)))
        (cell i j)))))

(defun write-cords (cell)
  (let ((i (cell-i cell))
        (j (cell-j cell)))
    (format t "~d ~d~%" i j)
    (finish-output)))


;;; Players
(defvar *current-cell*)
(defvar *prey-cell*)

(defvar *up*    '(-1  0))
(defvar *left*  '( 0 -1))
(defvar *down*  '( 1  0))
(defvar *right* '( 0  1))
(defvar *directions*
  (list *up* *left* *down* *right*))

;;; Move in the direction (DI, DJ).
(defun move (d)
  (setf *current-cell* (cell+ *current-cell* d))
  (write-cords *current-cell*)
  (set-cell-as-busy *current-cell*))

(defun valid-direction-p (d &optional (cell *current-cell*))
  (valid-cell-p (cell+ cell d)))

(defun available-direction-p (d &optional (cell *current-cell*))
  (available-cell-p (cell+ cell d)))

(defun valid-neighbours (&optional (cell *current-cell*))
  (let ((adjacents (map1 #'cell+ cell *directions*)))
    (remove-if-not #'valid-cell-p adjacents)))

(defun list-neighbours (&optional (cell *current-cell*))
  (remove-if-not #'free-cell-p (valid-neighbours cell)))

(defun valid-directions (&optional (cell *current-cell*))
  (remove-if-not (rcurry #'valid-directions cell) *directions*))

(defun list-directions (&optional (cell *current-cell*))
  (remove-if-not (rcurry #'available-direction-p cell) *directions*))

(defun random-direction (&optional (cell *current-cell*))
  (let ((available (list-directions cell)))
    (random-choice
     (if (null available)
         (valid-directions cell)
         (list-directions cell)))))


;;; Gradient

(defun make-gradient ()
  (make-array-as *map* :initial-element (array-total-size *map*)))

(define-accessor gradient-of-cell (gradient cell)
  (aref gradient (cell-i cell) (cell-j cell)))

(define-accessor gradient-of-direction (gradient cell)
  (gradient-of-cell gradient (cell+ cell *current-cell*)))

(defun set-gradient-of-cells (gradient list-cell value)
  (dolist (cell list-cell)
    (setf (gradient-of-cell gradient cell) value)))

(defun unexplored-neighbours (gradient cell)
  (let ((value (gradient-of-cell gradient cell)))
    (flet ((explored-cell-p (cell)
             (<= (gradient-of-cell gradient cell) (1+ value))))
      (remove-if #'explored-cell-p (list-neighbours cell)))))

;;; Return an array with the distances to the point I,J in *MAP*.
(defun gradient (cell &optional depth)
  (let ((gradient (make-gradient))
        (frontier (make-queue)))
    (setf (gradient-of-cell gradient cell) 0)
    (enqueue cell frontier)
    (until (null-queue-p frontier)
      (let* ((cell (dequeue frontier))
             (value (gradient-of-cell gradient cell))
             (neighbours (unexplored-neighbours gradient cell)))
        (when (and depth (= value depth))
          (return))
        (set-gradient-of-cells gradient neighbours (1+ value))
        (enqueue-list neighbours frontier)))
    gradient))


;;; Find the element of LIST whose value of KEY is maximum according
;;; to the order relationship PREDICATE.
(defun optimizing (list predicate &key (key #'identity))
  (let* ((max (first list))
         (max-value (funcall key max)))
    (dolist (x (rest list) max)
      (let ((x-value (funcall key x)))
        (when (funcall predicate x-value max-value)
          (setf max x)
          (setf max-value x-value))))))

;;; Find the element of LIST minimizing KEY.
(defun minimizing (list &key (key #'identity))
  (optimizing list #'< :key key))

;;; Find the element of LIST maximizing KEY.
(defun maximizing (list &key (key #'identity))
  (optimizing list #'> :key key))


(defun move-to-first-free-cell ()
  (move (random-direction)))

(defun move-to (cell)
  (let* ((grad (gradient cell))
         (directions (or (list-directions) (list (random-direction)))))
    (move
     (minimizing (scramble directions)
                 :key (curry #'gradient-of-direction grad)))))

(defun curry (fn &rest preargs)
  (lambda (&rest postargs)
    (apply fn (append preargs postargs))))

(defun rcurry (fn &rest postargs)
  (lambda (&rest preargs)
    (apply fn (append preargs postargs))))


(defun compute-fill-algorithm (cell)
  (let* ((grad (linearize-array (gradient cell)))
         (n (length grad)))
    (- n (count n grad))))


;;; Move to the maximizing fill area cell
(defun move-to-maximize-fill ()
  (move (maximizing (scramble (list-directions))
                    :key (lambda (d)
                           (compute-fill-algorithm (cell+ *current-cell* d))))))



(defun move-to-maximize-openess ()
  (move (maximizing (scramble (list-directions))
                    :key (lambda (d)
                           (let ((*current-cell* (cell+ *current-cell* d)))
                             (length (valid-directions)))))))


;;; LAS TECNICAS QUE VOY IMPLEMENTANDO ARRIBA SON INDIVIDUALES, Y EN
;;; GENERAL SE TRATA DE OPTIMIZAR UNA FUNCION VALOR. TENER ESTO EN
;;; CUENTA PARA CUANDO REORGANIZE EL CODIGO, HACIENDOLO MUCHO MAS
;;; GENERICO Y PERMITIENDO COMBINAR FUNCIONES DE VALORES COMODAMENTE,
;;; QUIZA CON PARAMETROS.
;;;
;;; IGUALMENTE, APLICAR EL VALOR NO A VECINOS INMEDIATOS, SINO A UNA
;;; VECINIDAD MAYOR Y/O PUNTOS ESTRATEGICOS DEL TABLERO.
;;;
;;; EN LA COMBINACION DE ESTAS ESTRATEGIAS (MINIMAX PAR CERCA),
;;; INTERVIENE LA DISTANCIA COMO PARAMETRO TAMBIEN.
;;;
;;; IGUALMENTE, UNA VEZ EL OBJETIVO DE SABER CUANDO EL ENEMIGO ESTA
;;; ENCERRADO ES RELLENAR EL MAYOR AREA POSIBLE, ESTO ES HECHO CON
;;; METODOS TRADICIONALES.

(defun main ()
  (log "------------------------------------------------------------")
  (log "New play started.")
  (setf *current-cell* (read-cords))
  (set-cell-as-busy *current-cell*)
  (setf *prey-cell* (read-cords))
  (set-cell-as-busy *prey-cell*)
  (log "  initial: ~a" *current-cell*)
  (log "  prey: ~a" *prey-cell*)
  ;; Read walls
  (loop repeat (cell-i (read-cords)) do (set-cell-as-busy (read-cords)))
  ;; Game loop
  (let ((*random-state* (make-random-state t)))
    (loop
      (move-to-maximize-fill)
      (set-cell-as-busy (read-cords)))))

;;; boa ends here
