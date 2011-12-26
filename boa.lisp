;;; boa -- My tron player

;;; Copyright (C) 2011 David Vazquez

(defpackage :boa
  (:use :cl)
  (:shadow #:log)
  (:export #:main))

(in-package :boa)

(declaim (optimize speed))

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

;;; Functional operators

(defun curry (fn &rest preargs)
  (lambda (&rest postargs)
    (apply fn (append preargs postargs))))

(defun rcurry (fn &rest postargs)
  (lambda (&rest preargs)
    (apply fn (append preargs postargs))))


;;; Find the element of LIST whose value of KEY is maximum according
;;; to the order relationship PREDICATE.
(defun optimizing (list predicate &key (key #'identity))
  (if (null list)
      nil
      (let* ((max (first list))
             (max-value (funcall key max)))
        (dolist (x (rest list) max)
          (let ((x-value (funcall key x)))
            (when (funcall predicate x-value max-value)
              (setf max x)
              (setf max-value x-value)))))))

;;; Find the element of LIST minimizing KEY.
(defun minimizing (list key)
  (optimizing list #'< :key key))

;;; Find the element of LIST maximizing KEY.
(defun maximizing (list key)
  (optimizing list #'> :key key))

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

;;; Copy an array
(defun copy-array (array)
  (let ((copy (make-array-as array :element-type (array-element-type array))))
    (dotimes (i (array-total-size copy) copy)
      (setf (row-major-aref copy i) (row-major-aref array i)))))

(defun linearize-array (array)
  (make-array (array-total-size array)
              :element-type (array-element-type array)
              :displaced-to array))

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


;;; Heaps

;;; Create a heap with a optional ESTIMATED SIZE.
(defun make-heap (&optional (estimated-size 32))
  (make-array (list (* 2 estimated-size))
              :adjustable t :fill-pointer 0))

(defun %heap-null-index-p (index)
  (zerop index))

;;; Offset of the indexed element in the array.
(defun %heap-offset (index)
  (* 2 (1- index)))

(defun %heap-index (offset)
  (1+ (/ offset 2)))

;;; Value of the indexed elment.
(define-accessor %heap-value (heap index)
  (aref heap (%heap-offset index)))

;;; Priority of the indexed element.
(define-accessor %heap-priority (heap index)
  (aref heap (1+ (%heap-offset index))))

;;; Left child of the indexed element.
(defun %heap-child-1 (index)
  (* 2 index))

;;; Right child of the indexed element.
(defun %heap-child-2 (index)
  (1+ (* 2 index)))

(defun %heap-childs (heap index)
  (flet ((valid-child-p (index) (< (%heap-offset index) (length heap))))
    (let ((child1 (%heap-child-1 index))
          (child2 (%heap-child-2 index)))
      (append
       (if (valid-child-p child1) (list child1) nil)
       (if (valid-child-p child2) (list child2) nil)))))

(defun %heap-top () 1)
(defun %heap-bottom (heap)
  (%heap-index (- (length heap) 2)))

(defun empty-heap-p (heap)
  (zerop (%heap-bottom heap)))

(defun %heap-parent (index)
  (truncate (/ index 2)))

(defun %heap< (heap index1 index2)
  (< (%heap-priority heap index1)
     (%heap-priority heap index2)))

(defun %heap-swap (heap index1 index2)
  (rotatef (%heap-value heap index1) (%heap-value heap index2))
  (rotatef (%heap-priority heap index1) (%heap-priority heap index2)))

;;; Insert a new element X with PRIORITY in HEAP.
(defun heap-insert (heap x priority)
  (let ((newindex
         (prog1 (%heap-index (vector-push-extend x heap))
           (vector-push-extend priority heap))))
    (loop for index = newindex then parent
          for parent = (%heap-parent index)
          until (%heap-null-index-p parent)
          while (%heap< heap index parent)
          do (%heap-swap heap index parent))))

;;; Remove the element with the highest priority in the HEAP.
(defun heap-remove-min (heap)
  (cond
    ((empty-heap-p heap) nil)
    (t
     (let ((value (%heap-value heap (%heap-top))))
       (%heap-swap heap (%heap-top) (%heap-bottom heap))
       (decf (fill-pointer heap) 2)
       (loop for index = (%heap-top) then child
             for childs = (%heap-childs heap index)
             for child = (minimizing childs (curry #'%heap-priority heap))
             while (and child (%heap< heap child index))
             do (%heap-swap heap index child)
             finally (return value))))))


;;; Board and cells

;;; Size of board
(defconstant +width+ 100)
(defconstant +height+ 100)

(defvar *map*
  (make-array (list +height+ +width+) :element-type 'bit :initial-element 0))

(defun cell (i j)
  (list i j))

(define-accessor cell-i (cell)
  (first cell))

(define-accessor cell-j (cell)
  (second cell))

(defun cell= (cell1 cell2)
  (and (= (cell-i cell1) (cell-i cell2))
       (= (cell-j cell1) (cell-j cell2))))

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
  (= 1 (aref *map* (cell-i cell) (cell-j cell))))

(defun free-cell-p (cell)
  (not (busy-cell-p cell)))

(defun available-cell-p (cell)
  (and (valid-cell-p cell) (free-cell-p cell)))

(defun set-cell-as-busy (cell)
  (let ((i (cell-i cell))
        (j (cell-j cell)))
    (setf (aref *map* i j) 1)))

(defun set-cell-as-free (cell)
  (let ((i (cell-i cell))
        (j (cell-j cell)))
    (setf (aref *map* i j) 0)))

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

;;; Assume in the context BODY that the action was to take DIRECTION.
(defmacro hypotesis (direction &body body)
  `(let ((*current-cell* (cell+ *current-cell* ,direction)))
     (log "Assume ~a~%" *current-cell*)
     (set-cell-as-busy *current-cell*)
     (unwind-protect ,@body
       (set-cell-as-free *current-cell*))))

(defun move-to (to)
  (log "Moving to ~a" to)
  (setf *current-cell* to)
  (write-cords *current-cell*)
  (set-cell-as-busy *current-cell*))

;;; Move in the direction (DI, DJ).
(defun move (d)
  (move-to (cell+ *current-cell* d)))

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
  (remove-if-not (rcurry #'valid-direction-p cell) *directions*))

(defun list-directions (&optional (cell *current-cell*))
  (remove-if-not (rcurry #'available-direction-p cell) *directions*))

(defun random-direction (&optional (cell *current-cell*))
  (let ((available (list-directions cell)))
    (random-choice
     (if (null available)
         (valid-directions cell)
         (list-directions cell)))))

(defun random-neighbour (&optional (cell *current-cell*))
  (let ((available (list-neighbours cell)))
    (random-choice
     (if (null available)
         (valid-neighbours cell)
         (list-neighbours cell)))))


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

(defun infinite-gradient-p (x)
  (= x (array-total-size *map*)))

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

;;; A-star algorithm implementation

(defun A* (initial goal h &optional max)
  (let ((frontier (make-heap 100))
        (explored (make-array-as *map* :element-type 'bit :initial-element 0)))
    (labels (;; Mark a cell as explored.
             (mark-as-explored (cell)
               (setf (aref explored (cell-i cell) (cell-j cell)) 1))
             ;; Check if a path has already been explored.
             (exploredp (cell)
               (= 1 (aref explored (cell-i cell) (cell-j cell))))
             ;; List unexplored neighbours.
             (list-unexplored (cell)
               (remove-if #'exploredp (list-neighbours cell))))
      ;; Algorithm
      (heap-insert frontier (list initial) 1)
      (mark-as-explored initial)
      (until (or (empty-heap-p frontier))
        (let* ((path (heap-remove-min frontier))
               (cost (length path)))
          (when (cell= goal (car path))
            (return-from A* (reverse path)))
          (when (and max (> cost max))
            (return-from A* nil))
          (dolist (new (list-unexplored (car path)))
            (mark-as-explored new)
            (heap-insert frontier
                         (cons new path)
                         (+ (1+ cost) (funcall h new)))))))))

(defun distance (from to)
  (+ (abs (- (cell-i from) (cell-i to)))
     (abs (- (cell-j from) (cell-j to)))))

(defun pathfind (from to &optional max)
  (A* from to
      (lambda (cell) (distance cell to))
      max))

(defun pathto (to &optional max)
  (pathfind *current-cell* to max))


;;; Experimental

(defvar *policy* 'hunter)

(defun moving-1 (optimizer function)
  (let ((d (list-directions)))
    (if (null d)
        (progn
          (move (random-direction)))
        (progn
          (move (funcall optimizer d function))))))

(defmacro moving (optimizer &body value-form)
  (let ((dirvar (gensym)))
    `(moving-1 (function ,optimizer)
               (lambda (,dirvar)
                 (hypotesis ,dirvar
                   ,@value-form)))))

(defun update-policy ()
  (let ((path (pathto (random-neighbour *prey-cell*))))
    (cond
      ((null path)
       (setf *policy* 'survive))
      ((<= (length path) 10)
       (setf *policy* 'killer))
      (t
       (setf *policy* 'hunter)))))

(defun survive ()
  (log "survive")
  (moving maximizing
    (if (null (list-directions))
        0
        1)))

(defun hunter ()
  (log "hunter")
  (move-to (or (second (pathto (random-neighbour *prey-cell*)))
               (random-neighbour))))

(defun killer ()
  (log "kill")
  (let ((n 20))
    (moving minimizing
      (- (count n (linearize-array (gradient *prey-cell* n)))
         (count n (linearize-array (gradient *current-cell* n)))))))

(defun run-policy ()
  (update-policy)
  (funcall *policy*))

(defun game-loop ()
  (loop
    (run-policy)
    ;; Update prey cell
    (setf *prey-cell* (read-cords))
    (log "Prey is moving to ~a" *prey-cell*)
    (set-cell-as-busy *prey-cell*)))

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
    (game-loop)))


;;; boa ends here
