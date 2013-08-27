;;;; ALGORITHM
;;
;; Set inital State Table
;; Repeat forever {{
;;   I.  Show current State Table:
;;       (Use external function)
;;
;;   II. Compute new state:
;;      Calc. new State Table size
;;      Calc. new State Table offset
;;      Create new State Table
;;      For each Cell in old State Table {
;;        Get adjacent Cell states (direction -> state)
;;        Compute new Cell state
;;        Set to new State Table
;;      }
;; }}

;;;; REQUIREMENTS/IMPLEMENTATION NOTES
;; A multi-dimensional matrix (vector of vectors) is used as automaton table.
;; Program should be able to handle any number of dimensions.
;;
;; Automaton should be able to "grow" indefinitely in any direction
;; (if space is not looped).
;; - if non-empty cell lies on space boundary, automaton should grow in
;;   corresponding direction on next step
;;   (TODO: expansion rate dependent on neighborhood radius);
;; - automaton should keep track of coordinate origin ([a1 a2 ...] cell should
;;   have same coordinates in each State Table);
;; - space loops (not implemented yet)

;;;; VARIABLE NAMES
;; - offset       (left . right) cons cell, State Table offsets along some
;;                dimension;
;; - size-change  (left . right) cons cell, next gen. State Table size changes
;;                for some dimension
;; - cell         cell coordinate vector;
;; - coords       coordinate vector in actual multidimensional state array;
;; - index        array of arrays of non-empty cell numbers for each coordinate;
;; - origin       [0 0 ...] cell offset in table;

(defvar ca-space-state
  nil
  "Empty cell state.")
(make-variable-buffer-local 'ca-state-table)

(defvar ca-state-table
  nil
  "Automaton cell State Table.")
(make-variable-buffer-local 'ca-state-table)

(defvar ca-dim-num
  2
  "State Table dimensionality.")
(make-variable-buffer-local 'ca-dim-num)

(defvar ca-init-sizes
  [10 10]
  "Initial State Table size (along each dimension).")
(make-variable-buffer-local 'ca-init-sizes)

(defvar ca-min-sizes
  [10 10]
  "Minimal sizes along each dimension.")

(defvar ca-dim-loop
  [nil nil])
(make-variable-buffer-local 'ca-dim-loop)

(defvar ca-dir-list-default
  '((:c  . [0 0])
    (:n  . [0 1])
    (:ne . [1 1])
    (:e  . [1 0])
    (:se . [1 -1])
    (:s  . [0 -1])
    (:sw . [-1 -1])
    (:w  . [-1 0])
    (:nw . [-1 1]))
  "Default direction vectors.")

(defvar ca-dir-list
  ca-dir-list-default
  "Direction vectors.")
(make-variable-buffer-local 'ca-dir-list)

(defvar ca-transition-default
  (lambda (neighbor-states)
    (cdr (assoc :c neighbor-states)))
  "Default state transition rule function.")

(defvar ca-transition-func
  ca-transition-default
  "Next state transition rule function.")
(make-variable-buffer-local 'ca-transition-func)

(defvar ca-show-state-table-func
  (lambda (state-table))
  "Function to display State Table.")

(defun ca-transition (neighbor-states)
  (funcall ca-transition-func neighbor-states))

(defun ca-set-state-table (state-table)
  (setq ca-state-table state-table))

(defun ca-get-state-table ()
  ca-state-table)

(defun ca-show-state-table ()
  (let ((state-table (ca-get-state-table)))
    (funcall ca-show-state-table-func state-table)))

(defun ca-step ()
  (interactive)
  (let ((state-table (ca-get-state-table)))
    (ca-set-state-table (ca-make-next-state-table state-table)))
  (ca-show-state-table))

;; VECTOR OPERATIONS {{
(defun ca-vector+ (v1 v2)
  (let* ((len1 (length v1))
         (len2 (length v2))
         (len (max len1 len2))
         (result (make-vector len nil)))
    (dotimes (i len result)
      (let ((a1 (if (< i len1) (aref v1 i) 0))
            (a2 (if (< i len2) (aref v2 i) 0)))
        (aset result i (+ a1 a2))))))

(defun ca-vector- (v1 v2)
  (ca-vector+ v1 (ca-vector-inv v2)))

(defun ca-vector-inv (v)
  (map 'vector '- v))
;; }}

(defun* ca-make-state-table (&key (sizes ca-init-sizes)
                                  (origin (make-vector ca-dim-num 0))
                                  (generation 1))
  "Create empty State Table."
  (let ((state-table nil)
        (table-block ca-space-state))
    (dotimes (d ca-dim-num)
      (let ((size (elt sizes (- ca-dim-num 1 d))))
        (setq state-table (make-vector size nil))
        (dotimes (i size)
          (aset state-table i (copy-seq table-block))))
      (setq table-block state-table))
    `(:table ,state-table
      :sizes ,sizes
      :origin ,origin
      :generation ,generation
      :index ,(ca--make-table-index sizes))))

(defun ca-state-null-p (state)
  (ca-states-eq state ca-space-state))

(defun ca-dim-looped-p (dim)
  "Check if space is looped for dimension."
  (aref ca-dim-loop dim))

(defun ca-state-table-get (state-table property)
  (plist-get state-table property))

(defun ca-cell-in-state-table-p (state-table cell)
  "Check if cell is in State Table."
  (let ((coords (ca-cell-to-state-table-coords state-table cell)))
    (ca-state-table-coords-valid-p state-table coords)))

(defun ca-get-cell-state (state-table cell)
  "Get cell state. Return ca-space-state if cell is not in State Table."
  (if (ca-cell-in-state-table-p state-table cell)
      (let ((coords (ca-cell-to-state-table-coords state-table cell))
            (table (ca-state-table-get state-table :table)))
        (ca--table-get-state table coords))
    ca-space-state))

(defun ca-set-cell-state (state-table cell state)
  "Set cell state. Returns t on success, nil otherwise."
  (let ((coords (ca-cell-to-state-table-coords state-table cell)))
    (if (ca-state-table-coords-valid-p state-table coords)
        (let* ((table (ca-state-table-get state-table :table))
              (index (ca-state-table-get state-table :index))
              (state-old (ca--table-get-state table coords)))
          ;; set cell state
          (ca--table-set-state table coords state)
          ;; update index
          (ca--update-index index coords state state-old)
          t)
      nil)))

(defun ca-state-table-coords-valid-p (state-table coords)
  (let ((valid-p t)
        (sizes (ca-state-table-get state-table :sizes))
        (d 0))
    (while (and valid-p (< d ca-dim-num))
      ;; check coord range only if space is not looped
      (if (not (ca-dim-looped-p d))
          (let ((coord (aref coords d))
                (size (aref sizes d)))
            (setq valid-p (and (>= coord 0)
                               (< coord size)))))
      (incf d))
    valid-p))

(defun ca-make-next-state-table (state-table)
  "Create new State Table from given State Table,
set cell states using transition rules."
  ;; calc. new State Table sizes and origin
  (let* ((index (ca-state-table-get state-table :index))
         (sizes (ca-state-table-get state-table :sizes))
         (size-changes (ca--index-calc-size-changes index sizes))
         (sizes-new (ca-vector+ sizes
                                (map 'vector
                                     (lambda (c) (+ (car c) (cdr c)))
                                     size-changes)))
         (origin-new (ca-vector+ (ca-state-table-get state-table :origin)
                             (map 'vector
                                  (lambda (c) (car c))
                                  size-changes)))
         (generation-new (1+ (ca-state-table-get state-table :generation))))
    ;; create State Table; set cell states
    (let ((state-table-new (ca-make-state-table :sizes sizes-new
                                                :origin origin-new
                                                :generation generation-new))
          (state-table-old state-table))
      (ca-do-each-cell
       state-table-new
       (lambda (cell state-table-old state-table-new)
         (let ((state-new (ca-get-next-cell-state state-table-old cell)))
           (ca-set-cell-state state-table-new cell state-new)))
       state-table-old
       state-table-new)
      state-table-new)))

(defun ca-do-each-cell (state-table callback &rest args)
  "Call given function for each cell."
  (ca--do-each-cell-rec state-table [] 0 callback args))

;; STATE TABLE (INTERNAL)/SPACE COORDINATES CONVERSION {{
(defun ca-cell-to-state-table-coords (state-table cell)
  (let* ((sizes (ca-state-table-get state-table :sizes))
         (coords (ca-vector+ cell (ca-state-table-get state-table :origin))))
    (dotimes (d ca-dim-num)
      (if (ca-dim-looped-p d)
          (let ((size (aref sizes d))
                (coord (aref coords d)))
            (aset coords d (mod coord size)))))
    coords))

(defun ca-state-table-coords-to-cell (state-table coords)
  (let ((origin (ca-state-table-get state-table :origin)))
    (ca-vector- coords origin)))
;; }}

(defun ca-get-next-cell-state (state-table cell)
  (let ((neighbor-states (ca-get-cell-neighbor-states-all state-table cell)))
    (ca-transition neighbor-states)))

(defun ca-get-cell-neighbor-states-all (state-table cell)
  (let ((neighbors '()))
    (dolist (dir (ca-get-dirs))
      (let ((state (ca-get-cell-neighbor-state state-table cell dir)))
        (push (cons dir state) neighbors)))
    neighbors))

(defun ca-get-dirs ()
  (mapcar 'car ca-dir-list))

(defun ca-get-dir-vector (dir)
  (cdr (assoc dir ca-dir-list)))

(defun ca-get-cell-neighbor-state (state-table cell dir)
  (let ((vector (ca-get-dir-vector dir)))
    (ca-get-cell-state state-table (ca-vector+ cell vector))))

(defun ca-states-eq (state1 state2)
  (equal state1 state2))

(defun ca-get-min-size (dim)
  (aref ca-min-sizes dim))


;;; PROTECTED
(defun ca--do-each-cell-rec (state-table coords d callback args)
  (if (= d ca-dim-num)
      (apply
       callback
       (ca-state-table-coords-to-cell state-table coords)
       args)
    (let* ((sizes (ca-state-table-get state-table :sizes))
           (size (aref sizes d)))
      (dotimes (coord size)
        (ca--do-each-cell-rec
         state-table
         (vconcat coords (make-vector 1 coord))
         (1+ d)
         callback
         args)))))

(defun ca--table-set-state (table coords state)
  (let ((vector table))
    (dotimes (d (1- ca-dim-num)
                (aset vector (aref coords (1- ca-dim-num)) state))
      (setq vector (aref vector (aref coords d))))))

(defun ca--table-get-state (table coords)
  (let ((state table))
    (dotimes (d ca-dim-num state)
      (let ((coord (aref coords d)))
        (setq state (aref state coord))))))

(defun ca--calc-size-changes (offsets sizes)
  (let ((size-changes (make-vector ca-dim-num nil)))
    (dotimes (d ca-dim-num)
      (if (ca-dim-looped-p d)
          ;; do not change size if space is looped for given dimension
          (aset size-changes d (cons 0 0))
        (let ((offset (aref offsets d))
              (size (aref sizes d)))
          (aset size-changes d (ca--calc-dim-size-change offset size d)))))
    size-changes))

(defun ca--calc-dim-size-change (offset size dim)
  "Calculate size changes along given dimension
with respect to `ca-min-size' value.
Offset parameter should be a cons cell (<left offset> . <right offset>)"
  ;;
  ;;  table size along given dimension (size)
  ;; |--------------------------------------------|
  ;;  minimal size (ca-get-min-size dim)    delta
  ;; |----------------------------------|---------|
  ;;   -left                            -right
  ;; |--------|                    |--------------|
  ;;
  (let ((left (- 1 (car offset)))
        (right (- 1 (cdr offset)))
        (delta (- size (ca-get-min-size dim))))
    (setq right (max (- delta) right))
    (setq left (max left (- (+ delta right))))
    (cons left right)))

;; TABLE INDEX {{
(defun ca--make-table-index (sizes)
  "Create empty State Table index."
  (let ((index (make-vector ca-dim-num nil)))
    (dotimes (d ca-dim-num)
      (let ((size (elt sizes d)))
        (aset index d (make-vector size 0))))
    index))

(defun ca--update-index (index coords state-new state-old)
  (let ((state-new-null-p (ca-state-null-p state-new))
        (state-old-null-p (ca-state-null-p state-old)))
    (if (or (and state-old-null-p (not state-new-null-p))
            (and (not state-old-null-p) state-new-null-p))
        (let ((num-change (if state-new-null-p -1 1)))
          (dotimes (d ca-dim-num)
            (let* ((coord (aref coords d))
                   (index-vector (aref index d))
                   (index-num (aref index-vector coord)))
              (aset index-vector coord (max 0 (+ index-num num-change)))))))))

(defun ca--index-calc-offsets (index)
  (let ((offsets (make-vector ca-dim-num nil)))
    (dotimes (d ca-dim-num)
      (let ((index-vector (aref index d)))
        (aset offsets d (ca--index-calc-dim-offsets index-vector))))
    offsets))

(defun ca--index-calc-dim-offsets (index-vector)
  (let ((len (length index-vector))
        (left 0)
        (right 0))
    (while (and (< left len)
                (= 0 (aref index-vector left)))
      (incf left))
    (while (and (< right len)
                (= 0 (aref index-vector (- len right 1))))
      (incf right))
    (cons left right)))

(defun ca--index-calc-size-changes (index sizes)
  (let ((offsets (ca--index-calc-offsets index)))
    (ca--calc-size-changes offsets sizes)))
;; }}

(provide 'cellular-automaton)