(require 'cellular-automaton)

(defun* ca-test-cell-state-set-get (sizes
                                    cell-tests
                                    &optional (state-default t))
  "Test state setters/getters on newly created State Table.
`cell-tests' param is a list of cells/cons cells.
Use (cell . state) cons cell to set particular state."
  (let ((state-table (ca-make-state-table :sizes sizes)))
    (dolist (cell-test cell-tests)
      (let ((cell (if (consp cell-test) (car cell-test) cell-test))
            (state (if (consp cell-test) (cdr cell-test) state-default)))
        ;; set state
        (ca-set-cell-state state-table cell state)
        ;; get state and compare
        (let ((state-get (ca-get-cell-state state-table cell)))
          (if (not (ca-states-eq state state-get))
              (error "State set/get error: cell %S, state set %S, state get %S"
                     cell state state-get)))))))

(defun* ca-test-index-calc-offsets (&optional (non-null-state t))
  "Test non-empty cell offsets calculation."
  (let ((state-table (ca-make-state-table :sizes [4 5])))
    ;;  +---+---+---+---+---+
    ;;  | X |   |   |   |   |
    ;;  +---+---+---+---+---+
    ;;  |   |   |   | X |   |
    ;;  +---+---+---+---+---+
    ;;  |   |   |   |   |   |
    ;;  +---+---+---+---+---+
    ;;  |   |   |   |   |   |
    ;;  +---+---+---+---+---+
    ;; offsets: [(0 . 2)  ; rows
    ;;           (0 . 1)] ; cols
    (ca-set-cell-state state-table [0 0] non-null-state)
    (ca-set-cell-state state-table [1 3] non-null-state)
    (let* ((offsets-valid [(0 . 2) (0 . 1)])
           (index (ca-state-table-get state-table :index))
           (offsets (ca--index-calc-offsets index)))
      (if (not (equal offsets offsets-valid))
          (error "Offset calculation error: calculated %S,  expected %S"
                 offsets offsets-valid)
        t))))

(defun ca-test-all ()
  (interactive)
  (ca-test-cell-state-set-get [4 5] '([0 0] [1 3]))
  (ca-test-index-calc-offsets))


;;; *SCRATCH*
(setq a '((:c . [0 0])
          (:n . [0 1])
          (:e . [1 0])
          (:s . [0 -1])
          (:w . [-1 0])))
