(require 'cellular-automaton)

(defconst ca-life-state-alive t)
(defconst ca-life-state-dead nil)

(defconst ca-life-coord-offset
  [1 0]
  "Table coordinates (not cell) offset relative to buffer line/column numbers.")

(defvar ca-life-mode-map
  (make-keymap)
  "`ca-life' mode keymap.")
(define-key ca-life-mode-map (kbd "a") 'ca-life-edit-set-alive-at-pos)
(define-key ca-life-mode-map (kbd "d") 'ca-life-edit-set-dead-at-pos)
(define-key ca-life-mode-map (kbd "<SPC>") 'ca-step)
;; (define-key ca-life-mode-map (kbd "C-c a") 'ca-life-edit-set-alive-at-pos)
;; (define-key ca-life-mode-map (kbd "C-c d") 'ca-life-edit-set-dead-at-pos)
;; (define-key ca-life-mode-map (kbd "C-c n") 'ca-step)


(setq ca-life-char-dead (string-to-char "."))
(setq ca-life-char-alive (string-to-char "*"))
(setq ca-life-char-newline (string-to-char "\n"))

;; `cellular-automaton' FUNCTION PARAMETERS {{
(defun ca-life-show-state-table (state-table)
  (erase-buffer)
  (let ((table (ca-state-table-get state-table :table)))
    (dotimes (row (length table))
      (let ((vector (aref table row)))
        (dotimes (col (length vector))
          (let ((state (aref vector col)))
            (insert-char (ca-life-get-state-char state) 1))))
      (insert-char ca-life-char-newline 1))))

(defun ca-life-transition (neighbor-states)
  (let* ((curr-state (cdr (assoc :c neighbor-states)))
         (is-alive-p (ca-states-eq curr-state ca-life-state-alive))
        (alive-num 0))
    ;; count alive neighbors
    (dolist (neighbor-state neighbor-states)
      (let ((dir (car neighbor-state))
            (state (cdr neighbor-state)))
        (if (and (not (eql dir :c))
                 (not (ca-state-null-p state)))
            (incf alive-num))))
    ;; calc. next state
    (cond ((and is-alive-p
                (or (< alive-num 2) (> alive-num 3)))
           ca-life-state-dead)
          ((and (not is-alive-p)(= alive-num 3))
           ca-life-state-alive)
          (t curr-state))))
;; }}

;; EDITING {{
(defun ca-life-line-col-to-cell (state-table line col)
  "Convert line/column numbers to cell coordinates."
  (let ((coords (ca-vector- (vector line col) ca-life-coord-offset)))
    (ca-state-table-coords-to-cell state-table coords)))

(defun ca-life-edit-set-state-at-pos (state)
  ;; TODO: add validation
  (let* ((state-table (ca-get-state-table))
        (cell (ca-life-line-col-to-cell state-table
                                        (line-number-at-pos)
                                        (current-column))))
    (ca-set-cell-state state-table cell state)))

(defun ca-life-edit-set-alive-at-pos ()
  (interactive)
  (when (ca-life-edit-set-state-at-pos ca-life-state-alive)
    (insert-char (ca-life-get-state-char ca-life-state-alive) 1)
    (delete-char 1)
    (backward-char)))

(defun ca-life-edit-set-dead-at-pos ()
  (interactive)
  (when (ca-life-edit-set-state-at-pos ca-life-state-dead)
    (insert-char (ca-life-get-state-char ca-life-state-dead) 1)
    (delete-char 1)
    (backward-char)))
;; }}

(defun ca-life-get-state-char (state)
  (if (ca-state-null-p state)
      ca-life-char-dead
    ca-life-char-alive))

;; INITIALIZATION {{
(defun ca-life-init-buffer ()
  (erase-buffer))

(defun ca-life-init-test ()
  (ca-set-state-table (ca-make-state-table))
  (let ((state-table (ca-get-state-table)))
    (dolist (cell '([1 2] [2 3] [3 1] [3 2] [3 3]))
      (ca-set-cell-state state-table cell ca-life-state-alive))
    )
  (ca-show-state-table))

(defun ca-life-init-vars ()
  "Set `cellular-automaton' buffer local variables."
  (setq ca-init-sizes [10 10])
  (setq ca-dim-loop [nil nil])
  (setq ca-min-sizes [10 10])
  (setq ca-dimension-num 2)

  ;;
  ;;      columns ->
  ;; rows +---+---+---+
  ;;   |  | NW| N | NE|
  ;;   V  +---+---+---+
  ;;      | W | C | E |
  ;;      +---+---+---+
  ;;      | SW| S | SE|
  ;;      +---+---+---+
  ;;
  ;; [row column]
  (setq ca-dir-list
        '((:c  . [0 0])
          (:n  . [-1 0])
          (:ne . [-1 1])
          (:e  . [0 1])
          (:se . [1 1])
          (:s  . [1 0])
          (:sw . [1 -1])
          (:w  . [0 -1])
          (:nw . [-1 -1])))
  (setq ca-space-state ca-life-state-dead)
  (setq ca-show-state-table-func 'ca-life-show-state-table)
  (setq ca-transition-func 'ca-life-transition))
;; }}

(define-derived-mode ca-life-mode fundamental-mode
  "ca-life"
  "Game of Life.")

(defun ca-life ()
  (interactive)
  (ca-life-mode)
  (switch-to-buffer "*ca-life*")
  (ca-life-init-buffer)
  (ca-life-init-vars)
  (ca-life-init-test)
  (use-local-map ca-life-mode-map))