(require 'cellular-automaton)

(defconst ca-gkl-state-excited t)
(defconst ca-gkl-state-quiescent nil)

(defconst ca-gkl-char-excited (string-to-char "*"))
(defconst ca-gkl-char-quiescent (string-to-char "."))
(defconst ca-gkl-char-newline (string-to-char "\n"))

(defvar
  ca-gkl-current-line
  1
  "Line number to show current generation")
(make-variable-buffer-local 'ca-gkl-current-line)

(defvar
  ca-gkl-size
  100
  "CA size")
(make-variable-buffer-local 'ca-gkl-size)

(defvar ca-gkl-mode-map
  (make-keymap)
  "`ca-gkl' mode keymap.")
(define-key ca-gkl-mode-map (kbd "a") 'ca-gkl-edit-set-alive-at-pos)
(define-key ca-gkl-mode-map (kbd "d") 'ca-gkl-edit-set-dead-at-pos)
(define-key ca-gkl-mode-map (kbd "<SPC>") 'ca-step)

;; `cellular-automaton' FUNCTION PARAMETERS {{
(defun ca-gkl-show-state-table (state-table)
  (message "ca-gkl-show-state-table")
  (ca-do-each-cell
   state-table
   (lambda (cell)
     (let ((state (ca-get-cell-state state-table cell)))
       (ca-gkl-goto-current-line)
       (insert-char (ca-gkl-get-state-char state) 1))))
  (insert-char ca-gkl-char-newline 1)
  (incf ca-gkl-current-line))

(defun ca-gkl-transition (neighbor-states)
  (let* ((curr-state (cdr (assoc :c neighbor-states)))
        (voters (if (ca-states-eq curr-state ca-gkl-state-quiescent)
                    (list :c :l :ll)
                  (list :c :r :rr)))
        (counter 0))
    (dolist (dir voters)
      (let ((state (cdr (assoc dir neighbor-states))))
        (if (ca-states-eq state ca-gkl-state-excited)
            (incf counter))))
    (if (> counter 1)
        ca-gkl-state-excited
      ca-gkl-state-quiescent)))
;; }}

;; EDITING {{
(defun ca-gkl-edit-set-alive-at-pos ()
  (interactive)
  (when (ca-gkl-edit-set-state-at-pos ca-gkl-state-excited)
    (insert-char ca-gkl-char-excited 1)
    (delete-char 1)
    (backward-char)))

(defun ca-gkl-edit-set-dead-at-pos ()
  (interactive)
  (when (ca-gkl-edit-set-state-at-pos ca-gkl-state-quiescent)
    (insert-char ca-gkl-char-quiescent 1)
    (delete-char 1)
    (backward-char)))

(defun ca-gkl-edit-set-state-at-pos (state)
  (if (equal (+ 1 (line-number-at-pos)) ca-gkl-current-line)
      (let* ((state-table (ca-get-state-table))
             (cell (ca-state-table-coords-to-cell state-table
                                                  (vector (current-column)))))
        (ca-set-cell-state state-table cell state)
        t)
    nil))
;; }}

(defun ca-gkl-goto-current-line ()
  (goto-char (point-min))
  (forward-line ca-gkl-current-line))

(defun ca-gkl-get-state-char (state)
  (if (ca-states-eq state ca-gkl-state-excited)
      ca-gkl-char-excited
    ca-gkl-char-quiescent))

;; INITIALIZATION {{
(defun ca-gkl-init-buffer ()
  (erase-buffer))

(defun ca-gkl-init (p)
  (ca-set-state-table (ca-make-state-table))
  (let ((state-table (ca-get-state-table))
        (init-state (ca-gkl-generate-init-state p)))
    (dotimes (d ca-gkl-size)
      (ca-set-cell-state state-table (make-vector 1 d) (aref init-state d))))
  (ca-show-state-table))

(defun ca-gkl-generate-init-state (p)
  (let* ((excited-num (round (* p ca-gkl-size)))
        (quiescent-num (- ca-gkl-size excited-num))
        (init-state (make-vector ca-gkl-size ca-gkl-state-quiescent)))
    (dotimes (i excited-num)
      ;; pos = # of "0" to be replaced with "1"
      (let ((pos (1+ (random (- ca-gkl-size i))))
            (j 0))
        (dotimes (k pos)
          (if (> k 0)
              ;; move to next position ("0" or "1")
              ;;   j->j
              ;; ..0  x
              (incf j))
          ;; skip all "1"s
          ;;   j-------->j
          ;; ..0  1...1  0
          (while (ca-states-eq (aref init-state j) ca-gkl-state-excited)
            (incf j)))
        (aset init-state j ca-gkl-state-excited)))
    init-state))


(defun ca-gkl-init-vars (size)
  "Set `cellular-automaton' buffer local variables."
  (setq ca-gkl-size size)
  (setq ca-init-sizes (make-vector 1 ca-gkl-size))
  (setq ca-dim-num 1)
  (setq ca-dim-loop [t])
  (setq ca-dir-list
        '((:c  . [0])
          (:l  . [-1])
          (:ll . [-3])
          (:r  . [1])
          (:rr . [3])))
  (setq ca-space-state ca-gkl-state-quiescent)
  (setq ca-show-state-table-func 'ca-gkl-show-state-table)
  (setq ca-transition-func 'ca-gkl-transition))
;; }}

(define-derived-mode ca-gkl-mode fundamental-mode
  "ca-gkl"
  "Gaks-Kurdiumov-Levin cellular automaton.")

(defun ca-gkl (density)
  (interactive (let ((number (string-to-number (read-string "Density: "))))
                 (list (min 1 (max 0 number)))))
  (switch-to-buffer "*ca-gkl*")
  (ca-gkl-mode)
  (ca-gkl-init-buffer)
  (ca-gkl-init-vars 149)
  (ca-gkl-init density)
  (use-local-map ca-gkl-mode-map))

(provide 'ca-gkl)
