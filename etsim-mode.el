(require 'transient)
(require 'cl-lib)

(defvar etsim-mode-hook nil)
(defvar etsim-sentence-overlays nil)

(defun etsim-on-normal-word ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[0-9]+\t")))

(defun etsim-on-any-word ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[0-9]")))

(defun etsim-is-last-word ()
  (save-excursion
    (next-line)
    (beginning-of-line)
    (looking-at "$")))

(defun etsim-is-first-word ()
  (save-excursion
    (previous-line)
    (beginning-of-line)
    (not (looking-at "[0-9]"))))

(defun etsim-goto-beginning-of-sentence ()
  (let ((p (point)))
    (search-backward-regexp "^$" (point-min) t)
    (when (and (= p (point)) (not (= (point) (point-min))))
      (goto-char (point-min)))))

(defun etsim-beginning-of-sentence ()
  (save-excursion
    (etsim-goto-beginning-of-sentence)
    (point)))

(defun etsim-end-of-sentence ()
  (save-excursion
    (beginning-of-line)
    (let ((p (point)))
      (search-forward-regexp "^$" (point-max) t)
      (if (= p (point))
          (point-max)
        (point)))))

(defun etsim-make-overlays ()
  (cl-flet ((mk-overlay (start col)
                        (let ((o (make-overlay
                                  start
                                  (save-excursion
                                    (forward-char -1)
                                    (if (looking-at "\t")
                                        (point)
                                      (1+ (point)))))))
                          (overlay-put o 'etsim-col col)
                          (when (= col 10)
                            (overlay-put o 'priority 2))
                          o)))
    (save-excursion
      (let ((sent-start nil)
            (sentence '()))
        (goto-char (point-min))
        (while (not (eobp))
          (beginning-of-line)
          (when (and (not sent-start) (not (looking-at "$")))
            (setq sent-start (point)))
          (when (and sent-start (looking-at "$"))
            (let ((o (make-overlay sent-start (- (point) 1))))
              (overlay-put o 'etsim-col -1)
              (overlay-put o 'etsim-rows (reverse sentence))
              (setq sentence nil)
              (push o etsim-sentence-overlays)
              (setq sent-start nil)))
          (when (etsim-on-any-word)
            (let ((line-start (point))
                  (cur-start (point))
                  (line-end (save-excursion (end-of-line) (point)))
                  (row (make-vector 11 nil)))
              ; TODO: we're assuming everything is valid
              (dotimes (i 9)
                (search-forward "\t" line-end t)
                (aset row (+ i 1) (mk-overlay cur-start (+ i 1)))
                (setq cur-start (point)))
              (end-of-line)
              (aset row 10 (mk-overlay cur-start 10))
              (aset row 0 (mk-overlay line-start 0))
              (push row sentence)))
          (forward-line 1))))))

(defun etsim-align-overlays (ovs)
  (cl-flet ((owidth (o) (- (overlay-end o) (overlay-start o))))
    (let ((width (cl-reduce #'max (cons 0 (mapcar #'owidth ovs)))))
      (dolist (o ovs)
        (let ((nw (- width (owidth o))))
          (overlay-put o 'after-string (make-string nw ?\s))
          (overlay-put o 'etsim-width nw))))))

(defun etsim-overlay-get (o p)
  (plist-get (overlay-properties o) p))

(defun etsim-filter-col (ovs col)
  (cl-remove-if-not (lambda (o)
                      (= (or (etsim-overlay-get o 'etsim-col) -1) col))
                    ovs))

(defun etsim-get-all-column (col)
  (etsim-filter-col (overlays-in (point-min) (point-max)) col))

(defun etsim-get-word-field (col)
  (save-excursion
    (let* ((s (progn (beginning-of-line) (point)))
           (e (progn (end-of-line) (point)))
           (ovs (etsim-filter-col (overlays-in s e) col)))
      (when (> (length ovs) 0) (car ovs)))))

(defun etsim-overlay-string (o)
  (buffer-substring (overlay-start o) (overlay-end o)))

(defun etsim-get-word-column (col)
  (let ((o (etsim-get-word-field col)))
    (if o (etsim-overlay-string o) "")))

(defun etsim-get-sentence-overlay ()
  (let ((ovs (overlays-at (point))))
    (cl-block fn
      (dolist (o ovs)
        (when (eq (etsim-overlay-get o 'etsim-col) -1)
          (cl-return-from fn o))))))

(defun etsim-get-sentence-data ()
  (let ((o (etsim-get-sentence-overlay)))
    (when o (etsim-overlay-get o 'etsim-rows))))

(defun etsim-show-column (col)
  (interactive)
  (mapcar (lambda (o)
            (overlay-put o 'invisible nil)
            (overlay-put o 'after-string
                         (make-string (etsim-overlay-get o 'etsim-width) ?\s)))
          (etsim-get-all-column col)))

(defun etsim-hide-column (col)
  (interactive)
  (mapcar (lambda (o)
            (overlay-put o 'invisible t)
            (overlay-put o 'after-string ""))
          (etsim-get-all-column col)))

(defun etsim-next-sentence ()
  (interactive)
  (search-forward-regexp "^$" (point-max) t)
  (next-line))

(defun etsim-get-arcs (sent-data)
  (cl-flet ((numberize (ov)
                       (when ov (string-to-number (etsim-overlay-string ov)))))
    (let* ((arr (apply #'vector
                       (mapcar (lambda (ln)
                                 (let ((i (numberize (elt ln 1)))
                                       (h (numberize (elt ln 7))))
                                   (if (and i h)
                                       (list (elt ln 0) i h 0 (= h 0))
                                     (list (elt ln 0) nil nil 0 t))))
                               sent-data)))
           (ln (length arr))
           (todo-count 1))
      (while (> todo-count 0)
        (setq todo-count 0)
        (dotimes (i ln)
          (unless (nth 4 (elt arr i))
            (let* ((h-min 0)
                   (clear t)
                   (wi (elt arr i))
                   (wii (cadr wi))
                   (wih (nth 2 wi)))
              (dotimes (j ln)
                (let* ((wj (elt arr j)) (wji (cadr wj)))
                  (when (and (cadr wj) ; it's a word
                             (or (< wii wji wih)
                                 (< wih wji wii))
                             ; break infinite loop in non-projective
                             (not (< (nth 2 wj) wii wji wih)))
                    (when (> (nth 3 wj) h-min)
                      (setq h-min (nth 3 wj)))
                    (unless (nth 4 wj)
                      (setq clear nil)))))
              (setf (elt arr i) (list (car wi) wii wih (1+ h-min) clear))
              (unless clear (setq todo-count (1+ todo-count)))))))
      (let* ((str-width (1+ (* 2 (apply #'max
                                        (mapcar (lambda (l) (nth 3 l)) arr)))))
             (strs (apply #'vector (mapcar
                                    (lambda (_) (make-vector str-width 0))
                                    arr))))
        (dotimes (i ln)
          (let* ((on nil)
                 (wi (elt arr i))
                 (idx (cadr wi))
                 (head (nth 2 wi))
                 (height (* 2 (nth 3 wi))))
            (dotimes (j ln)
              (let ((wj (elt arr j)))
                (cond
                 ((= (cadr wj) idx)
                  (if (= head 0)
                      (dotimes (k str-width)
                        (aset (elt strs j) k
                              (logior (elt (elt strs j) k) 5)))
                    (progn
                      (setq on (not on))
                      (dotimes (k height)
                        (aset (elt strs j) k
                              (logior (elt (elt strs j) k) 5)))
                      (aset (elt strs j) height
                            (logior (elt (elt strs j) height)
                                    (if (< head idx) 3 9))))))
                 ((= (cadr wj) head)
                  (setq on (not on)))
                 (on (aset (elt strs j) height
                           (logior (elt (elt strs j) height) 10))))))))
        (let ((chars [?\s ?╴ ?╵ ?┘ ?╶ ?─ ?└ ?┴ ?╷ ?┐ ?│ ?┤ ?┌ ?┬ ?├ ?┼]))
          (dotimes (i ln)
            (let* ((ov (car (elt arr i)))
                   (nm (elt strs i))
                   (s (make-string (1+ (length nm)) ?\s)))
              (dotimes (j (length nm))
                (aset s j (elt chars (elt nm j))))
              (overlay-put ov 'after-string s))))))))

(defun etsim-refresh-sentence ()
  (let ((data (etsim-get-sentence-data)))
    (dotimes (i 10)
      (etsim-align-overlays (mapcar (lambda (ln) (elt ln (1+ i))) data)))
    (etsim-get-arcs data)))

(defvar etsim-highlighted-overlays nil)

(defun etsim-clear-highlights ()
  (dolist (o etsim-highlighted-overlays)
    (overlay-put o 'face nil))
  (setq etsim-highlighted-overlays nil))

(defun etsim-set-highlights ()
  (when (etsim-on-normal-word)
    (let ((p (etsim-get-word-column 7))
          (data (etsim-get-sentence-data)))
      (dolist (line data)
        (cond ((string= p (etsim-overlay-string (elt line 1)))
               (overlay-put (elt line 0) 'face 'highlight)
               (push (elt line 0) etsim-highlighted-overlays))
              ((string= p (etsim-overlay-string (elt line 7)))
               (overlay-put (elt line 0) 'face 'underline)
               (push (elt line 0) etsim-highlighted-overlays)))))))

(defun etsim-setup ()
  (etsim-make-overlays)
  (save-excursion
    (dolist (ov etsim-sentence-overlays)
      (goto-char (overlay-start ov))
      (etsim-refresh-sentence)))
  (add-hook 'pre-command-hook #'etsim-clear-highlights)
  (add-hook 'post-command-hook #'etsim-set-highlights))

(defun etsim-next-word ()
  (interactive)
  (when (and (etsim-on-any-word) (not (etsim-is-last-word)))
    (next-line)))

(defun etsim-prev-word ()
  (interactive)
  (when (and (etsim-on-any-word) (not (etsim-is-first-word)))
    (previous-line)))

(defvar etsim-marked-head nil)
(defvar etsim-marked-dep nil)

(defun etsim-overlay-replace (ov str)
  (save-excursion
    (let ((s (overlay-start ov))
          (e (overlay-end ov))
          (inhibit-read-only t))
      (delete-overlay ov)
      (delete-region s e)
      (goto-char s)
      (insert str)
      (move-overlay ov s (+ s (length str))))))

(defun etsim-set-overlay-indicator (ov ch)
  (let ((s (etsim-overlay-get ov 'after-string)))
    (aset s (- (length s) 1) ch)
    (overlay-put ov 'after-string s)))

(defun etsim-jump-to-word (wid)
  (cl-block fn
    (let ((loc (point)))
      (goto-char (etsim-beginning-of-sentence))
      (while (and (not (etsim-is-last-word)))
        (when (and (etsim-on-normal-word)
                   (string= (etsim-get-word-column 1) wid))
          (cl-return-from fn t))
        (next-line))
      (goto-char loc))
    (cl-return-from fn nil)))

(defun etsim-clear-parent ()
  (when etsim-marked-head
    (etsim-set-overlay-indicator (car etsim-marked-head) ?\s)
    (setq etsim-marked-head nil)))

(defun etsim-clear-dep ()
  (when etsim-marked-dep
    (etsim-set-overlay-indicator (car etsim-marked-dep) ?\s)
    (setq etsim-marked-dep nil)))

(defun etsim-mark-parent ()
  (interactive)
  (when (etsim-on-normal-word)
    (let ((o (etsim-get-word-field 0)))
      (cond
       ((equal (car etsim-marked-head) o) (etsim-clear-parent))
       (etsim-marked-dep
        (save-excursion
          (let ((h (etsim-get-word-column 1)))
            (goto-char (overlay-start (car etsim-marked-dep)))
            (etsim-overlay-replace (etsim-get-word-field 7) h)))
        (etsim-clear-dep)
        (etsim-refresh-sentence))
       (t
        (etsim-clear-parent)
        (setq etsim-marked-head (list o (etsim-get-word-column 1)))
        (etsim-set-overlay-indicator o ?h))))))

(defun etsim-mark-dep ()
  (interactive)
  (when (etsim-on-normal-word)
    (let ((o (etsim-get-word-field 0)))
      (cond
       ((equal (car etsim-marked-dep) o) (etsim-clear-dep))
       (etsim-marked-head
        (etsim-overlay-replace (etsim-get-word-field 7) (cadr etsim-marked-head))
        (etsim-clear-parent)
        (etsim-refresh-sentence))
       (t
        (etsim-clear-dep)
        (setq etsim-marked-dep (list o (etsim-get-word-column 1)))
        (etsim-set-overlay-indicator o ?d))))))

(defun etsim-set-root ()
  (interactive)
  (when (etsim-on-normal-word)
    (let ((head (etsim-get-word-field 7))
          (rel (etsim-get-word-field 8)))
      (etsim-overlay-replace rel "root")
      (etsim-overlay-replace head "0")
      (etsim-refresh-sentence))))

(defun etsim-goto-parent ()
  (interactive)
  (when (etsim-on-normal-word)
    (etsim-jump-to-word (etsim-get-word-column 7))))

(defun etsim-edit-relation ()
  (interactive)
  (when (etsim-on-normal-word)
    (etsim-overlay-replace
     (etsim-get-word-field 8)
     (let ((r (etsim-get-word-column 8)))
       (read-string "Relation: " r :default r)))
    (etsim-refresh-sentence)))

(defun etsim-assign-keys ()
  (define-key etsim-mode-map (kbd "b") #'etsim-prev-word)
  (define-key etsim-mode-map (kbd "d") #'etsim-mark-dep)
  (define-key etsim-mode-map (kbd "f") #'etsim-next-word)
  (define-key etsim-mode-map (kbd "h") #'etsim-mark-parent)
  (define-key etsim-mode-map (kbd "p") #'etsim-goto-parent)
  (define-key etsim-mode-map (kbd "r") #'etsim-set-root)
  (define-key etsim-mode-map (kbd "SPC") #'etsim-edit-relation)
  (cl-macrolet ((defkey (i)
                  (let ((ks (format "S %d" i))
                        (kh (format "H %d" i))
                        (col (if (= i 0) 10 i)))
                    `(progn
                       (define-key etsim-mode-map (kbd ,ks)
                         (lambda () (interactive) (etsim-show-column ,col)))
                       (define-key etsim-mode-map (kbd ,kh)
                         (lambda () (interactive) (etsim-hide-column ,col)))))))
    (defkey 0)
    (defkey 1)
    (defkey 2)
    (defkey 3)
    (defkey 4)
    (defkey 5)
    (defkey 6)
    (defkey 7)
    (defkey 8)
    (defkey 9)))

(define-derived-mode etsim-mode fundamental-mode "CoNLL-U"
  "CoNLL-U with trees"
  (setq-local tab-width 1)
  (read-only-mode)
  (etsim-assign-keys)
  (let ((debug-on-error t))
    (etsim-setup))
  (run-hooks 'etsim-mode-hook))

(add-to-list 'auto-mode-alist '("\\.conllu\\'" . etsim-mode))

(provide 'etsim-mode)
