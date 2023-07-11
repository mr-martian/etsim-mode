(require 'transient)
(require 'cl)

(defvar etsim-mode-hook nil)

(defvar etsim-cur-sent-start -1)
(defvar etsim-cur-sent-end -1)

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

(defun etsim-beginning-of-sentence ()
  (save-excursion
    (beginning-of-line)
    (unless (= (point) (point-min))
      (search-backward-regexp "^$" (point-min) t))
    (point)))

(defun etsim-end-of-sentence ()
  (save-excursion
    (beginning-of-line)
    (let ((p (point)))
      (search-forward-regexp "^$")
      (if (= p (point))
          (point-max)
        (point)))))

(defun etsim-bound-sentence ()
  (let ((s (etsim-beginning-of-sentence))
        (e (etsim-end-of-sentence)))
    (unless (and (= s etsim-cur-sent-start)
                 (= e etsim-cur-sent-end))
      (setq-local etsim-cur-sent-start s)
      (setq-local etsim-cur-sent-end e))))

(defun etsim-get-arcs (start end)
  nil)

(defun etsim-make-overlays ()
  (cl-flet ((mk-overlay (start col)
                        (let ((o (make-overlay start (point))))
                          (overlay-put o 'etsim-col col))))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (beginning-of-line)
        (when (etsim-on-normal-word)
          (let ((line-start (point))
                (cur-start (point)))
            ; TODO: we're assuming everything is valid
            (dotimes (i 9)
              (search-forward "\t")
              ;(forward-char 1)
              (mk-overlay cur-start (+ i 1))
              (setq cur-start (point)))
            (end-of-line)
            (mk-overlay cur-start 10)
            (mk-overlay line-start 0)))
        (forward-line 1)))))

(defun etsim-align-overlays (ovs)
  (cl-flet ((owidth (o) (- (overlay-end o) (overlay-start o))))
    (let ((width (reduce #'max (mapcar #'owidth ovs))))
      (dolist (o ovs)
        (let ((nw (+ 1 (- width (owidth o)))))
          (overlay-put o 'after-string (make-string nw ?\s))
          (overlay-put o 'etsim-width nw))))))

(defun etsim-overlay-get (o p)
  (plist-get (overlay-properties o) p))

(defun etsim-filter-col (ovs col)
  (remove-if-not (lambda (o)
                   (= (or (etsim-overlay-get o 'etsim-col) -1) col))
                 ovs))

(defun etsim-get-all-column (col)
  (etsim-filter-col (overlays-in (point-min) (point-max)) col))

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
  (search-forward-regexp "^$")
  (next-line))

(defun etsim-setup ()
  (etsim-make-overlays)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((s (etsim-beginning-of-sentence))
             (e (etsim-end-of-sentence))
             (ovs (overlays-in s e)))
        (dotimes (i 10)
          (etsim-align-overlays (etsim-filter-col ovs (+ i 1)))))
      (etsim-next-sentence)))
  ;(etsim-hide-column 5)
  (etsim-hide-column 6)
  (etsim-hide-column 10))

(defun etsim-next-word ()
  (interactive)
  (next-line))

(defun etsim-assign-keys ()
  (define-key etsim-mode-map (kbd "f") #'etsim-next-word))

(define-derived-mode etsim-mode fundamental-mode "CoNLL-U"
  "CoNLL-U with trees"
  (setq-local tab-width 1)
  (read-only-mode)
  (etsim-keys)
  (let ((debug-on-error t))
    (etsim-setup))
  (run-hooks 'etsim-mode-hook))

(add-to-list 'auto-mode-alist '("\\.conllu\\'" . etsim-mode))

(provide 'etsim-mode)
