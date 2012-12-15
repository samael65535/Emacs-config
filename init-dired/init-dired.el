;; 为dired mode 添加一個toggle顯示目錄下隱藏文件的操作
;; (defun dired-toggle-display-hinden-file ()
;;   "Toggle to display hiden files when dired"
;;   (interactive)
;;   (let ((lsarg dired-listing-switches) (dir (dired-current-directory)))
;;     (if (equal lsarg "-lh")
;;         (setq lsarg "-alh")
;;       (setq lsarg "-lh"))
;;     (setq dired-listing-switches lsarg)
;;     ;; dired's rule:"If dirname is already in a dired buffer,
;;     ;; that buffer is used without refresh." so kill first
;;     (kill-buffer (current-buffer))
;;     (dired dir))
;; )
(add-hook 'dired-mode-hook (lambda ()
  (interactive)
  (make-local-variable  'dired-sort-map)
  (setq dired-sort-map (make-sparse-keymap))
  (define-key dired-mode-map "s" dired-sort-map)
  (define-key dired-sort-map "s"
              '(lambda () "sort by Size"
                (interactive) (dired-sort-other (concat dired-listing-switches "S"))))
  (define-key dired-sort-map "x"
              '(lambda () "sort by eXtension"
                 (interactive) (dired-sort-other (concat dired-listing-switches "X"))))
  (define-key dired-sort-map "t"
              '(lambda () "sort by Time"
                 (interactive) (dired-sort-other (concat dired-listing-switches "t"))))
  (define-key dired-sort-map "n"
              '(lambda () "sort by Name"
                 (interactive) (dired-sort-other (concat dired-listing-switches "")))))
)
(defun sof/dired-sort ()
  "Dired sort hook to list directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (and (featurep 'xemacs)
       (fboundp 'dired-insert-set-properties)
       (dired-insert-set-properties (point-min) (point-max)))
  (set-buffer-modified-p nil))
(add-hook 'dired-after-readin-hook 'sof/dired-sort)
