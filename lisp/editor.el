;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main editor configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq mac-command-modifier 'control)

;; Confirm ruin your precious uptime
(setq confirm-kill-emacs 'yes-or-no-p)

;; Override regions when writing text
(delete-selection-mode)

;; No backup-files
(setq make-backup-files nil)

;; Re-read file if changed externally
(global-auto-revert-mode t)
(setq auto-save-default -1)
(setq show-paren-style 'mixed)
(setq diredp-display-images t)
(defalias 'yes-or-no-p 'y-or-n-p)


;; Increase/Decrease font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
;; http://www.masteringemacs.org/articles/2012/09/10/hiding-replacing-modeline-strings/
(global-set-key (kbd "C-c f") 'toggle-frame-fullscreen)

;; Move between opened buffers
(global-set-key [C-tab] 'next-buffer)
(global-set-key [C-S-iso-lefttab] 'previous-buffer)
(set-buffer-file-coding-system 'unix t)


(defun set-mac-keyboard()
  (interactive)
  (setq mac-command-modifier 'control)
  )

(defun set-kinesis-keyboard()
  (interactive)
  (setq mac-command-modifier 'super)
  )

;; Comment or uncomment
(defun my-comment-or-uncomment-line (&optional arg)
  "Comment current line or, if at EOL, call `comment-dwim' with ARG."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p))
           (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position)
                                   (line-end-position))
    (comment-dwim arg)))

(global-set-key (kbd "C-;") 'my-comment-or-uncomment-line)
(global-set-key (kbd "C-M-;") 'comment-or-uncomment-region)


;; Whitespace
(set-default 'indent-tabs-mode nil)
(setq whitespace-style '(face trailing tabs))
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)


(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))


;; duplicate current line
(defun duplicate-current-line (&optional n)
  "duplicate current line, make more than 1 copy given a numeric argument"
  (interactive "p")
  (save-excursion
    (let ((nb (or n 1))
    	  (current-line (thing-at-point 'line)))
      ;; when on last line, insert a newline first
      (when (or (= 1 (forward-line 1)) (eq (point) (point-max)))
    	(insert "\n"))

      ;; now insert as many time as requested
      (while (> n 0)
    	(insert current-line)
    	(decf n)))))

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;; Search in other window @ikame
(defun isearch-forward-regexp-other-window ()
  (interactive)
  (save-selected-window
    (other-window 1)
    (isearch-forward-regexp)))

(defun isearch-backward-regexp-other-window ()
  (interactive)
  (save-selected-window
    (other-window 1)
    (isearch-backward-regexp)))


(defun move-region (start end n)
  "Move the current region up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun move-region-up (start end n)
  "Move the current line up by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) -1 (- n))))


(defun move-region-down (start end n)
  "Move the current line down by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) 1 n)))


(global-set-key (kbd "C-M-s") 'isearch-forward-regexp-other-window)
(global-set-key (kbd "C-M-r") 'isearch-backward-regexp-other-window)
(global-set-key (kbd "C-M-N") 'move-region-down)
(global-set-key (kbd "C-M-P") 'move-region-up)

(global-set-key (kbd "C-M-n") 'move-line-down)
(global-set-key (kbd "C-M-p") 'move-line-up)
(global-set-key (kbd "C-c l") 'copy-line)
(global-set-key (kbd "C-S-d") 'duplicate-current-line)
(global-set-key (kbd "M-o") 'other-window)

(provide 'editor)
