;;; Main pyton configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'python)

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt --pprint")

;; Debug

(defun python-add-breakpoint ()
  "Add a break point"
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent)
  (insert "import ipdb; ipdb.set_trace()")
  (newline-and-indent)

  (highlight-lines-matching-regexp "^[ ]*import i?pdb; i?pdb.set_trace()"))


(provide 'python-dev)
