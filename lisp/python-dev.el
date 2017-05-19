;;; Main pyton configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'python)
;; Debug
(defun python-add-breakpoint ()
  "Add a break point"
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent)
  (insert "import pdb; pdb.set_trace()")
  (newline-and-indent)

  (highlight-lines-matching-regexp "^[ ]*import pdb; pdb.set_trace()"))

(provide 'python-dev)
