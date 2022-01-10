(defun liomacs/tangle()
  (with-current-buffer (find-file-noselect "/tmp/git-test/someorg.org")
    (org-babel-tangle)))


(liomacs/tangle)
