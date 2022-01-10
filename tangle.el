#!/usr/bin/emacs --script

(defun liomacs/tangle(file)
  (with-current-buffer (find-file-noselect file)
    (org-babel-tangle)))

(defun main ()
  (setq org-confirm-babel-evaluate nil)
  (liomacs/tangle (car command-line-args-left)))


(main)
