;; default-directory
;; "d:/Local/Home/ELisp/tools/"

(defun devpath ()
  (interactive)

  (message (concat "in " default-directory))
  (query-replace-regexp "\\$(DEVPATH)" "../../Root")
  (query-replace-regexp "\\$(PSGDEV)" "../../PsgDev")
  )