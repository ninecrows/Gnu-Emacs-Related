;;-------------------------------------------------------------------;;
;;  Copyright (c) 2002 Kyle Wilson. All Rights Reserved.
;;  Time-stamp:    <2002-05-13 11:18:40 jameskw>
;;  Part-of:       Emacs Initialization Files
;;  File Name:     "Home/ELisp/Local-C-Headers.el"
;;  Date(s):       Monday May 13, 2002
;;  Author(s):     Kyle Wilson
;;-------------------------------------------------------------------;;
;; Setup header prep stuff for quick comment block insertion.
;;-------------------------------------------------------------------;;
;; Revision Information:
;;   (jkw) [Monday May 13, 2002] Initial Entry
;;-------------------------------------------------------------------;;

(defun new-headers-date () "" 
  (interactive)
  (save-excursion
    (goto-char 0)
    (search-forward "[")
    (zap-to-char 1 ?])
  (insert (format-time-string "%A %B %d, %Y") "]")

  (search-forward "[")
  (zap-to-char 1 ?])
(insert (format-time-string "%A %B %d, %Y") "]")

(search-forward "[")
(zap-to-char 1 ?])
(insert (format-time-string "%A %B %d, %Y") "]")

(goto-char 0)
(re-search-forward "^//[ \t]+Date(s):[ \t]+")
(insert (format-time-string "%A %B %d, %Y"))
(kill-line)
))

(defun headers.txt () "Setup for header text insertions"
  (interactive)
  (save-excursion
    (setq get-file-from (concat default-directory "headers.txt"))
    (setq hdr-buffer (get-buffer-create "*Headers.txt*"))
    (set-buffer hdr-buffer)
    (insert-file-contents get-file-from nil nil nil t)

    ;; Update the date information
    (new-headers-date)

    ;; Update creator/user information
    (save-excursion
    (goto-char 0)
    (while (search-forward "(???)" nil t)
      (replace-match (concat "(" (getenv "user_initials") ")")nil t))
    )

    (save-excursion
    (goto-char 0)
    (if (search-forward "//  Author(s):" nil t)
        (let () (end-of-line) (insert (getenv "user_fullname")))
      ))

    ;; Copy the available sections to buffers
    (goto-char 0)
    (setq spot-1 (point))
    (re-search-forward "^[ \t]*$")
    (copy-to-register ?1 spot-1 (point) nil)
    (next-line 1)

    (setq spot-1 (point))
    (re-search-forward "^[ \t]*$")
    (copy-to-register ?2 spot-1 (point) nil)
    (next-line 1)

    (setq spot-1 (point))
    (re-search-forward "^[ \t]*$")
    (copy-to-register ?3 spot-1 (point) nil)
    (next-line 1)

    (setq spot-1 (point))
    (re-search-forward "^[ \t]*$")
    (copy-to-register ?4 spot-1 (point) nil)
    (next-line 1)

    (setq spot-1 (point))
    (re-search-forward "^[ \t]*$")
    (copy-to-register ?5 spot-1 (point) nil)
    (next-line 1)

    ;;(bury-buffer)
    )
  )

(defun get-default-headers.txt () 
  "Setup for header text insertions"
  (interactive)
  (save-excursion
    (setq get-file-from "~/headers.txt")
    (setq hdr-buffer (get-buffer-create "*Headers.txt*"))
    (set-buffer hdr-buffer)
    (insert-file-contents get-file-from nil nil nil t)

    ;; Update the date information
    (new-headers-date)

    ;; Update creator/user information
    (save-excursion
    (goto-char 0)
    (while (search-forward "(???)" nil t)
      (replace-match (concat "(" (jkw-user-initials) ")")nil t))
    )

    (save-excursion
    (goto-char 0)
    (if (search-forward "//  Author(s):" nil t)
        (let () (end-of-line) (insert (jkw-user-fullname)))
      ))

    ;; Copy the available sections to buffers
    (goto-char 0)
    (setq spot-1 (point))
    (re-search-forward "^[ \t]*$")
    (copy-to-register ?1 spot-1 (point) nil)
    (next-line 1)

    (setq spot-1 (point))
    (re-search-forward "^[ \t]*$")
    (copy-to-register ?2 spot-1 (point) nil)
    (next-line 1)

    (setq spot-1 (point))
    (re-search-forward "^[ \t]*$")
    (copy-to-register ?3 spot-1 (point) nil)
    (next-line 1)

    (setq spot-1 (point))
    (re-search-forward "^[ \t]*$")
    (copy-to-register ?4 spot-1 (point) nil)
    (next-line 1)

    (setq spot-1 (point))
    (re-search-forward "^[ \t]*$")
    (copy-to-register ?5 spot-1 (point) nil)
    (next-line 1)

    ;;(bury-buffer)
    )
  )

(defun hfile-fix () "Insert file information into the .h file framing."
  (interactive)
  (save-excursion
    (let (tag-var starts-at ends-at)
    (goto-char 0)
    (if (search-forward "#if !defined(HAVE)" nil t)
        (let (ttx)
          (backward-char 1)
          (setq ttx buffer-file-name)
          (setq ttx (upcase ttx))
          (setq starts-at (point))
          (insert (concat ttx "_"))
          (setq ends-at (- (point) 1))

          (goto-char starts-at)
          (while (search-forward "/" ends-at t)
            (replace-match "_" nil t))

          (goto-char starts-at)
          (while (search-forward "." ends-at t)
            (replace-match "_" nil t))

          (goto-char starts-at)
          (while (search-forward " " ends-at t)
            (replace-match "_" nil t))

          (goto-char starts-at)
          (while (re-search-forward "[a-zA-Z]:" ends-at t)
            (replace-match "" nil t))

          (setq tag-var (buffer-substring starts-at (- ends-at 1)))
          )
      )

    (end-of-line)
    (forward-char 1)
    (if (looking-at "#define HAVE")
        (let ()
          (search-forward "#define HAVE")
          (insert tag-var))
      (insert "No Match"))
    )
    )
  )

(defun hwrap-buffer () "Add all required wrapper to a new .h file"
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)

    (goto-char (point-min))

    (insert-register (string-to-char "1") t)
    (insert "\n")
    
    (insert-register (string-to-char "4") t)
    (insert "\n")

    (goto-char (point-max))
    (insert "\n")
    (insert-register (string-to-char "5"))

    (bfn)
    (hfile-fix)
    (time-stamp)
    )
  )
