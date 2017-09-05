;;-------------------------------------------------------------------;;
;; (jkw) [Tue Dec 16 10:51:10 2003]
;;-------------------------------------------------------------------;;
;;
;;-------------------------------------------------------------------;;
;; Arguments:
;;     None.
;;-------------------------------------------------------------------;;

(defun strip-comments () "Strip all '//' comments from this buffer"
  (interactive)
  (save-excursion
    (goto-char (point-min))

                                      ; Look for full-line comments...
    (let ((count 1))
      (while (re-search-forward "^[ \t]*//.*$" (point-max) t)
        (replace-match "")
        (setq count (1+ count))
        )

      (message "Found %d matches" count)
      )
    )
  )

(defun strip-and-clean () ""
  (interactive)
  (strip-comments)
  (jkw-code-modify)
  )

(define-key global-map [kp-1] 'strip-and-clean)

;;===================================================================;;
;; (jkw) [Tue Dec 16 10:51:15 2003]
;;===================================================================;;
;;
;;===================================================================;;

(defun qwww () "Search ??? related directories only"
  (interactive)
  (save-excursion
    (let
        ((sstr (concat
                "grep -n -e "
                (buffer-substring (point) (mark))
                " "
                "I:/groove2_5maint/main/GROOVE/*/*.cpp "
                "I:/groove2_5maint/main/GROOVE/include/*.h "
                )))
      (grep sstr)
      )
    )
  )

(defun qwww-ctl () "Broader search"
  (interactive)
  (save-excursion
    (let
        ((sstr (concat
                "grep -n -e "
                (buffer-substring (point) (mark))
                " "
                "I:/groove2_5maint/main/GROOVE/*/*.cpp "
                "I:/groove2_5maint/main/GROOVE/include/*.h "
                "\"C:/Program Files/Microsoft Visual Studio .NET 2003/Vc7/include/*\" "
                "\"C:/Program Files/Microsoft Visual Studio .NET 2003/Vc7/include/*/*\" "
                "\"C:/Program Files/Microsoft Visual Studio .NET 2003/Vc7/ATL/include/*\" "
                "\"C:/Program Files/Microsoft Visual Studio .NET 2003/Vc7/ATL/include/*/*\" "
                )))
      (grep sstr)
      )
    )
  )

(defun qzzz () ""
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))

    (goto-char (point-min))
    (replace-regexp " +$" "")

    (goto-char (point-min))
    (replace-regexp " +;" ";")

    (goto-char (point-min))
    (replace-regexp " +)" ")")

    (goto-char (point-min))
    (replace-regexp "( +" "(")

    (goto-char (point-min))
    (replace-regexp "\\(if\\|switch\\)(" "\\1 (")

                                        ;(goto-char (point-min))
                           ;(replace-regexp "} *else *{" "}\nelse\n{")

    (goto-char (point-min))
    (replace-regexp "^#ifdef[ \t]+\\([A-Za-z_][A-Za-z0-9_]*\\)"
                    "#if defined(\\1)")

    (goto-char (point-min))
    (replace-regexp "^#ifndef[ \t]+\\([A-Za-z_][A-Za-z0-9_]*\\)"
                    "#if !defined(\\1)")

    (goto-char (point-min))
    (replace-regexp "\n\n+" "\n\n")

    (indent-region (point-min) (point-max) nil)
    )
  )

(defun spx () ""
  (interactive)
  ;;  (let ((itbegins (min (point) (mark)))
  ;;        (itends (max (point) (mark))))
  (let ((itbegins (point))
        (itends (re-search-forward "^[ \t]*{" (+ (point) 5000) nil)))
    (message "%S %S" itbegins itends)
    (save-excursion
      (save-restriction
        (narrow-to-region itbegins itends)
        (message "%S" (goto-char (point-min)))
        (goto-char itbegins)
        (while (re-search-forward "  +" nil t)
          (replace-match " " nil nil))
        )
      (indent-region itbegins itends nil)
      )
    )
  )

(defun ttti () ""
  (interactive)
  (re-search-forward "^\\(.*[^ \t\n].*\\){\\(.*\\)$")
  )

(defun rrsp () ""
  (interactive)
  (save-excursion
    (query-replace-regexp "[ \t][ \t]+" " " nil)
    )
  )

(defun hrclean ()
  "Try to fix HRESULT initialization issues"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (query-replace "HRESULT hr;" "HRESULT hr(S_OK);")
    (goto-char (point-min))
    (query-replace-regexp "HRESULT hr[ \t]*=[ \t]*S_OK;" "HRESULT hr(S_OK);")
    )
  )

(defun inclean ()
  "Clean header file naming"
  (interactive)
  (save-excursion
    (goto-char (point-min))

    )
  )

(defun varclean ()
  ""
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (query-replace "VARIANT" "CVariant")
    )
  )

(defun bfn () "Insert file name in the appropriate place..."
  (interactive)
  (save-excursion
    (goto-char 0)
    (re-search-forward "^//[ \t]+File[ \t]*[Nn]ame:[ \t]+\"")
    (kill-line)
    (insert buffer-file-name "\"")
    )
  )

;;-------------------------------------------------------------------;;
;; (jkw) [Thu Mar 18 16:07:55 2004]
;;-------------------------------------------------------------------;;
;; Set 2 space tabs.  Usually used to make XML files more readable.
;;-------------------------------------------------------------------;;

(defun tab2-set-tab-width-to-2 ()
  "Set the tab width in the current buffer to 2"
  (interactive)
  (setq tab-width 2)
  (let (is-read-only (buffer-read-only))
    (if is-read-only (toggle-read-only -1))
    (save-excursion
      ;;(goto-char (point-min))
      (replace-string "\r" "" nil (point-min) (point-max))
      )
    (set-buffer-file-coding-system 'undecided-dos)
    (if is-read-only (toggle-read-only 1))
    (set-buffer-modified-p nil)
    )
  )
(define-key global-map [f3] 'tab2-set-tab-width-to-2)

