;;-------------------------------------------------------------------;;
;;  Copyright (c) 2002 Kyle Wilson. All Rights Reserved.
;;  Time-stamp:    <2002-05-21 11:34:14 jameskw>
;;  Part-of:       Emacs Lisp Customization
;;  File Name:     "ELisp/Local-File-Fixup.el"
;;  Date(s):       Tue May 21 11:34:14 2002
;;  Author(s):     Kyle Wilson
;;-------------------------------------------------------------------;;
;;
;;-------------------------------------------------------------------;;
;; Revision Information:
;;   (jkw) [Tue May 21 11:34:14 2002] Initial Entry
;;-------------------------------------------------------------------;;

;;===================================================================;;
;; (jkw) [Tue May 21 11:34:52 2002]
;;===================================================================;;
;; Basic top-level clean-up entry points.
;;===================================================================;;

(defun jkw-fix ()
  "Fix a range of JKW system code issues
Knows the difference between various file types (currently by extension)
Tries to be relatively non-intrusive...changes made by this function should
be low-risk"

  (interactive)

  (let ((fileext (downcase (file-name-extension buffer-file-name))))
    ;;(message (format "Looking at: \"%s\"" fileext))

    (cond

     ;; C/C++ Source Files
     ((or (equal fileext "cpp") (equal fileext "h") (equal fileext "c"))
      (message "Clean C++ Source File")

      (untabify (point-min) (point-max))

      (jkw-just-one-blank-line)
      (jkw-trim-trailing-white)
      (jkw-tmacro-fix)
      (jkw-hresult-initialize)
      (jkw-clean-includes)
      (jkw-fix-comment-breaks)
      (jkw-fix-ifdefs)
      (jkw-update-header-wrapper)
      (jkw-update-file-header-block)
      (jkw-include-fix-pathseps)
      (jkw-remove-repeated-comment-breaks))

     ((or (equal fileext "el"))
      (jkw-just-one-blank-line)
      (jkw-trim-trailing-white))

     ;; MsDev Project Files
     ((or (equal fileext "dsp"))
      (jkw-dsp-remove-platform-define))

     ;; IDL Files
     ((or (equal fileext "idl"))
      (jkw-just-one-blank-line)
      (jkw-trim-trailing-white))

     ((or (equal fileext "rgs"))
      (jkw-fix-rgs-file))

     ((or (equal fileext "rc"))
      (message "Can't process resource files yet"))

     ;; Unrecognized files
     (t (message (format "Unknown type: \"%s\"" fileext)))
     )
    )
  )

(defun jkw-code-modify ()
  "Makes a broader range of modifications to existing code
This function may make more risky or specialized changes to
the code."
  (interactive)

  ;; Start out doing less-intrusive cleaning...
  (jkw-fix)

  (let ((fileext (downcase (file-name-extension buffer-file-name))))
    ;;(message (format "Looking at: \"%s\"" fileext))

    (cond

     ;; C/C++ Source Files
     ((or (equal fileext "cpp") (equal fileext "h") (equal fileext "c"))
      (message "Clean C++ Source File")
      ;;(jkw-fix-includes)
      (indent-region (point-min) (point-max) nil))

     ;; MsDev Project Files
     ((or (equal fileext "dsp"))
      (message "Can't process DSP files yet")
      )

     ;; Unrecognized files...
     (t (message (format "Unknown type: \"%s\"" fileext)))
     )
    )
  )

;;===================================================================;;
;; (jkw) [Tue May 21 11:35:28 2002]
;;===================================================================;;
;; Cleanup functions invoked by the non-intrusive fix front-end
;;===================================================================;;

(defun jkw-hresult-initialize ()
  "Make sure that all HRESULTs are reasonably initialized.
Should only modify the buffer if a change is needed."
  (interactive)

  (save-excursion
    (goto-char 0)
    (while (search-forward "HRESULT" nil t)
      (if (or (looking-at "[ \t]+hr[ \t]*;")
              (looking-at "[ \t]+hr[ \t]*=[ \t]*S_OK;"))
          (replace-match " hr(S_OK);")
        )
      )
    )
  )

(defun jkw-just-one-blank-line ()
  "Eliminate extra blank lines.
Should not change the buffer unless there are at least two adjacent blank
lines."
  (interactive)

  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t]*\n[ \t]*\n\\([ \t]*\n\\)+" nil t)
      (replace-match "\n\n")
      )
    )
  )

(defun jkw-trim-trailing-white ()
  "Trim trailing white-space on lines.
Should not modify the buffer unless there is extra trailing space."
  (interactive)

  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
      (replace-match "")
      )
    )
  )

(defun jkw-clean-includes ()
  "Look at include statements and try to fix them.
Performs simple changes that are generally always OK."
  (interactive)

  (let ((case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#[ \t]*include[ \t]+" nil t)
        (cond ((looking-at "[\"<]stdafx\.h[\">]")
               (replace-match "\"StdAfx.h\""))
              )
        )
      )
    )
  )

(defun jkw-fix-ifdefs ()
  "Convert #ifdef and #ifndef to #if ... constructs (less error prone)"
  (interactive)

  (let ((ifdef-pattern
         "^[ \t]*#[ \t]*if\\(n?\\)def[ \t]+\\([A-Za-z_][A-Za-z0-9_]*\\)"))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward ifdef-pattern nil t)
        (if (equal (match-string 1) "n")
            (replace-match "#if !defined(\\2)")
          (replace-match "#if defined(\\2)")
          )
        )
      )
    )
  )

(defun jkw-fix-comment-breaks ()
  "Fix long-line comments that are used for textual breaks.
Comments that will be changed include '//-----... and //////... type
comments"
  (interactive)
  (save-excursion
    (let ((next-line-add-newlines nil)
          (dash-string (make-string 67 ?-))
          (equal-string (make-string 67 ?=))
          (min-dashes (make-string 6 ?-)))
      (goto-char (point-min))
      (while (not (eobp))
        (forward-line 0)
        (cond
         ;; Don't need to do anything if the match is exact...
         ((looking-at (concat "^[ \t]*//" dash-string "//$"))
          )

         ((looking-at "^\\([ \t]*\\)//////+[ \t]*$")
          (replace-match (concat "\\1//" dash-string "//")))

         ((looking-at "^\\([ \t]*\\)//[ \t]*\\*\\*\\*\\*\\*\\*+[ \t]*$")
          (replace-match (concat "\\1//" dash-string "//")))

         ((looking-at "^\\([ \t]*\\)//[ \t]*======+[ \t]*$")
          (replace-match (concat "\\1//" equal-string "//")))

         ((looking-at (concat "^\\([ \t]*\\)//[ \t]*" min-dashes
                              "+\\(//\\)?[ \t]*$"))
          (replace-match (concat "\\1//" dash-string "//")))

         ((looking-at "^\\([ \t]*\\)/\\*\\*\\*\\*\\*\\*+[ \t]*$")
          (replace-match (concat (match-string 1) "/*" (make-string 67 ?*))))

         ((looking-at "^\\([ \t]*\\)\\*\\*\\*\\*\\*\\*+/[ \t]*$")
          (replace-match (concat (match-string 1) (make-string 67 ?*) "*/")))
         )
        (forward-line 1)
        )
      )
    )
  )

;;===================================================================;;
;; (jkw) [Tue May 21 11:42:59 2002]
;;===================================================================;;
;; More intrusive updater functions.
;;===================================================================;;

(defun jkw-strip-proxystub ()
  "Find and remove unwanted merged proxy/stub code"
  (interactive)

  (save-excursion
    (while
        (re-search-forward "^[ \t]*#[ \t]*ifdef[ \t]*_MERGE_PROXYSTUB.*$" nil t)

      (let ((start (match-beginning 0)))
        (if (re-search-forward "^[:space:]*#[:space:]*endif.*$" nil t)
            (progn
              (delete-region start (match-end 0))
              )
          )
        )
      )
    )
  )

(defun jkw-dsp-remove-platform-define ()
  "Fix issues in DSP files"
  (interactive)

  (save-excursion
    (goto-char 0)

    ;; Remove platform seelction stuff...
    (while (re-search-forward "/D _WIN32_WINNT=0x[0-9]+" nil t)
      (replace-match "")
      )
    )
  )

(defun jkw-fix-includes ()
  "Look at include statements and try to fix them"
  (interactive)

  (save-excursion
    (let ((case-fold-search t))
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#[ \t]*include[ \t]+" nil t)
        (cond ((jkw-fix-sheader "signaltrace" "Utility/SignalTrace"))
              )
        )
      )
    )
  )

(defun jkw-fix-sheader (a-name a-newname)
  "Convert a header with the given name (and any path) to the provided replacement name"

  (let ((case-fold-search t)
        (source-name-long (concat "[\"<].*[/\\\\]" a-name ".h[>\"]"))
        (source-name-short (concat "[\"<]" a-name ".h[>\"]")))
    (cond
     ((looking-at source-name-long)
      (replace-match (concat "<" a-newname ".h>")) t)

     ((looking-at source-name-short)
      (replace-match (concat "<" a-newname ".h>")) t)

     (t nil)
     )
    )
  )

(defun jkw-fix-aheader (a-name a-newpath)
  "Convert a header with the given name (and any path) to the provided replacement name"

  (let ((case-fold-search t)
        (source-name-long (concat "[\"<].*[/\\\\]" a-name ".[hH][>\"]"))
        (source-name-short (concat "[\"<]" a-name ".[Hh][>\"]")))
    (cond
     ((looking-at source-name-long)
      (replace-match (concat "<" a-newpath "/" a-name ".h>")) t)

     ((looking-at source-name-short)
      (replace-match (concat "<" a-newpath "/" a-name ".h>")) t)

     (t nil)
     )
    )
  )

(defun jkw-remove-repeated-comment-breaks ()
  "Remove repeated break comments //----// //----//"
  (interactive)
  
  (save-excursion
    (goto-char (point-min))
    (let ((comment-string 
           (concat "^[ \t]*//" (make-string 67 ?-) "//[ \t]*$")))
      (while (re-search-forward comment-string nil t)
        (save-excursion
          (let ((start-point (point)) (end-point nil))
            (forward-line 1)
            (while (looking-at comment-string)
              (forward-line 1)
              (setq end-point (point)))

            (if end-point
                (progn
                  (delete-region start-point (1- end-point))
                  (jkw-one-blank-line-after)))
            )))
      ))
  )

(defun jkw-one-blank-line-after ()
  "Make sure that the current line is followed by exactly one blank line"
  
  (save-excursion
    (forward-line 1)

    (if (not (looking-at "^[ \t]*$"))
        ;; If the following line isn't blank then insert a blank line.
        (insert "\n")

      ;; If the following line is blank then we need to make sure that there 
      ;; is only one. 
      (delete-blank-lines))
    )
  )

(defun jkw-one-blank-line-before ()
  "Make sure that the current line is followed by exactly one blank line"
  
  (save-excursion
    (forward-line -1)

    (if (not (looking-at "^[ \t]*$"))
        ;; If the following line isn't blank then insert a blank line.
        (progn
          (end-of-line)
          (insert "\n"))

      ;; If the following line is blank then we need to make sure that there 
      ;; is only one. 
      (delete-blank-lines))
    )
  )

(defun jkw-include-fix-pathseps ()
  "Convert all path separators in #include statments to '/'"
  (interactive)

  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*#[ \t]*include[ \t]+" nil t)
      (if (looking-at "[<\"]\\([^<>\"]+\\)[>\"]") 
          (progn
            (save-restriction
              (narrow-to-region (match-beginning 1) (match-end 1))
              (while (search-forward "\\" nil t)
                (replace-match "/"))
              )))
      )
    ))

(defun jkw-fix-rgs-file ()
  "Try to improve an RGS file"
  (interactive)
  (save-excursion
    (goto-char 0)
    (if (looking-at "^[ \t]*HKCR[ \t]*$")
        (let* ((infostring "[ \t]*=[ \t]*s[ \t]*'\\(.*\\)'[ \t]*")
               (itemstring "\\([a-zA-Z][a-zA-Z0-9]*\\)")
               (numstring "\\([0-9]+\\)")

               ;; 1) ProgIdVendor 2) ProgIdComp 3) Version 4) InfoString
               (gen-progid (concat "^[ \t]*"
                                   itemstring "\\." itemstring "\\." 
                                   numstring infostring "$"))

               (genvi-progid (concat "^[ \t]*"
                                     itemstring "\\." itemstring 
                                     infostring "[ \t]*$"))

               (progid-name) (progid-version) (progid-desc))

          (message "Looks like a good rgs file")
          (forward-line 1)
          (if (looking-at "^[ \t]*{[ \t]*$")
              (progn 
                (message "\tFound open brace")
                (forward-line 1)
                (cond

                 ;; If this looks like a general ProgId then convert it.
                 ((looking-at gen-progid)
                  (setq progid-name (concat (match-string 1) (match-string 2)))
                  (setq progid-version (match-string 3))
                  (setq progid-desc (match-string 4))
		  (replace-match (format "\tGroove.%s.%s = s '%s'"
					 progid-name
					 progid-version 
					 progid-desc))
                  )

                 (t (message "No Match"))
                 )

		(if progid-name
		    (progn

		      (save-excursion
			(goto-char 0)
			(while (re-search-forward 
				(concat progid-name "\\(\\." progid-version
					"\\)?"
				 "[ \t]+=[ \t]+s[ \t]+'\\(.*\\)'[ \t]*$")
				nil t)
			  (replace-match progid-desc t t nil 2)
			  )
			)
		      ))
                ))
          ))
    )
  )