;;-------------------------------------------------------------------;;
;;  Copyright (c) 2002 Kyle Wilson. All Rights Reserved.
;;  Time-stamp:    <2002-05-13 11:00:54 jameskw>
;;  Part-of:       Emacs Initialization Files
;;  File Name:     "Home/ELisp/Local-Defuns.el"
;;  Date(s):       Monday May 13, 2002
;;  Author(s):     Kyle Wilson
;;-------------------------------------------------------------------;;
;; 
;;-------------------------------------------------------------------;;
;; Revision Information:
;;   (jkw) [Monday May 13, 2002] Initial Entry
;;-------------------------------------------------------------------;;

(defun coding-system-forced-unix () 
  "Set the coding system for this buffer unconditionally to Unix style
This should result in saved lines ending in '\\n' only" 
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix))

(defun coding-system-forced-windows () 
  "Set the coding system for this buffer unconditionally to windows style
This should result in saved lines ending in '\\r\\n'" 
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos))

(defun source-search-local () 
  "Search all local source files for the marked text"
  (interactive)
  (save-excursion
    (let
        ((sstr (concat 
                "grep -n -e " 
                (buffer-substring (point) (mark)) 
                " " 
                "*.c *.h *.cpp *.C *.H *.CPP"
                )))
      (grep sstr)
      )
    )
  )

(defun source-search-local-extended () 
  "Search all local source files for the marked text"
  (interactive)
  (save-excursion
    (let
        ((sstr (concat 
                "grep -n -e " 
                (buffer-substring (point) (mark)) 
                " " 
                "*.c *.h *.cpp *.C *.H *.CPP"
                " "
                "../../*/src/*.c ../../*/src/*.h ../../src/*.cpp"
                " "
                "../../*/src/*.C ../../*/src/*.H ../../src/*.CPP"
                " "
                "../../include/*.h ../../include/*.H"
                )))
      (grep sstr)
      )
    )
  )

(defun reindent-buffer-preserve-point () 
  "Reindent the entire buffer preserving the current setting of point"
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))
    )
  )

(defun clean-trailing () 
  "Conditionally remove spaces from various points interactively"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (query-replace-regexp "[ \t][ \t]+\\*/[ \t]*$" " */" nil)

    (goto-char (point-min))
    (query-replace-regexp ",[ \t][ \t]+/\\*" ", /*" nil)
    )
  )


(defun unset-mark () "" (interactive) (set-mark nil))

(defun make-backup-file-name (file)
  "Create the non-numeric backup file name for FILE.
This is a separate function so you can redefine it for customization."
  (if (and (eq system-type 'ms-dos)
           (not (msdos-long-file-names)))
      (let ((fn (file-name-nondirectory file)))
        (concat (file-name-directory file)
                (or
                 (and (string-match "\\`[^.]+\\'" fn)
                      (concat (match-string 0 fn) ".~"))
                 (and (string-match "\\`[^.]+\\.\\(..?\\)?" fn)
                      (concat (match-string 0 fn) "~")))))
    (concat file ".bak")))


;;----------------------------------------------------------------------;;
;; Sanitize a block of code...
;;----------------------------------------------------------------------;;

(defun sanitize-this-code () ""
  (interactive)
  (save-excursion
    (undo-boundary)

    (goto-char (point-min))   
    (message "Remove all comments")
    (while (re-search-forward "^[ \t]*//.*$" nil t)
      (replace-match "" nil nil))

    (goto-char (point-min))
    (message "Collapse all multiple spaces into a single space")
    (while (re-search-forward "[ \t]+" nil t)
      (replace-match " " nil nil))

    (goto-char (point-min))
    (message "Trim out all leading spaces")
    (while (re-search-forward "^[ \t]+" nil t)
      (replace-match "" nil nil))

    (goto-char (point-min))
    (message "Trim out all trailing spaces")
    (while (re-search-forward "[ \t]+$" nil t)
      (replace-match "" nil nil))

    (goto-char (point-min))
    (message "Eliminate all multiple newlines")
    (while (re-search-forward "\n+" nil t)
      (replace-match "\n" nil nil))
    )
  )

(defun local-set-frame-settings (new-frame)
  "Sets up local frame settings to approximately match those on the default frame"
  (let ((current-frame (selected-frame)))
    (select-frame new-frame)
    (setup-my-preferred-font)
    (setup-all-of-my-display-defaults)
    (select-frame current-frame)
    ))

;;===================================================================;;
;; (jkw) [Tue May 21 09:30:05 2002]
;;===================================================================;;
;; New Defuns Built to support ongoing work on the system.
;;===================================================================;;

(defun jkw-groove-pathname (a-filepath)
  "Given a file-path string return the groove path string
This looks for the last 'groove' name in the path and removes everything
before that."

  (save-excursion
    (let ((working-string a-filepath)
          (last-match 0)
          (first-match 0)
          (case-fold-search t))
      ;;(message (format "Looking at \"%s\"" working-string))

      (while (or (string-match "/groove/" working-string last-match)
		 (string-match "/sandbox/" working-string last-match)
		 (string-match "/stigl/" working-string last-match)
                 (string-match "/OrthOne/" working-string last-match)
                 (string-match "/ONI/" working-string last-match)
                 (string-match "/Projects/" working-string last-match)
                 )
        (setq last-match (1- (match-end 0)))
        (setq first-match (match-beginning 0))
        )

      ;;(message (format "Done at %d with \"%s\"" first-match
      ;;                (substring working-string (1+ first-match))))

    ;; If at least one match was found then return the trimmed string.
      ;; If no matches were found then return the literal string.
      (if (equal last-match 0)
          working-string
        (substring working-string (1+ first-match))
        )
      )
    )
)

(defun jkw-filename-to-identifier (a-filename)
  "Given a file name return a string that is usable as an identifier
This largely replaces non-alphanumeric characters with '_' characters"

  (let ((working "") (index 0))
    (while (not (equal (length a-filename) (length working)))
      (let ((this-char (elt a-filename index)))
        (if (or
             (and (<= ?A this-char) (>= ?Z this-char))
             (and (<= ?a this-char) (>= ?z this-char))
             (and (<= ?0 this-char) (>= ?9 this-char)))
            (setq working (concat working (string this-char)))
          (setq working (concat working "_"))
          )
        )

      (setq index (1+ index))
      )

    working
    )
  )

(defun jkw-user-fullname ()
  "Return the full name for the current user"

  (if (boundp 'jkw-override-fullname)
      jkw-override-fullname
    (let ((envfullname (getenv "USER_FULLNAME")))
      (if (eq envfullname nil)
          "Unknown"
        envfullname
        )
      )
    )
  )

(defun jkw-user-initials ()
  "Return the initials for the current user"

  (if (boundp 'jkw-override-initials)
      jkw-override-initials
    (let ((envinitials (getenv "USER_INITIALS")))
      (if (eq envinitials nil)
          "???"
        envinitials
        )
      )
    )
  )

(defun jkw-copyright-string ()
  "Return the currently selected copyright string"
  (interactive)

  (if (boundp 'jkw-override-copyright-string)
      jkw-override-copyright-string
    (if (boundp 'local-copyright-notice)
        local-copyright-notice
      "Unknown")
    )
  )

(defun jkw-project-string ()
  "Return the currently selected project string"
  (interactive)

  (if (boundp 'jkw-override-project-string)
      jkw-override-project-string
    (if (boundp 'local-project-string)
        local-project-string
      "Experimental Sample Code")
    )
  
  )

(defun jkw-check-match-lines (a-pattern-list)
  "Given a list of regex patterns return nil if a mismatch occurs or point at end of match"

  (let ((matched t))
    (save-excursion

      ;; Run through each pattern and fail if any pattern fails to match.
      (while (and a-pattern-list matched)
        ;;(message (format "Checking \"%s\" %s" (car a-pattern-list)
	;;(prin1 matched)))
        (if (not (looking-at (car a-pattern-list)))
            (setq matched nil))
        ;;(message (format "Done \"%s\" %s" (car a-pattern-list)
	;;;(prin1 matched)))
        (forward-line 1)
        (setq a-pattern-list (cdr a-pattern-list))
        )

      ;; If we matched then return the final setting on point.
      (if matched (point) nil)
      )
    )
  )

(defun jkw-check-match-lines-contin (a-continue-pattern a-pattern-list)
  "Given a list of regex patterns return nil if a mismatch occurs or point at end of match"

  (let ((matched t)
        (match-list nil))
    (save-excursion

      ;; Run through each pattern and fail if any pattern fails to match.
      (while (and a-pattern-list matched)
        (cond 

         ;; Exact match...continue on to next pattern...
         ((looking-at (car a-pattern-list))
          (setq a-pattern-list (cdr a-pattern-list))
          (push (point) match-list))

         ;; Continuation line...
         ((looking-at a-continue-pattern))

         ;; Default...didn't match either of the required strings...
         (t
          (setq matched nil))
         )
        ;; Next line needs to be examinied either way here...
        (forward-line 1)
        )

      (if matched (push (point) match-list))

      ;; If we matched then return the final setting on point.
      (if matched match-list nil)
      )
    )
  )

(defun jkw-forced-revert ()
  "Do a no-questions-asked revert"
  (interactive)
  (revert-buffer t t))

(defun jkw-get-user-fullname ()
  "Retrieve the full name of the current user.
This is designed to cope gracefully with missing environment variables.
Given the necessary logic, I'd like it to be able to interrogate the registry as well."
  (let ((envfullname (getenv "user_fullname")))
    (if envfullname
        envfullname
      "Unknown User")
    ))
 

;; It would be nice if this looked at the insertion context and made
;; 'smart' decisions about how to inject the string...currently I'm
;; going to settle for the simple version.

(defun bug-date-time ()
  "Insert a date/time field that is compatible with the Groove bugs database format"
  (interactive)
  (let ((mystring (format-time-string "%m/%d/%Y %I:%M %p" (current-time))))
    (insert (format "-- %s %s --\n\n" mystring (jkw-get-user-fullname)))
    ))

(defun fix-pulse ()
  "Fix an ONI pulse sequence to work correctly"
  (interactive)
  
  (save-excursion

    (replace-regexp "m_repDur" "repDur()" nil (point-min) (point-max))
    (replace-regexp "m_echo_train" "echo_train()" nil (point-min) (point-max))
    (replace-regexp "m_numEchos" "numEchos()" nil (point-min) (point-max))
    (replace-regexp "m_bSaveRawData" "SaveRawData()" nil (point-min) (point-max))
    (replace-regexp "m_size3rdD" "size3rdD()" nil (point-min) (point-max))
    (replace-regexp "m_actualSize2ndD" "actualSize2ndD()" nil (point-min) (point-max))
    (replace-regexp "m_actualSize1stD" "actualSize1stD()" nil (point-min) (point-max))
    (replace-regexp "m_numAves" "numAves()" nil (point-min) (point-max))
    (replace-regexp "m_size1stD" "size1stD()" nil (point-min) (point-max))
    (replace-regexp "m_imgFreq" "imgFreq()" nil (point-min) (point-max))
    (replace-regexp "m_numSlices" "numSlices()" nil (point-min) (point-max))
    (replace-regexp "m_format" "format()" nil (point-min) (point-max))
    (replace-regexp "m_imageScale" "imageScale()" nil (point-min) (point-max))
    (replace-regexp "m_size2ndD" "size2ndD()" nil (point-min) (point-max))
    (replace-regexp "m_imgPhase" "imgPhase()" nil (point-min) (point-max))
    (replace-regexp "m_nPhaseOverSample" "PhaseOverSample()" nil (point-min) (point-max))
    (replace-regexp "m_bNoPhaseWrap" "NoPhaseWrap()" nil (point-min) (point-max))
    (replace-regexp "m_thickness" "thickness()" nil (point-min) (point-max))
    (replace-regexp "m_gap" "gap()" nil (point-min) (point-max))
    (replace-regexp "m_offsetReadout" "offsetReadout()" nil (point-min) (point-max))
    (replace-regexp "m_offsetPhase" "offsetPhase()" nil (point-min) (point-max))
    (replace-regexp "m_offsetSlice" "offsetSlice()" nil (point-min) (point-max))
    (replace-regexp "m_fovFreq" "fovFreq()" nil (point-min) (point-max))
    (replace-regexp "m_fovRatio" "fovRatio()" nil (point-min) (point-max))
    (replace-regexp "m_recvBW" "recvBW()" nil (point-min) (point-max))
    (replace-regexp "m_numFrames" "numFrames()" nil (point-min) (point-max))
    (replace-regexp "m_bPart1stD" "Part1stD()" nil (point-min) (point-max))
    (replace-regexp "m_bPart2ndD" "Part2ndD()" nil (point-min) (point-max))
    (replace-regexp "m_num_shots" "num_shots()" nil (point-min) (point-max))
    (replace-regexp "m_numDimensions" "numDimensions()" nil (point-min) (point-max))
    (replace-regexp "m_echo_spacing" "echo_spacing()" nil (point-min) (point-max))
    (replace-regexp "m_bSaveImages" "SaveImages()" nil (point-min) (point-max))
    (replace-regexp "m_dimFreq" "dimFreq()" nil (point-min) (point-max))
    (replace-regexp "m_dimPhase" "dimPhase()" nil (point-min) (point-max))
    (replace-regexp "m_nTE" "TE()" nil (point-min) (point-max))
    (replace-regexp "m_fRfTripPw" "RfTripPw()" nil (point-min) (point-max))
    (replace-regexp "m_bPreScan" "PreScan()" nil (point-min) (point-max))
    (replace-regexp "m_numLoops" "numLoops()" nil (point-min) (point-max))
    (replace-regexp "m_nLoopDur" "LoopDur()" nil (point-min) (point-max))
    (replace-regexp "m_nDecRatio" "DecRatio()" nil (point-min) (point-max))
    (replace-regexp "m_bFID" "FID()" nil (point-min) (point-max))
    ;(replace-regexp "" "" nil (point-min) (point-max))
    ;(replace-regexp "" "" nil (point-min) (point-max))
    ;(replace-regexp "" "" nil (point-min) (point-max))
    ;(replace-regexp "" "" nil (point-min) (point-max))
    ;(replace-regexp "" "" nil (point-min) (point-max))
    ;(replace-regexp "" "" nil (point-min) (point-max))
    ;(replace-regexp "" "" nil (point-min) (point-max))
    ;(replace-regexp "" "" nil (point-min) (point-max))
    ;(replace-regexp "" "" nil (point-min) (point-max))
    ;(replace-regexp "" "" nil (point-min) (point-max))
    ;(replace-regexp "" "" nil (point-min) (point-max))
    ;(replace-regexp "" "" nil (point-min) (point-max))
    ;(replace-regexp "" "" nil (point-min) (point-max))

    )

  ) 