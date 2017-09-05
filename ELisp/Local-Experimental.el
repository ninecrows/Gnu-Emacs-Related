;;-------------------------------------------------------------------;;
;;  Copyright (c) 2002 Kyle Wilson. All Rights Reserved.
;;  Time-stamp:    <2002-05-15 08:34:13 jameskw>
;;  Part-of:       Kyle Wilson's Emacs Initialization Files.
;;  File Name:     "ELisp/Local-Experimental.el"
;;  Date(s):       Tuesday May 14, 2002
;;  Author(s):     Kyle Wilson
;;-------------------------------------------------------------------;;
;;
;;-------------------------------------------------------------------;;
;; Revision Information:
;;   (jkw) [Tuesday May 14, 2002] Initial Entry
;;-------------------------------------------------------------------;;

(defun jkw-definition-macro-fix (a-name)
  "Find macros that need an extra ';' and add it if it isn't already there"
  (let ()
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward a-name nil t)
        (progn
          (beginning-of-line 2)
          (if (not (looking-at "[ \t]*;"))
              (progn
                (insert ";\n")
                )
            )
          )
        )
      )
    )
  )

(defun jkw-tmacro-fix ()
  "Find all interesting macros and try to fix them to make indent work better"
  (interactive)
  (let ((ilist '("END_OBJECT_MAP()"
                 "DECLARE_REGISTRY_RESOURCEID(IDR_[A-Z_]+)"
                 "DECLARE_PROTECT_FINAL_CONSTRUCT()"
                 "END_COM_MAP()"
                 "END_CATEGORY_MAP()"
                 "END_PROP_MAP()"
                 "END_MSG_MAP()"
                 "END_SINK_MAP()"
                 "DECLARE_VIEW_STATUS([0-9]+)"
                 "DECLARE_MESSAGE_MAP()"
                 "ILED_DEVICE_MAP([a-zA-Z0-9_]+)"
                 "IMPLEMENT_DYNCREATE([a-zA-Z0-9_, \t]+)"
                 "END_MESSAGE_MAP()"
                 "DECLARE_DYNCREATE([a-zA-Z0-9_]+)")))

    (while ilist
      (let ((thisone (pop ilist)))
        (jkw-definition-macro-fix thisone)
        )
      )
    )
  )

(defun jkw-insert-argument ()
  "Insert an argument name into the arguments field of the preceding comment"
  (interactive)
  (save-excursion
    (let ((end-var) (start-var) (varstring))
      (save-excursion
        (if (looking-at ",")
            (backward-char))
        (if (looking-at "[A-Za-z0-9_]+")
            (progn
              (message (format "Looks good: \"%s\"" (match-string 0)))
              (setq end-var (match-end 0))
              (if (re-search-backward "[^A-Za-z0-9_]" nil t)
                  (progn
                    (message (format "Back to \"%s\"" (match-string 0)))
                    (setq varstring (buffer-substring (1+ (point)) end-var))
                    (message (format "Found: \"%s\"" varstring))
                    (jkw-add-argument-comment varstring)
                    ))
              ))
        )))
  )

(defun jkw-add-argument-comment (a-argname)
  "Find the matching function description block and add this argument name to the end of it"
  
  (save-excursion
    (if (re-search-backward (concat "^[ \t]*//" (make-string 50 ?-) 
                                    "+//[ \t]*$"))
        (let ((end-area))
          (forward-line 0)
          (setq end-area (point))
          (if (re-search-backward "^[ \t]*//[ \t]+Arguments:[ \t]*$")
              (progn
                (forward-line 1)
                (if (looking-at "^[ \t]*//[ \t]+\\(\\.\\.\\.\\)[ \t]*$")
                   
                    ;; If this is the first argument then just replace it
                    (replace-match (concat a-argname " = ") t nil nil 1)
                    
                  ;; Second and subsequent arguments go at the end...
                  (goto-char end-area)
                  (insert "//     " a-argname " = \n")
                  )
                ))
          )))
  )

(defun jkw-fix-rgs-naming ()
  "Do what we can to fix *.rgs file naming"
  (interactive)

  ;; Try this on for size
  (let ((i-match-list (jkw-matches-sequence jkw-rgs-pattern)))

    ;; If the file matched (pattern of lines is reasonable for a stock
    ;; rgs file then proceed with processing
    (if i-match-list 
        (let ((i-names (jkw-rgs-get-information jkw-rgs-pattern i-match-list)))
          (message (format "Found patterned \"%s\" \"%s\" \"%s\" \"%s\"" 
                           (nth 0 i-names) (nth 1 i-names)
                           (nth 2 i-names) (nth 3 i-names)))

          ;; If we got valid names
          (if i-names
              ;; At this point everything looks pretty good...now we
              ;; need to examine individual features and see how they look.
              (save-excursion
                ;; Look at the ProgId we got.  If this looks like a
                ;; reasonable ProgId then just tag it for
                ;; replacement.  If it is too long or malformed then
                ;; ask about a fix...
                (if (not (jkw-rgs-mycompany-progid-p (nth 0 i-names)))
                    (let ((new-string (jkw-rgs-fix-progid (nth 2 i-names))))
                      (setq replacement (read-string 
                                         (format "New ProgID [%d]: " 
                                                 (length new-string))
                                         new-string))
                      (message (format "Replace \"%s\" with \"%s\"\n" 
                                       (nth 3 i-names) replacement))
                      (save-excursion
                        (let ((was (concat name1 "." name2)))
                          (goto-char 0)
                          (while (search-forward was nil t)
                            (replace-match replacement t t)
                            )
                          ))
                      )

                  (message (format "ProgId \"%s\" looks OK" (nth 0 i-names)))
                  ))

            (error "Cound not get ProgId and Description strings")
            ))
      (error "Failed to match stock *.rgs file pattern(s)")
      )
    ))

;; Version independent ProgId
(setq jkw-rgs-progid 
      "[_A-Za-z][_A-Za-z0-9]*\\.[_A-Za-z][_A-Za-z0-9]*")

;; Versioned ProgId
(setq jkw-rgs-vprogid 
      (concat jkw-rgs-progid "\\.[0-9]+"))

(setq jkw-rgs-hexnum "[0-9A-Fa-f]+")      

;; GUID
(setq jkw-rgs-guid
      (concat "{" jkw-rgs-hexnum "-" jkw-rgs-hexnum "-" jkw-rgs-hexnum "-"
              jkw-rgs-hexnum "-" jkw-rgs-hexnum "}"))

(setq jkw-rgs-stringx 
      "[ \t]*=[ \t]*s[ \t]*")

;; Patterns that describe a standard rgs file...
(setq jkw-rgs-pattern 
      (list 
       "^HKCR[ \t]*$"
       "^[ \t]*{[ \t]*$"
       (concat "^[ \t]*\\(" jkw-rgs-vprogid "\\)" jkw-rgs-stringx 
               "'\\(.*\\)'[ \t]*$")
       "^[ \t]*{[ \t]*$"
       (concat "^[ \t]*CLSID" jkw-rgs-stringx "'\\(" jkw-rgs-guid "\\)'[ \t]*$")
       "^[ \t]*}[ \t]*$"
       (concat "^[ \t]*\\(" jkw-rgs-progid 
               "\\)[ \t]*=[ \t]*s[ \t]*'\\(.*\\)'[ \t]*$")
       "^[ \t]*{[ \t]*$"
       (concat "^[ \t]*CLSID" jkw-rgs-stringx "'\\(" jkw-rgs-guid "\\)'[ \t]*$")
       (concat "^[ \t]*CurVer" jkw-rgs-stringx "'\\(" jkw-rgs-vprogid
               "\\)'[ \t]*$")
       "^[ \t]*}[ \t]*$"
       "^[ \t]*NoRemove[ \t]*CLSID[ \t]*$"
       "^[ \t]*{[ \t]*$"
       (concat "^[ \t]*ForceRemove[ \t]*\\(" jkw-rgs-guid 
               "\\)[ \t]*=[ \t]*s[ \t]*'.*'[ \t]*$")
       "^[ \t]*{[ \t]*$"
       (concat "^[ \t]*ProgID[ \t]*=[ \t]*s[ \t]*'\\(" jkw-rgs-vprogid 
               "\\)'[ \t]*$")
       (concat "^[ \t]*VersionIndependentProgID[ \t]*=[ \t]*s[ \t]*'\\("
               jkw-rgs-progid "\\)'[ \t]*$")
       ))
                
(defun jkw-matches-sequence (a-sequence)
  "Run through the provided sequence and return nil if the provided buffer does not match"
  
  (save-excursion
    (goto-char 0)
    (let ((working a-sequence) (i-matched t) (match-list) (this-line 1))
      (while (and working i-matched)
        (if (not (looking-at (car working)))
            (progn
              (setq i-matched nil)
              (message (format "No Match [%d]: \"%s\"" 
                               this-line (car working))))

          (message (format "Matched [%d]: \"%s\"" 
                           this-line (match-string 0)))
          (push (point) match-list)
          (setq working (cdr working))
          (forward-line 1)
          (setq this-line (1+ this-line))
          )
        )

      (message "Loop done")

      (if i-matched
          (progn 
            (message "Matched") 
            (reverse match-list)) ;;match-list
        nil)
      )))
  

(defun jkw-pattern-clean (a-string a-pattern)
  "Clean a-string of matches to a-pattern"
  (let ((temp (split-string a-string a-pattern)) (out))
    (while temp
      (setq out (concat out (car temp)))
      (setq temp (cdr temp))
      )
    out
    ))

(defun jkw-new-rgs-description (a-description)
  "Given a flawed description string propose a replacement"
  (interactive)
  (let ((trailing)
        (test-string "UI for the"))
    (if (string-match "^System[ \t]+\\(.*\\)$" a-description)
        (progn
          (message (format "OK-1: \"%s\"\n" a-description))
          (setq trailing (match-string 1 a-description))
          (message (format "OK-1t: \"%s\"\n" trailing))
          (if (string-match (concat "^" test-string "[ \t]+\\(.*\\)$") trailing)
              (progn
                (message (format "OK-2: \"%s\"\n" trailing))
                (setq trailing (match-string 1 trailing))))
          )
      )
    )
  )

(defun jkw-rgs-get-information (a-regex-list a-pos-list)
  "Given a buffer that contains valid *.rgs file information return the PtogId and descrption"
  
  (let ((retlist nil))
    (save-excursion
      (goto-char (nth 2 a-pos-list))
      (if (looking-at (nth 2 a-regex-list))
          (let ((mVPid (match-string 1)) (mDsc (match-string 2)) (mPid) (mPvn))
            (if (string-match (concat "\\(" jkw-rgs-progid
                                      "\\)\\.\\([0-9]+\\)") mVPid)
                (progn 
                  (setq mPid (match-string 1 mVPid))
                  (setq mPvn (match-string 2 mVPid))
                  (push nil retlist)

                  (push mPvn retlist)
                  (push mPid retlist)
                  (push mDsc retlist)
                  (push mVPid retlist))
              ))
        ))
    retlist
    ))
                         
(defun jkw-rgs-mycompany-progid-p (a-progid)
  "Given a progid return nil if it is not a valid ProgId"
  
  (message (format "Is \"%s\" a  ProgId?" a-progid))
  (let ((case-fold-search t) (succeeded t))
    ;; Must start with the company name...
    (if (not (string-match "^\\." a-progid))
        (progn (message "Name is not ") (setq succeeded nil))
      (message "Company name is OK"))

    ;; Must be no more than 36 characters long (39 is the limit but we
    ;; need to leave room for a version...
    (if (> (length a-progid) 37)
        (progn (message "ProgId is too long") (setq succeeded nil)))

    (if (string-match "[^A-Za-z0-9\\.]" a-progid)
        (progn (message "ProgId contains bad characters") 
               (setq succeeded nil)))

    (if succeeded (message "\tYes") (message "\tNo"))

    succeeded
    ))

(defun jkw-rgs-fix-progid (a-progid)
  "Try to convert a 'troubled' progid to a 'resonable' progid and return the result"

  (let ((case-fold-search t) (succeeded t) (max-length 39))
    (message (format "Fix rgs starting: \"%s\"" a-progid))

    ;; Remove troublesome characters...only alpha-numerics and '.' allowed.
    (if (string-match "[^A-Za-z0-9\\.]" a-progid)
        (setq a-progid (jkw-pattern-clean a-progid "[^A-Za-z0-9\\.]")))

    (message (format "Fix rgs chars: \"%s\"" a-progid))

    ;; Fix company name related problems...first part MUST be 'Mycompany'.
    (cond
     ;; Simple (unversioned) ProgId
     ((string-match (concat "^\\([A-Za-z][A-Za-z0-9]*\\)\\."
                            "\\([A-Za-z][A-Za-z0-9]*\\)$") a-progid)
      (message "Simple ProgId")
      (let ((i-company (match-string 1 a-progid)) 
            (i-component (match-string 2 a-progid)))
        (if (not (string-match "" i-company))
            (progn    
	      (message "Not ...fix it")
              (if (not (equal (downcase i-company) (downcase i-component)))
                  (setq i-component (concat i-company i-component)))
              (setq i-company ""))
	  (message "...OK"))

        ;; Now we need to clip the component part as needed...
        (let* ((iden-length (+ (length i-company) (length i-component) 1 1 3))
               (trim-to (- (length i-component) (- max-length iden-length))))
          (if (> iden-length max-length)
              (setq i-component (substring i-component 0 trim-to))
            ))

        (setq a-progid (concat i-company "." i-component))
        ))

     ;; Versioned ProgId
     ((string-match (concat "^\\([A-Za-z_][A-Za-z0-9_]*\\)\\."
                            "\\([A-Za-z_][A-Za-z0-9_]*\\)\\."
                            "\\([0-9]+\\)$") a-progid)
      (message "Versioned ProgId")
      (let ((i-company (match-string 1 a-progid)) 
            (i-component) (match-string 2 a-progid)
            (i-version) (match-string 3 a-progid))
        (if (not (string-match "mycompany" i-company))
            (progn                
              (if (not (equal (downcase i-company) (downcase i-component)))
                  (setq i-component (concat i-company i-component)))
              (setq i-company "Mycompany")
              ))

        ;; Now we need to clip the component part as needed...
        (let* ((iden-length (+ (length i-company) (length i-component) 1 1 3))
               (trim-to (- (length i-component) (- max-length iden-length))))
          (if (> iden-length max-length)
              (setq i-component (substring i-component 0 trim-to))
            ))

        (setq a-progid (concat i-company "." i-component "." i-version))
        ))

     (t (error "Suggested ProgId \"%s\" not suitable for fixing" a-progid))
     )

    (message (format "Fix rgs naming: \"%s\"" a-progid))
    
    ;; Return our final version...
    a-progid
    ))
