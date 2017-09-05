;;-------------------------------------------------------------------;;
;;  Copyright (c) 2002 Kyle Wilson. All Rights Reserved.
;;  Time-stamp:    <2002-05-21 09:43:18 jameskw>
;;  Part-of:       Emacs Lisp Customization Files
;;  File Name:     "ELisp/Local-File-Headers.el"
;;  Date(s):       Tue May 21 09:43:18 2002
;;  Author(s):     Kyle Wilson
;;-------------------------------------------------------------------;;
;; Functions used to find, insert and update standard source code comment
;; blocks.
;;-------------------------------------------------------------------;;
;; Revision Information:
;;   (jkw) [Tue May 21 09:43:18 2002] Initial Entry
;;-------------------------------------------------------------------;;

;;===================================================================;;
;; (jkw) [Tue May 21 11:15:29 2002]
;;===================================================================;;
;; Read the various header section templates from the defined header
;; definition file and store them in their assigned variables.
;;===================================================================;;

(defun jkw-get-default-header-text ()
  "Setup for header text insertions
This code reads in the header template lines for the various C/C++ header
setup functions."

  (let ((section-pattern
         "^#-[ \t]*Section[ \t]+\\([-a-zA-Z0-9_\\.]+\\)[ \t]*-#[ \t]*$")
        (section-start nil) (this-start) (this-name) (section-name nil))
    (save-excursion
      (with-temp-buffer

        (if (insert-file-contents "~/headers.txt" nil nil nil t)
            (progn
              ;; Copy the available sections to buffers
              (goto-char 0)

              ;; Ought to start with a section pattern...
              (while (re-search-forward section-pattern nil t)
                (setq this-start (1+ (match-end 0)))
                (setq this-name (match-string 1))
                (message
                 (format "Header.txt: Found \"%s\" file header"
                         (match-string 1)))
                (if section-start
                    (cond
                     ;; Main file header...
                     ((equal section-name "main")
                      (setq jkw-main-header-base
                            (buffer-substring section-start
                                              (match-beginning 0)))
                      )

                     ;; Function/Defun header
                     ((equal section-name "function")
                      (setq jkw-function-header-base
                            (buffer-substring section-start
                                              (match-beginning 0)))
                      )

                     ;; Class/Section Header
                     ((equal section-name "class")
                      (setq jkw-class-header-base
                            (buffer-substring section-start
                                              (match-beginning 0)))
                      )

                     ;; Beginning of include file header
                     ((equal section-name "header-begins")
                      (setq jkw-begins-header-base
                            (buffer-substring section-start
                                              (match-beginning 0)))
                      )

                     ;; End of include file header
                     ((equal section-name "header-ends")
                      (setq jkw-ends-header-base
                            (buffer-substring section-start
                                              (match-beginning 0)))
                      )

                     ;; End of file...
                     ((equal section-name "end")
                      )
                     )
                  )

                ;; Feed the next section into the process...
                (setq section-start this-start)
                (setq section-name this-name)
                )
              )

          ;; Else...file wasn't found
          (message "Couldn't find ~/Headers.txt")
          )
        )
      )
    )
  )

;;===================================================================;;
;; (jkw) [Tue May 21 11:13:40 2002]
;;===================================================================;;
;; Insert various types of headers with the required sections updated as
;; needed.
;;===================================================================;;

(defun jkw-insert-file-header ()
  "Insert a properly formatted and updated file header block into this file"
  (interactive)

  (save-excursion
    (let ((text-block jkw-main-header-base)
          (working-file-name (jkw-groove-pathname (buffer-file-name)))
          (block-bounds))
      (goto-char (point-min))
      (save-restriction
        (narrow-to-region (point-min) (point-min))
        (insert jkw-main-header-base)

        ;; Update the copyright string in the new header...
        (goto-char (point-min))
        (while (re-search-forward "Copyright[ \t]+(c)\\(.*\\)$" nil t)
          (replace-match (concat " "
                                 (format-time-string "%Y") " "
                                 local-copyright-notice) t t nil 1))

        ;; Update the time-stamp...
        (save-excursion
          (goto-char (point-min))
          (time-stamp))

        ;; Update the project name...
        (goto-char (point-min))
        (while (re-search-forward "Part-of:[ \t]+\\(.*\\)$" nil t)
          (replace-match local-project-string t t nil 1))

        ;; Update the creation/insertion date...
        (goto-char (point-min))
        (while (re-search-forward "Date(s):[ \t]+\\(.*\\)$" nil t)
          (replace-match (current-time-string) t t nil 1))

        ;; Update the file-name field...
        (goto-char (point-min))
        (while (re-search-forward "File[ \t]+Name:[ \t]+\"\\(.*\\)\"[ \t]*$"
                                  nil t)
          (replace-match working-file-name t t nil 1))

        ;; Update standard fields and transmute comments (if necessary)
        (jkw-header-helper-updates)
        (jkw-header-helper-transmute-comments)

        (setq block-bounds (list (point-max)))
        )
      (jkw-header-helper-blanks-around block-bounds)
      )
    )
  )

(defun jkw-insert-function-header ()
  "Insert a properly formatted and updated function header block into this file"
  (interactive)

  (save-excursion
    (let ((text-block jkw-main-header-base)
          (working-file-name (jkw-groove-pathname (buffer-file-name)))
          (block-bounds))

      (save-restriction
        (narrow-to-region (point) (point))
        (insert jkw-function-header-base)

        ;; Update standard fields and transmute comments (if necessary)
        (jkw-header-helper-updates)
        (jkw-header-helper-transmute-comments)

        (setq block-bounds (list (point-max) (point-min)))
        )

      (jkw-header-helper-blanks-around block-bounds)
      )
    )
  )

(defun jkw-insert-class-header ()
  "Insert a properly formatted and updated function header block into this file"
  (interactive)

  (save-excursion
    (let ((text-block jkw-main-header-base)
          (working-file-name (jkw-groove-pathname (buffer-file-name)))
          (block-bounds))

      (save-restriction
        (narrow-to-region (point) (point))
        (insert jkw-class-header-base)

        ;; Update standard fields and transmute comments (if necessary)
        (jkw-header-helper-updates)
        (jkw-header-helper-transmute-comments)

        (setq block-bounds (list (point-max) (point-max)))
        )
      (jkw-header-helper-blanks-around block-bounds)
      )
    )
  )

(defun jkw-insert-header-begins ()
  "Insert a properly formatted and updated header file start block into this file"
  (interactive)

  (save-excursion
    (let ((text-block jkw-main-header-base)
          (working-file-name (jkw-groove-pathname (buffer-file-name)))
          (block-bounds))

      (save-restriction
        (narrow-to-region (point) (point))
        (insert jkw-begins-header-base)

        (let ((new-name
               (upcase (jkw-filename-to-identifier
                        (jkw-groove-pathname (buffer-file-name))))))
          (goto-char (point-min))
          (while (re-search-forward "\\(#if !defined(\\|#define \\)")
            (if (looking-at "Unique_Name_Goes_Here")
                (replace-match (concat "HAVE_" new-name))
              )
            )
          )

        ;; Update standard fields and transmute comments (if necessary)
        (jkw-header-helper-updates)
        (jkw-header-helper-transmute-comments)

        (setq block-bounds (list (point-max) (point-max)))
        )
      (jkw-header-helper-blanks-around block-bounds)
      )
    )
  )

(defun jkw-insert-header-ends ()
  "Insert a properly formatted and updated header file start block into this file"
  (interactive)

  (save-excursion
    (let ((text-block jkw-main-header-base)
          (working-file-name (jkw-groove-pathname (buffer-file-name)))
          (block-bounds))

      (save-restriction
        (narrow-to-region (point) (point))
        (insert jkw-ends-header-base)

        ;; Update standard fields and transmute comments (if necessary)
        (jkw-header-helper-updates)
        (jkw-header-helper-transmute-comments)

        (setq block-bounds (list (point-max) (point-max)))
        )
      (jkw-header-helper-blanks-around block-bounds)
      )
    )
  )

;;===================================================================;;
;; (jkw) [Tue May 21 11:11:45 2002]
;;===================================================================;;
;; Updaters for various types of headers.  Find the relevant header
;; block in the file (these assume that there may only be one) and
;; update its contents preserving the state of point and mark.
;;===================================================================;;

(defun jkw-update-header-wrapper ()
  "Test function that should locate a standard header comment and then narrow onto that comment"
  (interactive)

  (let ((fileext (file-name-extension buffer-file-name)))
    (if (equal fileext "h")
        (save-excursion
          (let ((defined-pattern
                  "^[ \t]*#[ \t]*if[ \t]*!defined(HAVE_\\([A-Za-z0-9_]+\\))[ \t]*$")
                (define-pattern
                  "^[ \t]*#[ \t]*define[ \t]+HAVE_\\([A-Za-z0-9_]+\\)[ \t]*$")
                (header-begins nil)
                (header-ends nil))
            (goto-char (point-min))
            (while (and (not (eobp)) (not header-begins))
              (if (jkw-header-begins-p)
                  (setq header-begins (point))
                (forward-line 1)
                )
              )

            ;; If we found a header definition block then lets find
            ;; the extent of it...
            (if header-begins
                (progn
                  (forward-line 7)
                  (setq header-ends (point))

                  ;; Now we can get down to business...
                  (let ((working-file-name
                         (upcase (jkw-filename-to-identifier
                                  (jkw-groove-pathname
                                   (buffer-file-name))))))
                    (save-restriction
                      (narrow-to-region header-begins header-ends)
                      (goto-char (point-min))
                      (forward-line 1)

                      (if (and (looking-at defined-pattern)
                               (not (looking-at (concat "^#if !defined(HAVE_"
                                                        working-file-name
                                                        ")"))))
                          (replace-match (concat "#if !defined(HAVE_"
                                                 working-file-name ")")))

                      (forward-line 1)
                      (if (and (looking-at define-pattern)
                               (not (looking-at (concat "^#define HAVE_"
                                                        working-file-name
                                                        "[ \t]*$"))))
                          (replace-match (concat "#define HAVE_"
                                                 working-file-name)))
                      )
                    )
                  )

              (message "No header block found")
              )
            )
          )
      )
    )
  )

(defun jkw-update-file-header-block ()
  "Test function that should locate a standard header comment and then narrow onto that comment"
  (interactive)

  (let ((fileext (file-name-extension buffer-file-name)))
    (if (or (equal fileext "h") (equal fileext "c") (equal fileext "cpp"))
        (save-excursion
          (goto-char (point-min))
          (let ((parts-list nil))

            ;; Run until we run out of space or find a header.
            (while (and (not (eobp)) (not parts-list))
              (setq parts-list (jkw-file-header-begins-p))
              (forward-line 1))

            ;; If we found a header we need to start peeling it apart...
            (if parts-list
                (let ((list-items parts-list) (item-number 0) (last-item nil))
                  (while list-items
                    (let ((this-item (pop list-items)))
                      (goto-char this-item)
                      (cond
                       ;; End of the comment block...make sure that we have
                       ;; one and only one blank line here...
                       ((equal item-number 0)
                        (if (not (looking-at "^[ \t]*\n")) (insert "\n"))
                        (if (looking-at "^[ \t]*\n\\([ \t]*\n\\)+") 
                            (replace-match "\n")))

                       ;; Beginning of the revision information block.
                       ((equal item-number 1))

                       ;; Beginning of the 'free-form' description block.
                       ((equal item-number 2))

                       ;; Author name block
                       ((equal item-number 4))

                       ;; Creation date block
                       ((equal item-number 5))

                       ;; File-name block
                       ((equal item-number 6)
                        (jkw-header-helper-update-filename this-item
                                                           last-item))

                       ;; Part-of block...update if needed...
                       ((equal item-number 7)
                        (if (looking-at
                             "^[ \t]*//[ \t]+Part-of:[ \t]+\\(.*\\)[ \t]*$")
                            (progn
                              (if (not (equal (jkw-project-string)
                                              (match-string 1)))
                                  (replace-match (jkw-project-string)
                                                 t nil nil 1)))
                          ))

                       ;; Time-stamp block
                       ((equal item-number 8))

                       ;; Copyright block
                       ((equal item-number 9)
                        (jkw-header-helper-update-copyright this-item
                                                            last-item)
                        )

                       ;; Beginning of the comment header
                       ((equal item-number 10)
                        (if (equal (forward-line -1) 0)
                            (if (looking-at "^[ \t]*$")
                                (progn (delete-blank-lines)
                                       (delete-char 1 nil)))))
                       )

                      (setq item-number (1+ item-number))
                      (setq last-item this-item)
                      )
                    )
                  )
              (message "No header block found"))
            )
          )
      )
    )
  )

;;===================================================================;;
;; (jkw) [Tue May 21 11:08:52 2002]
;;===================================================================;;
;; Predicates that return nil if not looking at the appropriate type
;; of header.  They generally return some sort of useful information
;; if they are looking at their selected type of header.
;;===================================================================;;

(defun jkw-header-begins-p ()
  "If this returns non-nil then point is looking at a header file begins block
If we are looking at a proper header then the returned value is the buffer
offset to the end of the header block."
  (interactive)

  ;; Define the list of patterns that must match before we have success...
  (let ((pattern-list
         '("^[ \t]*//\\+\\+\\+\\+\\+\\++//[ \t]*$"
           "^[ \t]*#[ \t]*if[ \t]*!defined(HAVE_\\([A-Za-z0-9_]+\\))[ \t]*$"
           "^[ \t]*#[ \t]*define[ \t]+HAVE_\\([A-Za-z0-9_]+\\)[ \t]*$"
           "^[ \t]*#[ \t]*if[ \t]+defined(_SHOW_INCLUDED_FILES_)[ \t]*$"
           "^[ \t]*#[ \t]*pragma[ \t]+message(\"Included : <\" __FILE__ \">\")[ \t]*"
           "^[ \t]*#[ \t]*endif[ \t]*$"
           "^[ \t]*//\\+\\+\\+\\+\\+\\++//[ \t]*$")))

    (jkw-check-match-lines pattern-list)
    )
  )

(defun jkw-file-header-begins-p ()
  "If this returns non-nil then point is looking at a file header block
Returns a list of buffer positions where various items may be found:
  EndOfHeader Revision Description Author Date FileName PartOf TimeStamp
  Copyright"

  (interactive)

  ;; Define the list of patterns that must match before we have success...
  (let ((matched t)
        (pattern-list '("^[ \t]*//-------+//[ \t]*$"
                        "^[ \t]*//[ \t]*Copyright[ \t]+(c)[ \t]+[0-9]+"
                        "^[ \t]*//[ \t]*Time-stamp:"
                        "^[ \t]*//[ \t]*Part-of:"
                        "^[ \t]*//[ \t]*File[ \t]*Name:"
                        "^[ \t]*//[ \t]*Date(s):"
                        "^[ \t]*//[ \t]*Author(s):"
                        "^[ \t]*//-------+//[ \t]*$"))
        (pattern-cont "^[ \t]*//[ \t]+[A-Za-z_/\\.]+\"[ \t]*$"))

    (save-excursion
      (setq matched (jkw-check-match-lines-contin pattern-cont pattern-list))

      ;;(message "Looking over the description section")

      ;; We reached then end of the standard header without
      ;; incident...now traverse the free-form description area.
      (if matched
          (progn
            ;; Move to the end of the matched section...
            (goto-char (car matched))

            ;; At this point we're looking at the beginning of the
            ;; description section.  We should step past lines that begin
            ;; with '//' until we find a '//----...//' followed immediately
            ;; by a '// Revision Information:' line.  We should bail if any
            ;; lines fail to meet our expectations.

            ;; Skip over the description section...
            (while (or (looking-at "^[ \t]*//[ \t]*[^-]")
                       (looking-at "^[ \t]*//[ \t]*$"))
              (forward-line 1))

            ;; At this point we should be looking at a break line...if
            ;; we aren't then set the matched flag to nil to defeat
            ;; further scanning...
            (if  (looking-at "^[ \t]*//-------+//[ \t]*$")
                (forward-line 1)
              (setq matched nil))

            ;; Add the beginning of the revision block to the list...
            (if matched
                (push (point) matched))
            )
        )

      ;; Once at the revision block we should step past any lines that
      ;; are not '//----...----//' in form and when the ending dashed
      ;; comment is reached we are done.  If any other lines are
      ;; reached then we have failed...

      ;;(message "Looking over the revision block")

      (if matched
          (progn
            ;; Skip over the description section...
            (while (or (looking-at "^[ \t]*//[ \t]*[^-]")
                       (looking-at "^[ \t]*//[ \t]*$"))
              (forward-line 1))

            ;; At this point we should be looking at a break line...if
            ;; we aren't then set the matched flag to nil to defeat
            ;; further scanning...
            (if (looking-at "^[ \t]*//-------+//[ \t]*$")
                (forward-line 1)
              (setq matched nil))

            ;; Add the beginning of the revision block to the list...
            (if matched
                (push (point) matched))
            )
        )

      ;; if matched is still 't' here then we found what we expected...
      matched
      )
    )
  )

;;===================================================================;;
;; (jkw) [Tue May 21 11:07:13 2002]
;;===================================================================;;
;; Helper function used by the header code and too specialized to be
;; of much used elsewhere.
;;===================================================================;;

(defun jkw-header-helper-updates ()
  "Helper function to update header fields.
Expects to be called with a restriction set to limit it's scope"

  ;; Update the Author name...
  (goto-char (point-min))
  (while (re-search-forward "Author(s):[ \t]+\\(.*\\)$" nil t)
    (replace-match (jkw-user-fullname) t t nil 1))

  ;; Update any autor's initials entries...
  (goto-char (point-min))
  (while (re-search-forward "(\\(\\?\\?\\?\\))" nil t)
    (replace-match (jkw-user-initials) t t nil 1))

  ;; Update any standard format dates...
  (goto-char (point-min))
  (while (re-search-forward "\\[\\(DateStamp\\)\\]" nil t)
    (replace-match (current-time-string) t t nil 1))
  )

(defun jkw-header-helper-transmute-comments ()
  "Try to transmute comment introducer characters into the form appropriate to this file"

  ;; Check the file extension and make the appropriate changes...
  (let ((fileext (file-name-extension buffer-file-name)))
    (cond
     ;; If this is a lisp file then we need to transmute the comment
     ((or (equal fileext "el"))
      ;; Replace leading comment characters...
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*//" nil t)
        (replace-match ";;"))

      ;; Replace trailing comment characters...
      (goto-char (point-min))
      (while (re-search-forward "//[ \t]*$" nil t)
        (replace-match ";;")))
     )
    )
  )

(defun jkw-header-helper-blanks-around (a-bounds)
  "Try to make sure that there is exactly one blank line before and after this block"

  ;; Fix up trailing blank lines...
  (save-excursion
    (goto-char (car a-bounds))
    (if (looking-at "^[ \t]*$")
        (delete-blank-lines)
      (insert "\n"))
    )

  ;; If a leading space buffer position was provided then fix that up too...
  (if (cdr a-bounds)
      (save-excursion
        (goto-char (car (cdr a-bounds)))
        (if (equal (forward-line -1) 0)
            (if (looking-at "^[ \t]*$")
                (delete-blank-lines)
              (forward-line 1)
              (insert "\n")))
        )
    )
  )

(defun jkw-header-helper-update-filename (a-position a-lastposition)
  "Update the file-name field at the given position in the buffer
This field may flow over onto following lines"
  (save-excursion
    (if (re-search-forward "^[ \t]*//[ \t]+File ?Name:[ \t]*")
        (let ((this-point (point)))
          (if (or (and (looking-at "\"\\(.*\\)\"[ \t]*$")
                       (not (equal (match-string 1) 
                                   (jkw-groove-pathname (buffer-file-name)))))
                  (not (looking-at "\"\\(.*\\)\"[ \t]*$")))
              (progn
                (delete-region this-point a-lastposition)
                (insert (concat "\"" (jkw-groove-pathname (buffer-file-name))
                                "\"\n")))
            )
          )
      )
    )
  )

(defun jkw-header-helper-update-copyright (a-position a-lastposition)
  "Update the copyright field at the given position in the buffer"

  (let ((base-cp-string "^[ \t]*//[ \t]+Copyright[ \t]+(c)[ \t]+")
        (this-year (format-time-string "%Y")))
    (cond

     ;; Single year copyright string...
     ((looking-at (concat base-cp-string "\\([0-9]+\\)[ \t]+\\(.*\\)[ \t]*$"))
      (if (not (equal (jkw-copyright-string) (match-string 2)))
          (replace-match (jkw-copyright-string) t nil nil 2))
      (if (not (equal this-year (match-string 1)))
          (replace-match (concat (match-string 1) "-" this-year) t nil nil 1))
      )

     ;; Multi-year copyright string
     ((looking-at (concat base-cp-string
                          "\\([0-9]+\\)-\\([0-9]+\\)[ \t]+\\(.*\\)[ \t]*$"))
      (if (not (equal (jkw-copyright-string) (match-string 3)))
          (replace-match (jkw-copyright-string) t nil nil 3))
      (if (not (equal this-year (match-string 2)))
          (replace-match this-year t nil nil 2))
      )

     (t (message "No copyright string match"))
     )
    )
  )

(defun c-sandbox ()
  "Set copyright and project for local sandbox work"
  (interactive)
  
  (message (format "Set to sandbox"))
  (setq local-project-string "Local Sandbox Samples")
  (setq local-copyright-notice "Kyle Wilson. All Rights Reserved.")
  )

(defun c-groove-eis ()
  "Set copyright and project for Groove EIS work"
  (interactive)
  
  (message (format "Set to Groove EIS System"))
  (setq local-copyright-notice "Groove Networks Corporation. All Rights Reserved.")
  (setq local-project-string "Groove EIS Software")
  )