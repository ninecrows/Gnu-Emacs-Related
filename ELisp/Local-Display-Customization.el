;;-------------------------------------------------------------------;;
;;  Copyright (c) 2002 Kyle Wilson. All Rights Reserved.
;;  Time-stamp:    <2002-05-13 11:02:28 jameskw>
;;  Part-of:       Emacs Initialization Files
;;  File Name:     "Home/ELisp/Local-Display-Customization.el"
;;  Date(s):       Monday May 13, 2002
;;  Author(s):     Kyle Wilson
;;-------------------------------------------------------------------;;
;; 
;;-------------------------------------------------------------------;;
;; Revision Information:
;;   (jkw) [Monday May 13, 2002] Initial Entry
;;-------------------------------------------------------------------;;

;;-------------------------------------------------------------------;;
;; (jkw) [Monday May 13, 2002]
;;-------------------------------------------------------------------;;
;; This should actually be dependent on the current hostname...
;;-------------------------------------------------------------------;;
;; Arguments:
;;     None.
;;-------------------------------------------------------------------;;

(defun setup-my-preferred-font () "Select my preferred font for editing"
  (interactive)
  (set-default-font "-*-Courier New-bold-r-*-*-16-142-*-*-c-*-*-ansi-"))

;; Projector friendly (larger) font.
;;  (set-default-font "-*-Courier New-bold-r-*-*-22-142-*-*-c-*-*-ansi-")

;;-------------------------------------------------------------------;;
;; (jkw) [Monday May 13, 2002]
;;-------------------------------------------------------------------;;
;; 
;;-------------------------------------------------------------------;;
;; Arguments:
;;     None.
;;-------------------------------------------------------------------;;

(defun setup-main-colors () "This function sets up my preferred color scheme" 
  (interactive)
  (set-background-color "brown4")
  ;;(set-background-color "dark green")
  (set-cursor-color "yellow")
  (set-foreground-color "yellow2")

  (set-face-foreground 'default "yellow2")
  (set-face-background 'default "brown4")

  ;; Setup modeline colors...
  ;;(set-face-foreground 'modeline "light gray")
  ;;(set-face-background 'modeline "dark slate gray")

  (w32-define-rgb-color 120 35 35 "dark brown 4")
  (set-face-background 'region "dark brown 4")
  ;;(w32-define-rgb-color 255 255 0 "special bright yellow")
  ;;(set-face-foreground 'region "special bright yellow")
  ;;(set-face-foreground 'region nil)
                                        ;(set-face-background 'region "yellow")
  ;;(set-face-background 'region "brown3")

  ;;(set-face-background 'highlight "dark brown 4")
  (w32-define-rgb-color 139 75 15 "Greenish Brown 4")
  (set-face-background 'highlight "Greenish Brown 4")
  (set-face-foreground 'highlight "white")

  (set-face-background 'secondary-selection "dark green")

  (set-face-background 'show-paren-match-face "brown2")
  (set-face-foreground 'show-paren-match-face "yellow3")

  (set-face-background 'show-paren-mismatch-face "brown2")
  (set-face-foreground 'show-paren-mismatch-face "yellow3")
  )

;;-------------------------------------------------------------------;;
;; (jkw) [Monday May 13, 2002]
;;-------------------------------------------------------------------;;
;; 
;;-------------------------------------------------------------------;;
;; Arguments:
;;     None.
;;-------------------------------------------------------------------;;

(defun setup-font-lock () 
  "This function sets up my preferred font lock highlighting"
  (interactive)

  (set-face-foreground 'font-lock-comment-face "darkgrey")
  (set-face-foreground 'font-lock-function-name-face "orange")
  (set-face-foreground 'font-lock-keyword-face "moccasin")
  (set-face-foreground 'font-lock-string-face "lawn green")
  (set-face-foreground 'font-lock-type-face "light blue")
  (set-face-foreground 'font-lock-variable-name-face "orchid")
  (set-face-foreground 'font-lock-builtin-face "deep sky blue")
  (set-face-foreground 'font-lock-constant-face "light green")
  (set-face-foreground 'font-lock-warning-face "deep pink")
  )

;;-------------------------------------------------------------------;;
;; (jkw) [Monday May 13, 2002]
;;-------------------------------------------------------------------;;
;; 
;;-------------------------------------------------------------------;;
;; Arguments:
;;     None.
;;-------------------------------------------------------------------;;

(defun setup-all-of-my-display-defaults () 
  "Calls all of my display defaults setup functions"
  (interactive)
  (setup-main-colors)
  )

