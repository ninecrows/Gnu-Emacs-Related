;;-------------------------------------------------------------------;;
;;  Copyright (c) 2002 Kyle Wilson. All Rights Reserved.
;;  Time-stamp:    <2002-05-13 11:05:01 jameskw>
;;  Part-of:       Emacs Initialization Files
;;  File Name:     "Home/ELisp/Local-Enable-Interactive.el"
;;  Date(s):       Monday May 13, 2002
;;  Author(s):     Kyle Wilson
;;-------------------------------------------------------------------;;
;; This file sets the 'enable' keys for various interactive commands
;; that are set to disabled by default.
;;-------------------------------------------------------------------;;
;; Revision Information:
;;   (jkw) [Monday May 13, 2002] Initial Entry
;;-------------------------------------------------------------------;;

(put 'eval-expression 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
