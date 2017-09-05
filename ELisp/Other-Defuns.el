;;-------------------------------------------------------------------;;
;;  Copyright (c) 2002 Kyle Wilson. All Rights Reserved.
;;  Time-stamp:    <2002-05-13 10:27:50 jameskw>
;;  Part-of:       Emacs List Setup Files
;;  File Name:     "Home/ELisp/other_funcs.el"
;;  Date(s):       Monday May 13, 2002
;;  Author(s):     Kyle Wilson
;;-------------------------------------------------------------------;;
;; This file contains an assortment of bits taken from other folks
;; init files.
;;-------------------------------------------------------------------;;
;; Revision Information:
;;   (jkw) [Monday May 13, 2002] Initial Entry
;;-------------------------------------------------------------------;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; taken originally from c++-mode.el (Lucid Emacs V19.10)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-match-paren ()
  "Jumps to the paren matching the one under point, if there is one."
  (interactive)
  ;;  (let ((parse-sexp-ignore-comments (memq 'v19 c++-emacs-features)))
  (let ((parse-sexp-ignore-comments t))
    (cond
     ((looking-at "[\(\[{]")
      (forward-sexp 1)
      (backward-char))
     ((looking-at "[])}]")
      (forward-char)
      (backward-sexp 1))
     (t (message "Could not find matching paren.")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom functions from Russ Kuhn;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun scroll-down-in-place (n)
  (interactive "p")
  (previous-line n)
  (scroll-down n)
  )

(defun scroll-up-in-place (n)
  (interactive "p")
  (next-line n)
  (scroll-up n)
  )

;;-------------------------------------------------------------------;;
;; (jkw) [Monday May 13, 2002]
;;-------------------------------------------------------------------;;
;; Ask one extra time when shutting down emacs whether we really want
;; to shut down.  This make it less likely that an Emacs with lots of
;; live but not dirty buffers will get killed inadvertently.
;;-------------------------------------------------------------------;;
;; Arguments:
;;     None.
;;-------------------------------------------------------------------;;

(setq kill-emacs-query-functions 'my-kill-emacs-query-hook)
(defun my-kill-emacs-query-hook ()
  (interactive)
  (ding)
  (yes-or-no-p "Do you really want to exit Emacs? "))

