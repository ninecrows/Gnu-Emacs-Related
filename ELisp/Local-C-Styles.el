;;-------------------------------------------------------------------;;
;;  Copyright (c) 2002 Kyle Wilson. All Rights Reserved.
;;  Time-stamp:    <2002-05-13 11:09:57 jameskw>
;;  Part-of:       Emacs Initialization Files
;;  File Name:     "Home/ELisp/Local-C-Styles.el"
;;  Date(s):       Monday May 13, 2002
;;  Author(s):     Kyle Wilson
;;-------------------------------------------------------------------;;
;; 
;;-------------------------------------------------------------------;;
;; Revision Information:
;;   (jkw) [Monday May 13, 2002] Initial Entry
;;-------------------------------------------------------------------;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C and C++ common mode hook ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst schneider-c-style
  '(
    ;;basic offset used by + or - symbols 
    (c-basic-offset           . 4)

    (c-hanging-colons-alist   . ((member-init-intro    after)
                                 (inher-intro)
                                 (case-label           after)
                                 (label after)
                                 (access-label         after)))
    (c-hanging-braces-alist   . ((inline-open)
                                 (inline-close)))
    (c-cleanup-list           . (scope-operator))
    (c-offsets-alist          . ((arglist-close        . c-lineup-arglist)
                                 (comment-intro        . 0)
                                 (access-label         . -)
                                 (case-label           . +)
                                 (statement-case-intro . +)
                                 (substatement-open    . 0)
                                 (knr-argdecl-intro    . -)))
    (c-echo-syntactic-information-p . t)
    )
  "Schneider Programming Style"
  )

(defconst local-c-style
  '(
    ;;basic offset used by + or - symbols 
    (c-basic-offset           . 4)
    (tab-width                . 4)

    (c-hanging-colons-alist   . ((member-init-intro    after)
                                 (inher-intro)
                                 (case-label           after)
                                 (label after)
                                 (access-label         after)))
    (c-hanging-braces-alist   . ((inline-open)
                                 (inline-close)))
    (c-cleanup-list           . (scope-operator))
    (c-offsets-alist          . ((arglist-close        . c-lineup-arglist)
                                 (comment-intro        . 0)
                                 (access-label         . -)
                                 (label                . -)
                                 (case-label           . +)
                                 (statement-case-intro . +)
                                 (substatement-open    . 0)
                                 (topmost-intro        . 0)
                                 (arglist-intro        . 8)
                                 (knr-argdecl-intro    . -)))
    (c-echo-syntactic-information-p . t)
    )
  "Local Programming Style"
  )

(defconst Groove-c-style
  '(
    ;;basic offset used by + or - symbols 
    (c-basic-offset           . 4)
    (tab-width                . 4)

    (c-hanging-colons-alist   . ((member-init-intro    after)
                                 (inher-intro)
                                 (case-label           after)
                                 (label after)
                                 (access-label         after)))
    (c-hanging-braces-alist   . ((inline-open)
                                 (inline-close)))
    (c-cleanup-list           . (scope-operator))
    (c-offsets-alist          . ((arglist-close        . c-lineup-arglist)
                                 (comment-intro        . 0)
                                 (access-label         . -)
                                 (label                . -)
                                 (case-label           . +)
                                 (statement-case-intro . +)
                                 (substatement-open    . 0)
                                 (topmost-intro        . 0)
                                 (arglist-intro        . 8)
                                 (knr-argdecl-intro    . -)))
    (c-echo-syntactic-information-p . t)
    )
  "Groove Programming Style"
  )

(defconst tr132-c-style
  '(
    ;;basic offset used by + or - symbols 
    (c-basic-offset           . 3)
    (tab-width                . 4)

    (c-hanging-colons-alist   . ((member-init-intro    after)
                                 (inher-intro)
                                 (case-label           after)
                                 (label after)
                                 (access-label         after)))
    (c-hanging-braces-alist   . ((inline-open)
                                 (substatement-open    after)
                                 (block-close        . c-snug-do-while)
                                 (inline-close)))
    (c-cleanup-list           . ((scope-operator)
                                 (brace-else-brace)))
    (c-offsets-alist          . ((arglist-close        . c-lineup-arglist)
                                 (comment-intro        . 0)
                                 (access-label         . -)
                                 (case-label           . +)
                                 (statement-case-intro . +)
                                 (statement-block-intro . +)
                                 (substatement-open    . +)
                                 (defun-open           . +)
                                 (defun-close          . 0)
                                 (topmost-intro        . +)
                                 (topmost-intro-cont   . +)
                                 (block-close          . +)
                                 (knr-argdecl-intro    . -)))
    (c-echo-syntactic-information-p . t)
    )
  "TR132 Coding Style"
  )

(defun my-c-mode-common-hook () 
  "Hook code that sets the local preferred C++ editing styles"
         ;; add my personal style and set it for the current buffer
         (c-add-style "TR132" tr132-c-style)
         (c-add-style "Schneider" schneider-c-style)
         (c-add-style "Local" local-c-style)
         (c-add-style "Groove" Groove-c-style t)

         (setq fill-column 76)
         (auto-fill-mode 1)

         ;; indent if at left margin otherwise insert TAB
         ;;(setq c-tab-always-indent nil)

         (define-key c-mode-map "\C-c\C-c" 'copy-region-as-kill-nomark)
         (define-key c-mode-map "\e\C-a" 'my-beginning-of-defun)
         (define-key c-mode-map "\e\C-e" 'my-end-of-defun)
         )
