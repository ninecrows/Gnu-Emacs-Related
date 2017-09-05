;;-------------------------------------------------------------------;;
;;  Copyright (c) 2002 Kyle Wilson. All Rights Reserved.
;;  Time-stamp:    <2002-05-13 11:07:00 jameskw>
;;  Part-of:       Emacs Initialization Files
;;  File Name:     "Home/ELisp/Local-Key-Bindings.el"
;;  Date(s):       Monday May 13, 2002
;;  Author(s):     Kyle Wilson
;;-------------------------------------------------------------------;;
;; This files sets up global key definitions for Emacs.
;;-------------------------------------------------------------------;;
;; Revision Information:
;;   (jkw) [Monday May 13, 2002] Initial Entry
;;-------------------------------------------------------------------;;

(define-key ctl-x-map [f1] 'bury-buffer)
(define-key global-map [C-f11] 'reindent-buffer-preserve-point)
(define-key global-map [C-f12] 'xw_curlies)
(define-key global-map [C-f1] 'go-compile)
(define-key global-map [C-f5] 'query-replace-regexp)
(define-key global-map [f11] 'jkw-code-modify)
(define-key global-map [insert] 'auto-fill-mode)
(define-key global-map [f8] 'jkw-fix-includes)

(global-set-key "\C-c\C-c" 'copy-region-as-kill-nomark)
(global-set-key "\eg" 'goto-line)

(global-set-key [S-down] 'scroll-up-in-place)
(global-set-key [S-up] 'scroll-down-in-place)
(global-set-key [f7] 'undo)

(global-set-key [kp-6] 'time-stamp)
(global-set-key [kp-0] 'next-error)
(global-set-key [kp-enter] 'call-last-kbd-macro)

;; New better code cleaning defuns...
(define-key global-map [kp-7] 'jkw-dsp-checkup)
(define-key global-map [kp-8] 'jkw-fix)
(define-key global-map [kp-9] 'jkw-code-modify)
(define-key global-map [kp-3] 'jkw-forced-revert)
(define-key global-map [kp-5] 'msdev-compile)

(global-set-key "\C-c1" 'jkw-insert-file-header)
(global-set-key "\C-c2" 'jkw-insert-function-header)
(global-set-key "\C-c3" 'jkw-insert-class-header)
(global-set-key "\C-c4" 'jkw-insert-header-begins)
(global-set-key "\C-c5" 'jkw-insert-header-ends)

;; Temporary...
(global-set-key "\C-c0" 'jkw-file-header-begins-p)

(define-key global-map [f6] 'jkw-insert-argument)

(define-key global-map [f5] 'jkw-fix-rgs-naming)