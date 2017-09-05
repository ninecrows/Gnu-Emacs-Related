;;-------------------------------------------------------------------;;
;;  Copyright (c) 2002 Kyle Wilson. All Rights Reserved.
;;  Time-stamp:    <2002-05-13 10:55:06 jameskw>
;;  Part-of:       Emacs Initialization Files
;;  File Name:     "Home/_emacs"
;;  Date(s):       Monday May 13, 2002
;;  Author(s):     Kyle Wilson
;;-------------------------------------------------------------------;;
;; This is my main emacs setup file.  Defuns are loaded from a number
;; of other places and invoked from here...
;;-------------------------------------------------------------------;;
;; Revision Information:
;;   (jkw) [Monday May 13, 2002] Initial Entry
;;-------------------------------------------------------------------;;

 (when (>= emacs-major-version 24)
      (require 'package)
      (setq package-enable-at-startup nil)
      (setq package-archives '())
      (package-initialize)
      (add-to-list 'package-archives
                   '("melpa" . "http://melpa.milkbox.net/packages/") t)
      (add-to-list 'package-archives
                   '("gnu" . "http://elpa.gnu.org/packages/"))  
      (add-to-list 'package-archives
                   '("marmalade" . "https://marmalade-repo.org/packages/"))
      (add-to-list 'package-archives
                   '("org" . "http://orgmode.org/elpa/") t)
      (add-to-list 'package-archives
                   '("tromey" . "http://tromey.com/elpa/") t)
      )

;;===================================================================;;
;; (jkw) [Wednesday March 08, 2000]
;;===================================================================;;
;; Configuration and setup variables...
;;===================================================================;;

(setq local-elisp-folder "c:/Local/Home/ELisp")

(cond
 ((equal (downcase (system-name)) "logrus")
  (setq local-source-project "i:/projects"))

 ((equal (downcase (system-name)) "mht-l-fyjmr32")
  (setq local-source-project "c:/Local/Projects/KMC/MQ/IEC/source/trunk/qnx"))

 ((equal (downcase (system-name)) "chaos")
  (setq local-source-project "c:/"))

 ((equal (downcase (system-name)) "kylewilson")
  (setq local-source-project "d:/Projects/ONI"))

 ((equal (downcase (system-name)) "pattern")
  (setq local-source-project "s:/Projects/ONI"))

 ;; Point this to somewhere plausible if we don't know where we are...
 (t
  (setq local-source-project "c:/Local/Projects/KMC"))
 )

;; Copyright notice to use by default and default project for comment headers.
(setq local-copyright-notice "KMC Systems Inc.")
(setq local-project-string "MQ")

(setq mouse-drag-copy-region t)

(setq compilation-auto-jump-to-first-error t)

;;===================================================================;;
;; (jkw) [Wednesday March 08, 2000]
;;===================================================================;;
;; Other setup and configuration bits...
;;===================================================================;;

(setq user-full-name "Kyle Wilson")

;;(cd local-source-project)
;;(setq default-directory local-source-projecti

(defun myactive () 
  "" 
  (interactive) 
  (find-file "c:/local/projects/KMC/MQ/trunk/trunk/qnx/apps/TestTracker"))

;; Point the load path to my local elisp repository...
(setq load-path (cons local-elisp-folder load-path))
;;(setq load-path (cons "C:/Emacs-21.1/site-lisp/cc-mode-5.30.6" load-path))

;;===================================================================;;
;; (jkw) [Wednesday March 08, 2000]
;;===================================================================;;
;; Load local utility code...
;;===================================================================;;

;; Various functions borrowed from other folks init files...
(load "Other-Defuns.el")

;; Local customizations functions...
(load "Local-Defuns.el")

;; Compile command shims for building OTS system code...
(load "Custom-Compile-Commands.el")

;; Enable disabled interactive commands here...
(load "Local-C-Styles.el")
(load "Local-Enable-Interactive.el")
(load "Local-Display-Customization.el")
(load "Local-Ps-Printing.el")

(load "Local-Key-Bindings.el")
(load "Local-Set-Extra-Registers.el")

(load "Local-Temporary-Defuns.el")

(load "Local-Experimental.el")
(load "Local-File-Fixup.el")
(load "Local-File-Headers.el")

;; Completion library
;;(load "complete")

;;(require 'magit)

;;-------------------------------------------------------------------;;
;; (jkw) [Monday May 13, 2002]
;;-------------------------------------------------------------------;;
;; Now start hooking things up.
;;-------------------------------------------------------------------;;

(jkw-get-default-header-text)

;; And enable icomplete minor mode...
(defvar grep-null-device null-device)

;; completion
(setq completion-ignored-extensions
      (append completion-ignored-extensions
              '(".obj" ".exe" ".bin" ".com" ".zip" ".lzh" ".cod" ".bk0" ".bk1"
                ".bk2" ".bk3" ".bk4" ".bk5" ".bk6" ".bk7" ".bk8" ".bk9")))

(setq kept-new-versions '10)
(setq version-control t)

(show-paren-mode 1)
(transient-mark-mode t)

;; Timestamp can be farther down the file than the default.
(setq time-stamp-line-limit 30)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(case-fold-search t)
 '(current-language-environment "Latin-1")
 '(default-input-method "latin-1-prefix")
 '(dired-garbage-files-regexp
   "\\.log$\\|\\.toc$\\|\\.dvi$\\|\\.bak$\\|\\.orig$\\|\\.rej$|\\.obj$|\\.tmp$|\\.lib$|\\.pdb$|\\.pch$" t)
 '(global-font-lock-mode t nil (font-lock))
 '(package-selected-packages (quote (csharp-mode)))
 '(show-paren-mode t nil (paren))
 '(transient-mark-mode t))

;; No longer valid, but I don't have a postscript printer so it really
;; doesn't matter.
;;(setq ps-printer-name "//kylewilson/Konica7030")
;;(setq ps-printer-name "//hcu-d3c9jl1/bosprt019")
(setq ps-printer-name "//kmcpsp001/KM-XEROX-22")
      ;(setq ps-lpr-command "C:\\Program Files\\Ghostgum\\gsview\\gsprint.exe")
      ;;(setq ps-lpr-command "C:\\gs\\gs8.14\\bin\\gswin32c.exe")

      ;; THis line causes ghostscript to query which printer to
      ;; use - which you may not need if, for example, you only
      ;; have one printer.
      ;;(setq ps-lpr-switches '("-dBATCH -dNOPAUSE -sDEVICE#laserjet -sOutputFile#c:/temp/Test.out "))
;; -sOutputFile=\"%printer%\\\\poor\\BigBuild\"
;; -sOutputFile#\"c:\\Temp\\TestOutputEmacs.bin\"
      ;;(setq ps-printer-name t)
(ps-stapled-duplex-mode)
(ps-one-up-printing)
(ps-ref-docs)
(setq ps-line-number t)

(setq compile-command "make")

(setq user-mail-address "kyle@ninecrows.com")
(setq smtpmail-default-smtp-server "amermsx.med.ge.com") ; mail.adelphia.net
(setq smtpmail-local-domain nil)
(setq send-mail-function 'smtpmail-send-it)
(load-library "smtpmail")

(setenv "MAILHOST" "amermsx.med.ge.com") ; mail.adelphia.net
;;(setq rmail-primary-inbox-list '("po:Kyle") rmail-pop-password-required)

;;===================================================================;;
;; (jkw) [Wednesday March 08, 2000]
;;===================================================================;;
;; More misc setup
;;===================================================================;;

;;(add-to-list 'file-name-buffer-file-type-alist (cons "\\.tpl$" nil))
;;(add-to-list 'file-name-buffer-file-type-alist (cons "\\.grp$" nil))

;; add to the 'auto' load list
(add-to-list 'auto-mode-alist (cons "\\.hpp$" 'c++-mode))
(add-to-list 'auto-mode-alist (cons "\\.h$" 'c++-mode))
(add-to-list 'auto-mode-alist (cons "\\.c$" 'c++-mode))
;;(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
;;(add-to-list 'auto-mode-alist (cons "\\.cs$" 'csharp-mode))

;; Groove XML File Types...
;;(add-to-list 'auto-mode-alist (cons "\\.tpl$" 'sgml-mode))
;;(add-to-list 'auto-mode-alist (cons "\\.grv$" 'sgml-mode))
;;(add-to-list 'auto-mode-alist (cons "\\.grp$" 'sgml-mode))
;;(add-to-list 'auto-mode-alist (cons "\\.gsl$" 'sgml-mode))
;;(add-to-list 'auto-mode-alist (cons "\\.rbd$" 'sgml-mode))
;;(add-to-list 'auto-mode-alist (cons "\\.osd$" 'sgml-mode))
;;(add-to-list 'auto-mode-alist (cons "\\.tst$" 'sgml-mode))

;; Position and size the primary frame...Assumes a dual monitor setup at work...
(cond
 ((equal (downcase (system-name)) "logrus")
  (set-frame-position (selected-frame) 400 5)
  (set-frame-width    (selected-frame) 81)
  (set-frame-height   (selected-frame) 53)
  )

 ((equal (downcase (system-name)) "kylewilson")
  (set-frame-position (selected-frame) 420 5)
  (set-frame-width    (selected-frame) 81)
  (set-frame-height   (selected-frame) 53)
  )

 ((equal (downcase (system-name)) "chaos")
  (set-frame-position (selected-frame) 400 5)
  (set-frame-width    (selected-frame) 81)
  (set-frame-height   (selected-frame) 53)
  )
 )  

;;;;;;;;;;;;;;;;;;;
;; General setup;;
;;;;;;;;;;;;;;;;;;;

(setq next-line-add-newlines nil) ; next-line doesn't insert blank lines
(setq line-number-mode t) ; display line # in mode line
(setq-default tab-width 8) 
(setq-default indent-tabs-mode nil) ; TABs inserts spaces only
(setq-default win32-downcase-file-names t)
(transient-mark-mode t) ; make selections highlight (needed by
			; copy-region-as-kill-nomark)

(setq archive-zip-extract  '("unzip" "-e"))

(defun my-sgml-mode-hook () "My local SGML hook customizations"
  (setq tab-width 2)
  ;;(set-buffer-file-coding-system 'undecided-dos nil)
  )

;; Inject c-mode customizations...
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c++-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'sgml-mode-hook 'my-sgml-mode-hook)

(setq text-mode-hook
      '(lambda ()
         (setq fill-column 73)
         (auto-fill-mode 1)
         ))

;;;;;;;;;;;;;;;;;;;;
;; Find File hooks;;
;;;;;;;;;;;;;;;;;;;;
(add-hook 'find-file-hooks
          (function
           (lambda ()

     ;; If the file loads into C-mode but has C++ comments, change the
             ;; mode to C++ mode.
             (if (and (equal major-mode 'c-mode)
                      (search-forward "//" nil t))
                 (progn
                   (c++-mode)
                   (beginning-of-buffer)))
             )))

;;;;;;;;;;;;;;;;;
;; ediff Setup;;
;;;;;;;;;;;;;;;;;

(add-hook 'ediff-before-setup-windows-hook
          ;;(add-hook 'ediff-load-hooks
          (function
           (lambda ()

             (setq ediff-split-window-function 'split-window-horizontally)
             (setq ediff-window-setup-function 'ediff-setup-windows-plain)
             )))

;;;;;;;;;;;;;;;;;;;;
;; Font-Locksetup;;
;;;;;;;;;;;;;;;;;;;;
(add-hook    'c-mode-hook           'turn-on-font-lock)
(add-hook    'c++-mode-hook         'turn-on-font-lock)
(remove-hook 'lisp-mode-hook        'turn-on-font-lock)
(add-hook    'emacs-lisp-mode-hook  'turn-on-font-lock)

(setq font-lock-maximum-decoration t)

(add-hook 'font-lock-mode-hook 'setup-font-lock)

(setup-my-preferred-font)
(setup-all-of-my-display-defaults)

(add-hook 'after-make-frame-functions 'local-set-frame-settings)

(setq text-mode-hook '(lambda ()
                        (local-set-key "\M-\t" 'ispell-complete-word)))
(setq tex-mode-hook '(lambda ()
                       (local-set-key "\M-\t" 'ispell-complete-word)))
(setq latex-mode-hook '(lambda ()
                         (local-set-key "\M-\t" 'ispell-complete-word)))

(set-message-beep 'ok)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(nxml-attribute-local-name-face ((t (:inherit nxml-name-face :foreground "light blue"))))
 '(nxml-attribute-prefix-face ((t (:inherit nxml-name-face :foreground "green"))))
 '(nxml-cdata-section-CDATA-face ((t (:inherit nxml-name-face :foreground "yellow"))))
 '(nxml-comment-content-face ((t (:foreground "gray" :slant italic))))
 '(nxml-element-local-name-face ((t (:inherit nxml-name-face :foreground "green")))))

;; Setup default printing mode
(ps-ref-docs)
(ps-stapled-duplex-mode)

(set-extra-registers)

;;(add-to-list 'load-path (expand-file-name "C:/Emacs-21.1/site-lisp/xae-1.0beta8/lisp"))
;(require 'xae)

;;(add-to-list 'load-path (expand-file-name "C:/Emacs-21.1/site-lisp/psgml-1.2.5"))
;;(add-to-list 'load-path (expand-file-name "C:/Emacs-21.1/site-lisp/nxml-mode-20031031"))

;(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
;(autoload 'xml-mode "psgml" "Major mode to edit XML files." t)

;; NXML-Mode File...
;;(load "rng-auto.el")

;; Disable caps lock in emacs...
(setq w32-enable-caps-lock nil)
(setq w32-apps-modifier 'super)
(setq w32-enable-num-lock nil)

(setq w32-pass-lwindow-to-system nil)
(setq w32-pass-rwindow-to-system nil)

(setq w32-lwindow-modifier 'hyper)
(setq w32-rwindow-modifier 'hyper)

;;(csharp-mode-0.8.5.el)
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
(append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
;;(require 'csharp-mode)

;;(setq auto-mode-alist
;;      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
;;   (defun my-csharp-mode-fn ()
;;      "function that runs when csharp-mode is initialized for a buffer."
;;      ...insert your code here...
;;      ...most commonly, your custom key bindings ...
;;   )
;;   (add-hook  'csharp-mode-hook 'my-csharp-mode-fn t)
