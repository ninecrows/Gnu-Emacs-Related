;;-------------------------------------------------------------------;;
;;  Copyright (c) 2002 Kyle Wilson. All Rights Reserved.
;;  Time-stamp:    <2002-05-13 10:59:10 jameskw>
;;  Part-of:       Emacs Initialization Files
;;  File Name:     "Home/ELisp/Custom-Compile-Commands.el"
;;  Date(s):       Monday May 13, 2002
;;  Author(s):     Kyle Wilson
;;-------------------------------------------------------------------;;
;; This file defines a bunch of small interactive commands for running
;; JKW system component compiles.
;;-------------------------------------------------------------------;;
;; Revision Information:
;;   (jkw) [Monday May 13, 2002] Initial Entry
;;-------------------------------------------------------------------;;

(defun tcr-compile () "Quick compile  library"
  (interactive)
  (compile "msdev .dsp /make \" - Win32 Debug\" /rebuild"))

(defun tsr-compile () "Quick compile "
  (interactive)
  (compile "msdev .dsp /make \" - Win32 Debug\" /rebuild"))

(defun tc-compile () "Quick compile  library"
  (interactive)
  (compile "msdev .dsp /make \" - Win32 Debug\""))

(defun ts-compile () "Quick compile "
  (interactive)
  (compile "msdev .dsp /make \" - Win32 Debug\""))

(defun tdm-compile () "Quick compile  library"
  (interactive)
  (compile "msdev .dsp /make \" - Win32 Debug\""))

(defun tdmr-compile () "Quick compile  library"
  (interactive)
  (compile "msdev .dsp /make \" - Win32 Debug\" /rebuild"))


(defun tem-compile () "Quick compile  library"
  (interactive)
  (compile "msdev .dsp /make \" - Win32 Debug\""))

(defun temr-compile () "Quick compile  library"
  (interactive)
  (compile "msdev .dsp /make \" - Win32 Debug\" /rebuild"))

(defun cbbrx-compile () "Quick compile  library"
  (interactive)
  (compile "msdev .dsp /make \" - Win32 Debug\""))

(defun nmsdev-compile () "Compile a developer studio project"
  (interactive)
  (let ((dspname (jkw-dsp-file-name)) (prjname (jkw-get-dsp-project-name)))
    (compile 
     (concat "msdev " dspname " /make \"" prjname " - Win32 Debug\""))
    ))

(defun msdev-compile () "Compile a developer studio project"
  (interactive)

  (let ((dspname (jkw-dsp-file-name)) (prjname (jkw-get-dsp-project-name)))
    (compile 
     (concat "msdev " dspname " /make \"" prjname " - Win32 Debug\" /rebuild"))
    ))

;;===================================================================;;
;; (jkw) [Tuesday May 14, 2002]
;;===================================================================;;
;; Code to extract relevant information from dsp files.
;;===================================================================;;

(defun jkw-get-dsp-project-name ()
  "Retrieve the project name from a local *.dsp file"
  (interactive)

  (jkw-get-dsp-eval
   (indirect-function 'jkw-extract-dsp-project-name))
  )

(defun jkw-extract-dsp-project-name () ""
  (goto-char (point-min))
  ;;(message "Ping-pong")
  (if (looking-at (concat "# Microsoft Developer Studio Project File"
                          " - Name=\"\\([^\"]+\\)\""))
      (match-string 1)
    )
  )

(defun jkw-get-dsp-eval (funtorun)
  "Look in the current folder for a *.dsp file and load it into a buffer"
  (let ((here (file-name-directory (buffer-file-name)))
        (dsp-files))
    (setq dsp-files (directory-files here nil "\\.dsp$"))

    (let ((thisone (car dsp-files)))

      (with-temp-buffer
        (insert-file-contents thisone)
        (goto-char (point-min))
        (funcall funtorun)
        )
      )
    )
  )

(defun jkw-dsp-file-name ()
  "Get the name of the first *.dsp file in this folder"
  
  (let* ((here (file-name-directory (buffer-file-name)))
        (dsp-files (directory-files here nil "\\.dsp$")))
    (car dsp-files)
    ))