;;-------------------------------------------------------------------;;
;;  Copyright (c) 2002 Kyle Wilson. All Rights Reserved.
;;  Time-stamp:    <2002-05-13 11:16:39 jameskw>
;;  Part-of:       Emacs Initialization Files
;;  File Name:     "Home/ELisp/Local-Set-Extra-Registers.el"
;;  Date(s):       Monday May 13, 2002
;;  Author(s):     Kyle Wilson
;;-------------------------------------------------------------------;;
;; 
;;-------------------------------------------------------------------;;
;; Revision Information:
;;   (jkw) [Monday May 13, 2002] Initial Entry
;;-------------------------------------------------------------------;;

;;-------------------------------------------------------------------;;
;; (jkw) [Wednesday March 06, 2002]
;;-------------------------------------------------------------------;;
;; Setup extra text registers with useful bits of C++ code.
;;-------------------------------------------------------------------;;
;; Arguments:
;;     None.
;;-------------------------------------------------------------------;;

(defun set-extra-registers ()
  "Setup registers with interesting contents related to COM stuff"
  (interactive)

  (set-register ?h "    HRESULT hr(S_OK);\n")
  (set-register ?s "    if (SUCCEEDED(hr))\n")
  (set-register ?r "    return (hr);\n")

  (set-register ?n "namespace GrooveEisSandbox\n{\n")
  (set-register ?t "    GESFunc\n") ;
  (set-register ?N (concat
                    "//===================================================================//\n"
                    "// (jkw) [Tuesday December 30, 2003]\n"
                    "//===================================================================//\n"
                    "// Provide a short and easy to use alias for the real namespace name.  This may\n"
                    "// be disabled if needed to work around a conflict by defining\n"
                    "// GROOVE_EIS_DISABLE_SHORT_NAMESPACES.\n"
                    "//===================================================================//\n"
                    "\n"
                    "#if !defined(GROOVE_EIS_DISABLE_SHORT_NAMESPACES)\n"
                    "namespace ges = GrooveEisSandbox;\n"
                    "#endif\n"
                    "\n"))
  )
