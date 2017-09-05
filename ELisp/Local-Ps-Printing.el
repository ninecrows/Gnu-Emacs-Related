;;-------------------------------------------------------------------;;
;;  Copyright (c) 2002 Kyle Wilson. All Rights Reserved.
;;  Time-stamp:    <2002-05-13 11:08:19 jameskw>
;;  Part-of:       Emacs Initialization Files
;;  File Name:     "Home/ELisp/Local-Ps-Printing.el"
;;  Date(s):       Monday May 13, 2002
;;  Author(s):     Kyle Wilson
;;-------------------------------------------------------------------;;
;; Various defuns that setup postscript printing in useful ways...
;;-------------------------------------------------------------------;;
;; Revision Information:
;;   (jkw) [Monday May 13, 2002] Initial Entry
;;-------------------------------------------------------------------;;

(defun ps-two-up-printing () "Setup for two-up postscript printing"
  (interactive)
  (setq ps-landscape-mode t)
  (setq ps-number-of-columns 2)
  (setq ps-font-size 7)
  (setq ps-header-font-size 8)
  (setq ps-zebra-stripes t)
  (setq ps-top-margin 18)
  (setq ps-left-margin 36)
  (setq ps-inter-column 18)
  (setq ps-right-margin 36))

(defun ps-one-up-printing () "Setup for two-up postscript printing"
  (interactive)
  (setq ps-landscape-mode nil)
  (setq ps-number-of-columns 1)
  (setq ps-font-size 10)                ; was 11
  (setq ps-header-font-size 11)
  (setq ps-zebra-stripes t)
  (setq ps-header-offset 5)            ; Was 28.3
  (setq ps-top-margin 35) ;; Was 18
  (setq ps-bottom-margin 50) ;; was 42.5
  (setq ps-left-margin 45)
  (setq ps-inter-column 18)
  (setq ps-right-margin 36)
  t)

(defun ps-one-up-short-printing () "Setup for two-up postscript printing"
  (interactive)
  (setq ps-landscape-mode nil)
  (setq ps-number-of-columns 1)
  (setq ps-font-size 11) 
  (setq ps-header-font-size 12)
  (setq ps-zebra-stripes t)
  (setq ps-header-offset 28)
  (setq ps-top-margin 18) 
  (setq ps-bottom-margin 42)
  (setq ps-left-margin 45)
  (setq ps-inter-column 18)
  (setq ps-right-margin 36))

(defun ps-duplex-mode () "Set the printer to print in duplex mode"
  (interactive)
  (setq ps-adobe-tag "%!PS-Adobe-3.0\n[{\n%%BeginFeature: *Duplex DuplexNoTumble\n1 dict dup /Duplex true put setpagedevice\n 1 dict dup /Tumble false put setpagedevice\n %%EndFeature\n } stopped cleartomark\n\n[{\n%%BeginFeature: *MediaType Drilled\n<< /MediaType (Drilled) >> setpagedevice\n%%EndFeature\n} stopped cleartomark\n")
  (setq ps-spool-duplex t)
  )

(defun ps-stapled-duplex-mode () "Set the printer to print in duplex mode"
  (interactive)
  (setq ps-adobe-tag 
        (concat "%!PS-Adobe-3.0\n"

                "%%BeginSetup\n"

                "[{\n"
                "%%BeginFeature: *Duplex DuplexNoTumble\n"
                "<< /Duplex true\n"
                "   /Tumble false >> setpagedevice\n"
                "%%EndFeature\n"
                "} stopped cleartomark\n\n"

                ;;"[{\n"
                ;;"%%BeginFeature: *MediaType Drilled\n"
                ;;"<< /MediaType (Drilled) >> setpagedevice\n"
                ;;"%%EndFeature\n"
                ;;"} stopped cleartomark\n"

                "[{\n"
                "%%BeginFeature: *OutputBin Stacker\n"
                "<< /OutputType (Stacker) >> setpagedevice\n"
                "%%EndFeature\n"
                "} stopped cleartomark\n"

                "[{\n"
                "%%BeginFeature: *KOCollate True\n"
                "<< /Collate true >> setpagedevice\n"
                "%%EndFeature\n"
                "} stopped cleartomark\n"

                "[{\n"
                "%%BeginFeature: *Jog None\n"
                "<< /Jog 0 >> setpagedevice\n"
                "%%EndFeature\n"
                "} stopped cleartomark\n"

                "[{\n"
                "%%BeginFeature: *OutputBin None\n"
                "<</OutputType (DEFAULT)>> setpagedevice\n"
                "%%EndFeature\n"
                "} stopped cleartomark\n"

                "[{\n"
                "%%BeginFeature: *MediaType Normal\n"
                "\n"
                "<< /MediaType (PLAIN) /OHPInterleave (Off) >>\n"
                "/KonicaOptions /ProcSet findresource "
                "/setkonicaoptions get exec\n"
                "%%EndFeature\n"
                "} stopped cleartomark\n"

                "[{\n"
                ;;"%%BeginFeature: *StapleLocation SinglePortrait\n"
                "%%BeginFeature: *StapleLocation DualPortrait\n"
                ;;"\n"
                ;;"<< /StapleMode (SinglePortrait) >>\n"
                "<< /StapleMode (DoubleSidePortrait) >> "
                ;;"/KonicaOptions /ProcSet findresource "
               ;; "/setkonicaoptions get exec\n"
                "%%EndFeature\n"
                "} stopped cleartomark\n\n"

                ;;"[{\n"
                ;;"%%BeginFeature: *StapleLocation DualPortrait\n"
                ;;"<< /StapleMode (DoubleSidePortrait) >> "
                ;;"setpagedevice\n"
                ;;"/KonicaOptions /ProcSet findresource /setkonicaoptions get exec\n"
                ;;"%%BeginFeature: *StapleWhen EndOfSet\n"
                ;;"<< /Staple 3 >> setpagedevice\n"
                ;;"%%EndFeature\n"
                ;;"} stopped cleartomark\n"

                "%%EndSetup\n"
                ))
  (setq ps-spool-duplex t)
  )

(defun ps-simplex-mode () "Set the printer back into simplex mode"
  (interactive)
  (setq ps-adobe-tag "%!PS-Adobe-3.0\n")
  (setq ps-spool-duplex nil)
  )

(defun ps-ref-docs () "Setup for printing reference documents"
  (interactive)
  (ps-one-up-printing)
  (ps-duplex-mode)
  (setq ps-line-number t)
  (message "One up duplex with line numbers"))

(defun ps-code-docs () "Setup for printing code documents"
  (interactive)
  (ps-two-up-printing)
  (ps-duplex-mode)
  (setq ps-line-number t)
  (message "Two up duplex with line numbers"))

(defun ps-1-code-docs () "Setup for printing code documents"
  (interactive)
  (ps-two-up-printing)
  (ps-duplex-mode)
  (setq ps-line-number t)
  (message "Two up simplex with line numbers"))

