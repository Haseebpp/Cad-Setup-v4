;;; ==========================================================================
;;; 00_Init.lsp - Core Environment Initialization & System Path Discovery
;;; Layer: Core (Level 0)
;;; ==========================================================================

(vl-load-com)

;; ===========================================================================
;; SYSTEM ROOT DIRECTORY RESOLUTION (AUTORUN.LSP VIA APPLOAD)
;; ===========================================================================

;; CadSetup:GetDir
;; Resolves the system root directory strictly from the location of 'Autorun.lsp'
;; (manually added via AutoCAD APPLOAD / Startup Suite).
;; Prints an error message to the console if resolution fails.
(defun CadSetup:GetDir ( / p )
  (if (and (setq p (findfile "Autorun.lsp"))
           (setq p (vl-filename-directory p))
           (vl-file-directory-p p))
    (vl-string-right-trim "\\/" p)
    (progn
      (princ "\n[CadSetup ✖ Error]: System path resolution failed. Could not locate 'Autorun.lsp'.")
      (princ "\nPlease add 'Autorun.lsp' to AutoCAD APPLOAD (Startup Suite) or Support File Search Paths.")
      nil
    )
  )
)


;; ===========================================================================
;; CONSOLE LOGGING & DIAGNOSTIC UTILITIES
;; ===========================================================================

(defun CadSetup:Log (msg)
  (if msg (princ (strcat "\n[CadSetup] " msg)))
  (princ)
)

(defun CadSetup:LogSuccess (msg)
  (if msg (princ (strcat "\n[CadSetup ✓] " msg)))
  (princ)
)

(defun CadSetup:LogWarn (msg)
  (if msg (princ (strcat "\n[CadSetup ⚠ Warning]: " msg)))
  (princ)
)

(defun CadSetup:LogError (msg)
  (if msg (princ (strcat "\n[CadSetup ✖ Error]: " msg)))
  (princ)
)

;; ===========================================================================
;; ENVIRONMENT & CAPABILITY CHECKS
;; ===========================================================================

(defun CadSetup:CheckEnvironment ( / acadApp doc )
  (setq acadApp (vlax-get-acad-object))
  (if (null acadApp)
    (progn
      (CadSetup:LogError "AutoCAD COM automation server could not be reached.")
      nil
    )
    (progn
      (setq doc (vla-get-activedocument acadApp))
      (if (null doc)
        (progn
          (CadSetup:LogWarn "No active drawing document open.")
          nil
        )
        T
      )
    )
  )
)

(princ "\n[Core/00_Init.lsp] Core environment & path resolver initialized.")
(princ)
