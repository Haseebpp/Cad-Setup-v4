;;; ==========================================================================
;;; 00_Init.lsp - Core Environment Initialization & Logging Utilities
;;; Layer: Core (Level 0)
;;; ==========================================================================

(vl-load-com)


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

(princ "\n[Core/00_Init.lsp] Core environment & diagnostic utilities initialized.")
(princ)
