;;; ==========================================================================
;;; 00_Init.lsp - Core Environment Initialization & Logging Utilities
;;; Layer: Core (Level 0)
;;; ==========================================================================

(vl-load-com)


;; ===========================================================================
;; CONSOLE LOGGING & DIAGNOSTIC UTILITIES
;; ===========================================================================

;; Global diagnostic verbosity switch (default: quiet/silent)
(if (null *CadSetup-Debug*)
  (setq *CadSetup-Debug* nil)
)

(defun CadSetup:Log (msg)
  (if msg (princ (strcat "\n[CadSetup] " msg)))
  (princ)
)

(defun CadSetup:LogSuccess (msg)
  (if msg (princ (strcat "\n[CadSetup OK] " msg)))
  (princ)
)

(defun CadSetup:LogWarn (msg)
  (if msg (princ (strcat "\n[CadSetup WARN]: " msg)))
  (princ)
)

(defun CadSetup:LogError (msg)
  (if msg (princ (strcat "\n[CadSetup ERR]: " msg)))
  (princ)
)

(defun CadSetup:LogDebug (msg)
  (if (and *CadSetup-Debug* msg)
    (princ (strcat "\n[CadSetup DEBUG] " msg))
  )
  (princ)
)

;; Command to toggle diagnostic verbosity
(defun c:CAD-SETUP-DEBUG ()
  (setq *CadSetup-Debug* (not *CadSetup-Debug*))
  (if *CadSetup-Debug*
    (princ "\n[CadSetup DEBUG]: Diagnostic mode enabled (verbose logging ON).")
    (princ "\n[CadSetup DEBUG]: Diagnostic mode disabled (quiet logging ON).")
  )
  (princ)
)
(defun c:CS-DEBUG () (c:CAD-SETUP-DEBUG))

;; ===========================================================================
;; ENVIRONMENT & CAPABILITY CHECKS
;; ===========================================================================

;; AutoLISP compatibility utility for symbolp and fboundp
(defun symbolp (sym)
  (= (type sym) 'SYM)
)

(defun fboundp (sym)
  (and (= (type sym) 'SYM)
       (boundp sym)
       (member (type (vl-symbol-value sym)) '(SUBR USUBR EXRXSUBR)))
)

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

(if *CadSetup-Debug*
  (princ "\n[Core/00_Init.lsp] Core environment & diagnostic utilities initialized.")
)
(princ)
