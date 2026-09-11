;;; ==========================================================================
;;; 00_Init.lsp - Core Environment Initialization & System Path Discovery
;;; Part of Cad-Setup-v3 Horizontal Layered Architecture
;;; Layer: Core (Level 0)
;;; ==========================================================================

(vl-load-com)

;; ===========================================================================
;; GLOBAL PROJECT DIRECTORY CONFIGURATION & PATH RESOLUTION
;; ===========================================================================

;; Configurable fallback path for manual overriding:
(if (not (boundp '*CAD-SETUP-DIR*))
  (setq *CAD-SETUP-DIR* "D:\\Cad-Setup-v3")
)

;; CadSetup:GetDir
;; Resolves the root directory of the Cad-Setup system in order of precedence:
;; 1. Validated user global *CAD-SETUP-DIR*
;; 2. Directory containing CAD-SETUP-AUTORUN.lsp
;; 3. Directory containing this Core/00_Init.lsp file (ascends one level)
;; 4. Default fallback: "D:\\Cad-Setup-v3"
(defun CadSetup:GetDir ( / p candidate )
  (cond
    ;; 1. Validated global path
    ((and (boundp '*CAD-SETUP-DIR*)
          *CAD-SETUP-DIR*
          (= (type *CAD-SETUP-DIR*) 'STR)
          (setq candidate (vl-string-right-trim "\\/" *CAD-SETUP-DIR*))
          (vl-file-directory-p candidate))
     candidate)

    ;; 2. Look for master CAD-SETUP-AUTORUN.lsp anywhere on AutoCAD support search path
    ((and (setq p (findfile "CAD-SETUP-AUTORUN.lsp"))
          (setq p (vl-filename-directory p))
          (vl-file-directory-p p))
     (vl-string-right-trim "\\/" p))

    ;; 3. Check relative location of 00_Init.lsp (parent directory)
    ((and (setq p (findfile "00_Init.lsp"))
          (setq p (vl-filename-directory p))            ; .../Core
          (setq p (vl-filename-directory p))            ; .../Cad-Setup-v3
          (vl-file-directory-p p))
     (vl-string-right-trim "\\/" p))

    ;; 4. Hardcoded safe default
    (t "D:\\Cad-Setup-v3")
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
