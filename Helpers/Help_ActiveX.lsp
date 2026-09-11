;;; ==========================================================================
;;; Help_ActiveX.lsp - Safe COM/ActiveX Wrappers & System Variable Utilities
;;; Layer: Helpers (Level 1)
;;; ==========================================================================

(vl-load-com)

;; ===========================================================================
;; COM / VLA OBJECT GETTERS
;; ===========================================================================

;; CadSetup:GetAcad - Safely gets the top-level AutoCAD application COM object
(defun CadSetup:GetAcad ( / acadApp )
  (setq acadApp (vlax-get-acad-object))
  (if (and acadApp (= (type acadApp) 'VLA-OBJECT))
    acadApp
    nil
  )
)

;; CadSetup:GetDoc - Safely gets the ActiveDocument object
(defun CadSetup:GetDoc ( / acadApp acadDoc )
  (setq acadApp (CadSetup:GetAcad))
  (if acadApp
    (progn
      (setq acadDoc (vl-catch-all-apply 'vla-get-activedocument (list acadApp)))
      (if (and (not (vl-catch-all-error-p acadDoc)) (= (type acadDoc) 'VLA-OBJECT))
        acadDoc
        nil
      )
    )
    nil
  )
)

;; CadSetup:GetModelSpace - Safely returns the ModelSpace collection
(defun CadSetup:GetModelSpace ( / acadDoc ms )
  (setq acadDoc (CadSetup:GetDoc))
  (if acadDoc
    (progn
      (setq ms (vl-catch-all-apply 'vla-get-modelspace (list acadDoc)))
      (if (and (not (vl-catch-all-error-p ms)) (= (type ms) 'VLA-OBJECT))
        ms
        nil
      )
    )
    nil
  )
)

;; CadSetup:GetPaperSpace - Safely returns the PaperSpace collection of the active layout
(defun CadSetup:GetPaperSpace ( / acadDoc ps )
  (setq acadDoc (CadSetup:GetDoc))
  (if acadDoc
    (progn
      (setq ps (vl-catch-all-apply 'vla-get-paperspace (list acadDoc)))
      (if (and (not (vl-catch-all-error-p ps)) (= (type ps) 'VLA-OBJECT))
        ps
        nil
      )
    )
    nil
  )
)

;; ===========================================================================
;; SAFE SYSTEM VARIABLE WRAPPERS
;; ===========================================================================

;; CadSetup:SafeGetVar - Reads a sysvar, returns default value if undefined or error
(defun CadSetup:SafeGetVar (varName defaultVal / val)
  (if (null varName)
    defaultVal
    (progn
      (setq val (vl-catch-all-apply 'getvar (list varName)))
      (if (or (vl-catch-all-error-p val) (null val))
        defaultVal
        val
      )
    )
  )
)

;; CadSetup:SafeSetVar - Sets a sysvar only if it differs from current value
;; Returns T on successful set, nil if unchanged or failed
(defun CadSetup:SafeSetVar (varName newVal / curVal res)
  (if (and varName newVal)
    (progn
      (setq curVal (CadSetup:SafeGetVar varName nil))
      (if (not (equal curVal newVal))
        (progn
          (setq res (vl-catch-all-apply 'setvar (list varName newVal)))
          (not (vl-catch-all-error-p res))
        )
        T ; already equal
      )
    )
    nil
  )
)

;; ===========================================================================
;; NUMERIC & RANGE HELPERS
;; ===========================================================================

;; CadSetup:Clamp - Constrains a numeric value between min-val and max-val
(defun CadSetup:Clamp (val min-val max-val)
  (max min-val (min max-val val))
)

(princ "\n[Helpers/Help_ActiveX.lsp] Safe ActiveX & SysVar helpers loaded.")
(princ)
