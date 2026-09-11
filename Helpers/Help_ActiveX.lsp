;;; ==========================================================================
;;; Help_ActiveX.lsp - Safe COM/ActiveX Wrappers & System Variable Utilities
;;; Part of Cad-Setup-v3 Horizontal Layered Architecture
;;; Layer: Helpers (Level 1)
;;; ==========================================================================

(vl-load-com)

;; ===========================================================================
;; COM / VLA OBJECT GETTERS
;; ===========================================================================

;; CadSetup:GetAcad - Safely gets the top-level AutoCAD application COM object
(defun CadSetup:GetAcad ( / acad )
  (setq acad (vlax-get-acad-object))
  (if (and acad (= (type acad) 'VLA-OBJECT))
    acad
    nil
  )
)

;; CadSetup:GetDoc - Safely gets the ActiveDocument object
(defun CadSetup:GetDoc ( / acad doc )
  (setq acad (CadSetup:GetAcad))
  (if acad
    (progn
      (setq doc (vl-catch-all-apply 'vla-get-activedocument (list acad)))
      (if (and (not (vl-catch-all-error-p doc)) (= (type doc) 'VLA-OBJECT))
        doc
        nil
      )
    )
    nil
  )
)

;; CadSetup:GetModelSpace - Safely returns the ModelSpace collection
(defun CadSetup:GetModelSpace ( / doc ms )
  (setq doc (CadSetup:GetDoc))
  (if doc
    (progn
      (setq ms (vl-catch-all-apply 'vla-get-modelspace (list doc)))
      (if (and (not (vl-catch-all-error-p ms)) (= (type ms) 'VLA-OBJECT))
        ms
        nil
      )
    )
    nil
  )
)

;; CadSetup:GetPaperSpace - Safely returns the PaperSpace collection of the active layout
(defun CadSetup:GetPaperSpace ( / doc ps )
  (setq doc (CadSetup:GetDoc))
  (if doc
    (progn
      (setq ps (vl-catch-all-apply 'vla-get-paperspace (list doc)))
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
