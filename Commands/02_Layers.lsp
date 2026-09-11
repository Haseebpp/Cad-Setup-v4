;;; ==========================================================================
;;; 02_Layers.lsp - Lightning-Fast Layer Management Shortcuts & Utilities
;;; Layer: Commands (Priority 02)
;;; Author   : Haseeb
;;; ==========================================================================

(vl-load-com)

;;; --------------------------------------------------------------------------
;;; LAYER SELECTION & STATE TOOLS
;;; --------------------------------------------------------------------------

;; L0 : Switch active layer to "0" immediately
(defun c:L0 ()
  (CadSetup:SetCurrentLayerSafe "0")
  (princ "\n[L0] Current Layer is now: \"0\"")
  (princ)
)

(princ "\n[02_Layers.lsp] Quick layer control commands loaded.")
(princ)
