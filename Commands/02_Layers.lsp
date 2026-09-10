;;; ==========================================================================
;;; 02_Layers.lsp - Lightning-Fast Layer Management Shortcuts & Utilities
;;; ==========================================================================
;;; Category : Layer Operations
;;; Author   : Haseeb
;;; ==========================================================================

(vl-load-com)

;;; --------------------------------------------------------------------------
;;; LAYER SELECTION & STATE TOOLS
;;; --------------------------------------------------------------------------

;; L0 : Switch active layer to "0" immediately
(defun c:L0 ()
  (setvar "CLAYER" "0")
  (princ "\n[L0] Current Layer is now: \"0\"")
  (princ)
)

;; LC : Set Current Layer by selecting an object
(defun c:LC (/ ent lay)
  (setq ent (car (entsel "\nSelect an object to make its layer CURRENT: ")))
  (if ent
    (progn
      (setq lay (cdr (assoc 8 (entget ent))))
      (setvar 'clayer lay)
      (princ (strcat "\n[LC] Current Layer is now: \"" lay "\""))
    )
    (princ "\n[LC] No object selected.")
  )
  (princ)
)

(princ "\n[02_Layers.lsp] Quick layer control commands loaded.")
(princ)
