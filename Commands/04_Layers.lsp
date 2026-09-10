;;; ==========================================================================
;;; LAYERS.LSP - Lightning-Fast Layer Management Shortcuts & Utilities
;;; ==========================================================================
;;; Category : Layer Operations
;;; Author   : Haseeb
;;; ==========================================================================

(vl-load-com)

;;; --------------------------------------------------------------------------
;;; 1. LAYER ISOLATION & GLOBAL RESTORATION
;;; --------------------------------------------------------------------------
;;; Note: Direct single-line layer aliases (11, LO, 44, LON, 55, LF, 66, LTH,
;;; LAYC, LM, LLK, LUK) are centralized in Commands/Aliases-Min.lsp.

;; 22 or LI : Layer Isolate (Viewport-safe isolation)
(defun c:22 (/ *error* oldCmd)
  (setq oldCmd (getvar "CMDECHO"))
  (defun *error* (msg)
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (princ)
  )
  (setvar "CMDECHO" 0)
  (command "_.LAYISO")
  (setvar "CMDECHO" oldCmd)
  (princ)
)
(defun c:LI () (c:22))

;; 33 or LU : Layer Unisolate (Restore isolated layers)
(defun c:33 (/ *error* oldCmd)
  (setq oldCmd (getvar "CMDECHO"))
  (defun *error* (msg)
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (princ)
  )
  (setvar "CMDECHO" 0)
  (command "_.LAYUNISO")
  (setvar "CMDECHO" oldCmd)
  (princ)
)
(defun c:LU () (c:33))

;; LALL : Thaw ALL layers and turn ALL layers ON in one step
(defun c:LALL (/ *error* oldCmd)
  (setq oldCmd (getvar "CMDECHO"))
  (defun *error* (msg)
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (princ)
  )
  (setvar "CMDECHO" 0)
  (command "_.LAYER" "_thaw" "*" "_on" "*" "")
  (setvar "CMDECHO" oldCmd)
  (princ "\n[LALL] All layers thawed and turned ON.")
  (princ)
)

;;; --------------------------------------------------------------------------
;;; 2. LAYER SELECTION & STATE TOOLS
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

(princ "\n[04_Layers.lsp] Quick layer control commands loaded.")
(princ)
