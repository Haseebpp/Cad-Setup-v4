;;; ==========================================================================
;;; 03_Blocks.lsp - Auto Block Generators & In-Place Fast Block Transforms
;;; Part of Cad-Setup-v3 Horizontal Layered Architecture
;;; Layer: Commands (Priority 03)
;;; Author   : Haseeb
;;; ==========================================================================

(vl-load-com)

;;; --------------------------------------------------------------------------
;;; 1. AUTO BLOCK CREATION (WITH TIMESTAMP & SMART BASE POINT)
;;; --------------------------------------------------------------------------

;; CB / G : Auto-create block with timestamp name and Bottom-Left base point
(defun c:CB (/ *error* ss bbox blkName baseName count basePt oldCmd oldAtt)
  (defun *error* (msg)
    (if oldAtt (setvar 'attreq oldAtt))
    (if oldCmd (setvar 'cmdecho oldCmd))
    (CadSetup:UndoReset)
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[CB] Error: " msg))
    )
    (princ)
  )

  (setq oldCmd (getvar 'cmdecho)
        oldAtt (getvar 'attreq))
  (setvar 'cmdecho 0)
  (setvar 'attreq 0)

  (setq ss (ssget "_I"))
  (if (not ss)
    (progn
      (princ "\nSelect objects to convert into Block: ")
      (setq ss (ssget))
    )
  )

  (if ss
    (progn
      (CadSetup:UndoStart)

      ;; Compute collective bottom-left bounding coordinate in CURRENT UCS using geometry helper
      (setq bbox (CadSetup:GetBoundingBoxUcs ss))
      (if bbox
        (setq basePt (car bbox))
        (setq basePt (getvar 'insbase))
      )

      ;; Generate unique timestamp name with collision guard
      (setq blkName (strcat "BLK_" (menucmd "M=$(edtime,$(getvar,date),YYYYMODD_HHMMSS)")))
      (if (or (null blkName) (= blkName "BLK_"))
        (setq blkName (strcat "BLK_" (rtos (getvar "CDATE") 2 6)))
      )
      (if (tblsearch "BLOCK" blkName)
        (progn
          (setq baseName blkName
                count 1)
          (while (tblsearch "BLOCK" (strcat baseName "_" (itoa count)))
            (setq count (1+ count))
          )
          (setq blkName (strcat baseName "_" (itoa count)))
        )
      )

      ;; Create block and re-insert in place using UCS basePt
      (command "._-block" blkName "_non" basePt ss "")
      (command "._-insert" blkName "_non" basePt 1.0 1.0 0.0)

      (CadSetup:UndoEnd)
      (princ (strcat "\n[CB] Block created: \"" blkName "\" at Bottom-Left base point."))
    )
    (princ "\n[CB] No objects selected.")
  )

  (setvar 'attreq oldAtt)
  (setvar 'cmdecho oldCmd)
  (princ)
)

;; Alias: G for Quick Block
(defun c:G () (c:CB) (princ))

;; OB : Auto-create block with timestamp name at Origin (0,0,0)
(defun c:OB (/ *error* ss blkName baseName count basePt oldCmd oldAtt)
  (defun *error* (msg)
    (if oldAtt (setvar 'attreq oldAtt))
    (if oldCmd (setvar 'cmdecho oldCmd))
    (CadSetup:UndoReset)
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[OB] Error: " msg))
    )
    (princ)
  )

  (setq oldCmd (getvar 'cmdecho)
        oldAtt (getvar 'attreq))
  (setvar 'cmdecho 0)
  (setvar 'attreq 0)

  (setq ss (ssget "_I"))
  (if (not ss)
    (progn
      (princ "\nSelect objects to convert into Block at Origin: ")
      (setq ss (ssget))
    )
  )

  (if ss
    (progn
      (CadSetup:UndoStart)
      (setq basePt '(0.0 0.0 0.0))
      (setq blkName (strcat "BLK_" (menucmd "M=$(edtime,$(getvar,date),YYYYMODD_HHMMSS)")))
      (if (or (null blkName) (= blkName "BLK_"))
        (setq blkName (strcat "BLK_" (rtos (getvar "CDATE") 2 6)))
      )
      (if (tblsearch "BLOCK" blkName)
        (progn
          (setq baseName blkName
                count 1)
          (while (tblsearch "BLOCK" (strcat baseName "_" (itoa count)))
            (setq count (1+ count))
          )
          (setq blkName (strcat baseName "_" (itoa count)))
        )
      )

      (command "._-block" blkName "_non" basePt ss "")
      (command "._-insert" blkName "_non" basePt 1.0 1.0 0.0)

      (CadSetup:UndoEnd)
      (princ (strcat "\n[OB] Block created: \"" blkName "\" at Origin (0,0,0)."))
    )
    (princ "\n[OB] No objects selected.")
  )

  (setvar 'attreq oldAtt)
  (setvar 'cmdecho oldCmd)
  (princ)
)


;;; --------------------------------------------------------------------------
;;; 2. FAST IN-PLACE BLOCK TRANSFORMS (RB, RBH, RBV)
;;; --------------------------------------------------------------------------

;; Helper: Extract Bounding Box Center using Geometry Helpers
(defun _FastMidPt (obj / bbox)
  (setq bbox (CadSetup:GetBoundingBox obj))
  (if bbox
    (CadSetup:MidPoint (car bbox) (cadr bbox))
    nil
  )
)

;; Master Transform Engine
(defun _ExecTransform (mode / *error* echo ss i ent obj mid ucsMid pt2 new-obj)
  (defun *error* (msg)
    (if echo (setvar 'CMDECHO echo))
    (CadSetup:UndoReset)
    (if (and msg (not (wcmatch (strcase msg t) "*break*,*cancel*,*exit*,*quit*")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  (setq echo (getvar 'CMDECHO))
  (setvar 'CMDECHO 0)

  (CadSetup:UndoStart)

  ;; "_:L" filter rejects locked layers instantly at selection time
  (if (setq ss (ssget "_:L" '((0 . "INSERT"))))
    (repeat (setq i (sslength ss))
      (setq ent (ssname ss (setq i (1- i)))
            obj (vlax-ename->vla-object ent))
      
      (if (setq mid (_FastMidPt obj))
        (cond
          ;; 90-Degree Clockwise Rotation around center
          ((eq mode 'ROTATE)
           (vla-rotate obj (vlax-3d-point mid) (- (/ pi 2.0)))
          )

          ;; Flip Horizontal (Mirror across vertical midpoint axis in current UCS)
          ((eq mode 'FLIP_H)
           (setq ucsMid (trans mid 0 1))
           (setq pt2 (trans (list (car ucsMid) (+ (cadr ucsMid) 100.0) (caddr ucsMid)) 1 0))
           (setq new-obj (vl-catch-all-apply 'vla-mirror (list obj (vlax-3d-point mid) (vlax-3d-point pt2))))
           (if (not (vl-catch-all-error-p new-obj))
             (vla-delete obj)
           )
          )

          ;; Flip Vertical (Mirror across horizontal midpoint axis in current UCS)
          ((eq mode 'FLIP_V)
           (setq ucsMid (trans mid 0 1))
           (setq pt2 (trans (list (+ (car ucsMid) 100.0) (cadr ucsMid) (caddr ucsMid)) 1 0))
           (setq new-obj (vl-catch-all-apply 'vla-mirror (list obj (vlax-3d-point mid) (vlax-3d-point pt2))))
           (if (not (vl-catch-all-error-p new-obj))
             (vla-delete obj)
           )
          )
        )
      )
    )
  )

  (CadSetup:UndoEnd)
  (setvar 'CMDECHO echo)
  (princ)
)

;; Command Shortcuts
(defun c:RB  () (_ExecTransform 'ROTATE))
(defun c:RBH () (_ExecTransform 'FLIP_H))
(defun c:RBV () (_ExecTransform 'FLIP_V))

(princ "\n[03_Blocks.lsp] Block creation and transform tools loaded (CB, OB, RB, RBH, RBV).")
(princ)
