;;; ==========================================================================
;;; 06_Utilities.lsp - Drawing Cleanup, Measurement, System Fixes & Utilities
;;; Part of Cad-Setup-v3 Horizontal Layered Architecture
;;; Layer: Commands (Priority 06)
;;; Author   : Haseeb
;;; ==========================================================================

(vl-load-com)

;;; --------------------------------------------------------------------------
;;; 1. MEASUREMENT & CALCULATION
;;; --------------------------------------------------------------------------

;; TC : Total Curve & Length Calculator (Lines, Polylines, Arcs, Splines, Circles)
;; Powered by CadSetup:GetTotalCurveLength from Helpers/Help_Geometry.lsp
(defun c:TC (/ ss total)
  (setq ss (ssget "_I"))
  (if (not ss)
    (progn
      (princ "\nSelect lines, polylines, arcs, circles, or splines for total length: ")
      (setq ss (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE,ARC,CIRCLE,SPLINE,ELLIPSE"))))
    )
  )
  (if ss
    (progn
      (setq total (CadSetup:GetTotalCurveLength ss))
      (princ (strcat "\n[TC] Total Length of " (itoa (sslength ss)) " object(s) = " (rtos total 2 4)))
    )
    (princ "\n[TC] No objects selected.")
  )
  (princ)
)


;;; --------------------------------------------------------------------------
;;; 2. SMART DUPLICATION & BOUNDING BOX
;;; --------------------------------------------------------------------------

;; CIP : Duplicate In-Place -> Bring to Front -> Keep Duplicates Selected
(defun c:CIP (/ *error* ss ssNew i ent vlaEnt newVlaObj oldCmd)
  (setq oldCmd (getvar 'cmdecho))

  (defun *error* (msg)
    (if oldCmd (setvar 'cmdecho oldCmd))
    (CadSetup:UndoReset)
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[CIP] Error: " msg))
    )
    (princ)
  )

  (setvar 'cmdecho 0)

  (setq ss (ssget "_I"))
  (if (not ss)
    (progn
      (princ "\nSelect objects to duplicate in place: ")
      (setq ss (ssget))
    )
  )

  (if ss
    (progn
      (CadSetup:UndoStart)
      (setq ssNew (ssadd))

      (repeat (setq i (sslength ss))
        (setq ent (ssname ss (setq i (1- i))))
        (setq vlaEnt (vlax-ename->vla-object ent))
        (setq newVlaObj (vla-copy vlaEnt))
        (ssadd (vlax-vla-object->ename newVlaObj) ssNew)
      )

      (command "._draworder" ssNew "" "_Front")
      (CadSetup:UndoEnd)

      (sssetfirst nil ssNew)
      (princ (strcat "\n[CIP] " (itoa (sslength ssNew)) " object(s) duplicated in place on top."))
    )
    (princ "\n[CIP] No objects selected.")
  )

  (setvar 'cmdecho oldCmd)
  (princ)
)

;; BBOX : Draw Automatic Bounding Box Rectangle Around Selected Objects
;; Powered by CadSetup:GetBoundingBoxUcs from Helpers/Help_Geometry.lsp
(defun c:BBOX (/ *error* ss bbox p1 p2 oldCmd)
  (setq oldCmd (getvar 'cmdecho))

  (defun *error* (msg)
    (if oldCmd (setvar 'cmdecho oldCmd))
    (CadSetup:UndoReset)
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[BBOX] Error: " msg))
    )
    (princ)
  )

  (setvar 'cmdecho 0)

  (setq ss (ssget "_I"))
  (if (not ss)
    (progn
      (princ "\nSelect objects to draw bounding box around: ")
      (setq ss (ssget))
    )
  )
  (if ss
    (progn
      (CadSetup:UndoStart)
      (setq bbox (CadSetup:GetBoundingBoxUcs ss))
      (if bbox
        (progn
          (setq p1 (car bbox)
                p2 (cadr bbox))
          (command "._rectang" "_non" p1 "_non" p2)
          (princ "\n[BBOX] Bounding box rectangle drawn.")
        )
        (princ "\n[BBOX] Could not calculate bounding box for selected entities.")
      )
      (CadSetup:UndoEnd)
    )
    (princ "\n[BBOX] No objects selected.")
  )
  (setvar 'cmdecho oldCmd)
  (princ)
)


;;; --------------------------------------------------------------------------
;;; 3. OBJECT PROPERTIES & GEOMETRY MODIFIERS
;;; --------------------------------------------------------------------------

;; CTRANS : Set Object Transparency (0 to 90)
(defun c:CTRANS (/ *error* oldCmd ss val)
  (setq oldCmd (getvar "CMDECHO"))

  (defun *error* (msg)
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (CadSetup:UndoReset)
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[CTRANS] Error: " msg))
    )
    (princ)
  )

  (setvar "CMDECHO" 0)
  (princ "\nSelect objects to change transparency: ")
  (if (setq ss (ssget))
    (progn
      (initget 4)
      (setq val (getint "\nEnter transparency value (0 to 90): "))
      (if (and val (<= val 90))
        (progn
          (CadSetup:UndoStart)
          (command "_.CHPROP" ss "" "_Transparency" val "")
          (CadSetup:UndoEnd)
          (princ (strcat "\n[CTRANS] Transparency set to " (itoa val) " for " (itoa (sslength ss)) " object(s)."))
        )
        (princ "\n[CTRANS] Invalid transparency value. Must be between 0 and 90.")
      )
    )
    (princ "\n[CTRANS] No objects selected.")
  )
  (setvar "CMDECHO" oldCmd)
  (princ)
)

;; FL0 : Flatten Selected Objects to 2D (Z = 0)
(defun c:FL0 ( / *error* ss oldecho )
  (setq oldecho (getvar "CMDECHO"))

  (defun *error* (msg)
    (if oldecho (setvar "CMDECHO" oldecho))
    (CadSetup:UndoReset)
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[FL0] Error: " msg))
    )
    (princ)
  )

  (setvar "CMDECHO" 0)
  (princ "\nSelect objects to flatten (or Enter for all modelspace): ")
  (setq ss (ssget))
  (if (not ss) (setq ss (ssget "X" '((410 . "Model")))))
  (if ss
    (progn
      (CadSetup:UndoStart)
      (command "_.MOVE" ss "" '(0 0 0) '(0 0 1e99))
      (command "_.MOVE" ss "" '(0 0 0) '(0 0 -1e99))
      (CadSetup:UndoEnd)
      (princ (strcat "\n[FL0] " (itoa (sslength ss)) " object(s) flattened to Z=0."))
    )
    (princ "\n[FL0] No objects found to flatten.")
  )
  (setvar "CMDECHO" oldecho)
  (princ)
)


;;; --------------------------------------------------------------------------
;;; 4. DRAWING CLEANUP & REPAIR
;;; --------------------------------------------------------------------------

;; PUA / QA : Deep Purge & Database Audit
(defun c:PUA ( / *error* oldecho )
  (setq oldecho (getvar "CMDECHO"))

  (defun *error* (msg)
    (if oldecho (setvar "CMDECHO" oldecho))
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[PUA] Error: " msg))
    )
    (princ)
  )

  (setvar "CMDECHO" 0)
  (princ "\n[PUA] Purging all unused blocks, layers, and styles...")
  (command "-PURGE" "ALL" "*" "N")
  (command "-PURGE" "REGAPPS" "*" "N")
  (command "-PURGE" "ZERO" "N")
  (command "-PURGE" "EMPTY" "N")
  (princ "\n[PUA] Auditing database and fixing errors...")
  (command "_.AUDIT" "Y")
  (setvar "CMDECHO" oldecho)
  (princ "\n[PUA] Deep purge and audit complete.")
  (princ)
)
(defun c:QA () (c:PUA))


;;; --------------------------------------------------------------------------
;;; 5. SYSTEM REPAIR & ENVIRONMENT FIXES
;;; --------------------------------------------------------------------------

;; FIXSELECT : Restore Noun/Verb, Additive Selection, and Highlights
(defun c:FIXSELECT ()
  (CadSetup:SafeSetVar "PICKFIRST" 1)
  (CadSetup:SafeSetVar "PICKADD" 2)
  (CadSetup:SafeSetVar "PICKAUTO" 5)
  (CadSetup:SafeSetVar "HIGHLIGHT" 1)
  (princ "\n[FIXSELECT] Selection environment restored (PICKFIRST=1, PICKADD=2, PICKAUTO=5, HIGHLIGHT=1).")
  (princ)
)

;; FIXBOX : Restore File, Command & Attribute Dialog Boxes
(defun c:FIXBOX ()
  (CadSetup:SafeSetVar "FILEDIA" 1)
  (CadSetup:SafeSetVar "CMDDIA" 1)
  (CadSetup:SafeSetVar "ATTDIA" 1)
  (princ "\n[FIXBOX] Dialog boxes restored (FILEDIA=1, CMDDIA=1, ATTDIA=1).")
  (princ)
)

;; WF : Wipeout Frame Toggle (Cycle: Visible & Printable -> Draft -> Hidden)
(defun c:WF (/ *error* oldecho curVal newVal)
  (setq oldecho (getvar "CMDECHO"))

  (defun *error* (msg)
    (if oldecho (setvar "CMDECHO" oldecho))
    (princ)
  )

  (setvar "CMDECHO" 0)
  (setq curVal (getvar "WIPEOUTFRAME"))
  (cond
    ((= curVal 0)
      (setvar "WIPEOUTFRAME" 1)
      (princ "\n[WF] WIPEOUTFRAME = 1 (Display & Plot)"))
    ((= curVal 1)
      (setvar "WIPEOUTFRAME" 2)
      (princ "\n[WF] WIPEOUTFRAME = 2 (Display Only, Do Not Plot)"))
    (t
      (setvar "WIPEOUTFRAME" 0)
      (princ "\n[WF] WIPEOUTFRAME = 0 (Frames Hidden)"))
  )
  (setvar "CMDECHO" oldecho)
  (princ)
)

(princ "\n[06_Utilities.lsp] Productivity utilities and system repair tools loaded.")
(princ)