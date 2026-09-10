;;; ==========================================================================
;;; 06_Utilities.lsp - Drawing Cleanup, Measurement, System Fixes & Utilities
;;; ==========================================================================
;;; Category : Drawing Management & Productivity Aids
;;; Author   : Haseeb
;;; ==========================================================================

(vl-load-com)

;;; --------------------------------------------------------------------------
;;; 1. MEASUREMENT & CALCULATION
;;; --------------------------------------------------------------------------

;; TC : Total Curve & Length Calculator (Lines, Polylines, Arcs, Splines, Circles)
(defun c:TC (/ ss i ent obj len total)
  (setq total 0.0)
  (setq ss (ssget "_I"))
  (if (not ss)
    (progn
      (princ "\nSelect lines, polylines, arcs, circles, or splines for total length: ")
      (setq ss (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE,ARC,CIRCLE,SPLINE,ELLIPSE"))))
    )
  )
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i)
              obj (vlax-ename->vla-object ent))
        (if (not (vl-catch-all-error-p (setq len (vl-catch-all-apply 'vlax-curve-getdistatparam (list obj (vlax-curve-getendparam obj))))))
          (setq total (+ total len))
        )
        (setq i (1+ i))
      )
      (princ (strcat "\n[TC] Total Length of " (itoa (sslength ss)) " object(s) = " (rtos total 2 4)))
    )
    (princ "\n[TC] No objects selected.")
  )
  (princ)
)

;; Note: T (MEASUREGEOM QUICK) alias is maintained in Commands/99_Aliases.lsp

;;; --------------------------------------------------------------------------
;;; 2. SMART DUPLICATION & BOUNDING BOX
;;; --------------------------------------------------------------------------

;; CIP : Duplicate In-Place -> Bring to Front -> Keep Duplicates Selected
(defun c:CIP (/ *error* ss ssNew doc i ent vlaEnt newVlaObj oldCmd)
  (vl-load-com)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq oldCmd (getvar 'cmdecho))

  ;; Localized Error Handler & Undo Stack Safety
  (defun *error* (msg)
    (if oldCmd (setvar 'cmdecho oldCmd))
    (if (and doc (= (type doc) 'VLA-OBJECT))
      (vl-catch-all-apply 'vla-endundomark (list doc))
    )
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
      (vla-startundomark doc)
      (setq ssNew (ssadd))

      (repeat (setq i (sslength ss))
        (setq ent (ssname ss (setq i (1- i))))
        (setq vlaEnt (vlax-ename->vla-object ent))
        (setq newVlaObj (vla-copy vlaEnt))
        (ssadd (vlax-vla-object->ename newVlaObj) ssNew)
      )

      (command "._draworder" ssNew "" "_Front")
      (vla-endundomark doc)

      ;; Activate grips on new duplicates
      (sssetfirst nil ssNew)
      (princ (strcat "\n[CIP] " (itoa (sslength ssNew)) " object(s) duplicated in place on top."))
    )
    (princ "\n[CIP] No objects selected.")
  )

  (setvar 'cmdecho oldCmd)
  (princ)
)

;; BBOX : Draw Automatic Bounding Box Rectangle Around Selected Objects
(defun c:BBOX (/ *error* doc ss i ent obj minPt maxPt pMinW pMaxW ptU
                 allUcsX allUcsY p1 p2 oldCmd)
  (vl-load-com)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq oldCmd (getvar 'cmdecho))

  ;; Localized Error Handler & Undo Stack Safety
  (defun *error* (msg)
    (if oldCmd (setvar 'cmdecho oldCmd))
    (if (and doc (= (type doc) 'VLA-OBJECT))
      (vl-catch-all-apply 'vla-endundomark (list doc))
    )
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
      (vla-startundomark doc)
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i)
              obj (vlax-ename->vla-object ent))
        (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list obj 'minPt 'maxPt))))
          (progn
            (setq pMinW (vlax-safearray->list minPt)
                  pMaxW (vlax-safearray->list maxPt))
            ;; Evaluate all 4 bounding rectangle corners transformed from WCS (0) to Current UCS (1)
            (foreach ptW (list pMinW
                               (list (car pMaxW) (cadr pMinW) (caddr pMinW))
                               pMaxW
                               (list (car pMinW) (cadr pMaxW) (caddr pMinW)))
              (setq ptU (trans ptW 0 1))
              (setq allUcsX (cons (car ptU) allUcsX)
                    allUcsY (cons (cadr ptU) allUcsY))
            )
          )
        )
        (setq i (1+ i))
      )
      (if (and allUcsX allUcsY)
        (progn
          (setq p1 (list (apply 'min allUcsX) (apply 'min allUcsY) 0.0)
                p2 (list (apply 'max allUcsX) (apply 'max allUcsY) 0.0))
          (command "._rectang" "_non" p1 "_non" p2)
          (princ "\n[BBOX] Bounding box rectangle drawn.")
        )
      )
      (vla-endundomark doc)
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
(defun c:CTRANS (/ *error* doc oldCmd ss val)
  (vl-load-com)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq oldCmd (getvar "CMDECHO"))

  (defun *error* (msg)
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if (and doc (= (type doc) 'VLA-OBJECT))
      (vl-catch-all-apply 'vla-endundomark (list doc))
    )
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
          (vla-startundomark doc)
          (command "_.CHPROP" ss "" "_Transparency" val "")
          (vla-endundomark doc)
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
(defun c:FL0 ( / *error* doc ss oldecho )
  (vl-load-com)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq oldecho (getvar "CMDECHO"))

  (defun *error* (msg)
    (if oldecho (setvar "CMDECHO" oldecho))
    (if (and doc (= (type doc) 'VLA-OBJECT))
      (vl-catch-all-apply 'vla-endundomark (list doc))
    )
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
      (vla-startundomark doc)
      (command "_.MOVE" ss "" '(0 0 0) '(0 0 1e99))
      (command "_.MOVE" ss "" '(0 0 0) '(0 0 -1e99))
      (vla-endundomark doc)
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

;; Note: QS (QSAVE) and CL (CLOSE) aliases are maintained in Commands/99_Aliases.lsp

;;; --------------------------------------------------------------------------
;;; 5. SYSTEM REPAIR & ENVIRONMENT FIXES
;;; --------------------------------------------------------------------------

;; FIXSELECT : Restore Noun/Verb, Additive Selection, and Highlights
(defun c:FIXSELECT ()
  (setvar "PICKFIRST" 1)
  (setvar "PICKADD" 2)
  (setvar "PICKAUTO" 5)
  (setvar "HIGHLIGHT" 1)
  (princ "\n[FIXSELECT] Selection environment restored (PICKFIRST=1, PICKADD=2, PICKAUTO=5, HIGHLIGHT=1).")
  (princ)
)

;; FIXBOX : Restore File, Command & Attribute Dialog Boxes
(defun c:FIXBOX ()
  (setvar "FILEDIA" 1)
  (setvar "CMDDIA" 1)
  (setvar "ATTDIA" 1)
  (princ "\n[FIXBOX] Dialog boxes restored (FILEDIA=1, CMDDIA=1, ATTDIA=1).")
  (princ)
)

;; WF : Wipeout Frame Toggle (Cycle: Visible & Printable -> Draft -> Hidden)
(defun c:WF (/ *error* oldecho)
  (setq oldecho (getvar "CMDECHO"))

  (defun *error* (msg)
    (if oldecho (setvar "CMDECHO" oldecho))
    (princ)
  )

  (setvar "CMDECHO" 0)
  (cond
    ((= (getvar "WIPEOUTFRAME") 0)
      (setvar "WIPEOUTFRAME" 1)
      (princ "\n[WF] WIPEOUTFRAME = 1 (Display & Plot)"))
    ((= (getvar "WIPEOUTFRAME") 1)
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