;;; ==========================================================================
;;; 07_Copy-Utilities.lsp - Smart Duplication, In-Place & Segment Copy Tools
;;; Layer: Commands (Priority 07)
;;; Author   : Haseeb
;;; Commands : CIP (Copy In-Place), COPYSEG / CS (Copy Segment or Line)
;;; ==========================================================================
;;; ARCHITECTURAL NOTE:
;;; Dedicated suite for high-velocity copying and duplication workflows:
;;; 1. CIP (Copy In-Place)     : Clones entities at (0,0,0) and moves them to
;;;                              the front of the draw order, keeping the new
;;;                              entities selected.
;;; 2. COPYSEG / CS (Copy Seg) : Duplicates a clicked single polyline segment,
;;;                              individual line, or curve and immediately
;;;                              attaches it to the cursor with live placement
;;;                              drag from the pick point.
;;; Both commands feature atomic undo transactions and defensive cancellation
;;; cleanup so aborted operations leave zero orphaned entities in the drawing.
;;; ==========================================================================

(vl-load-com)

;;; --------------------------------------------------------------------------
;;; 1. IN-PLACE DUPLICATION & DRAW ORDER
;;; --------------------------------------------------------------------------

;; CIP : Duplicate In-Place -> Bring to Front -> Keep Duplicates Selected
(defun c:CIP (/ *error* ss ssNew i ent vlaEnt newVlaObj oldCmd)
  (setq oldCmd (getvar 'cmdecho))

  (defun *error* (msg)
    (if oldCmd (setvar 'cmdecho oldCmd))
    (if (boundp 'CadSetup:UndoReset) (CadSetup:UndoReset))
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
      (if (boundp 'CadSetup:UndoStart) (CadSetup:UndoStart))
      (setq ssNew (ssadd))

      (repeat (setq i (sslength ss))
        (setq ent (ssname ss (setq i (1- i))))
        (setq vlaEnt (vlax-ename->vla-object ent))
        (setq newVlaObj (vla-copy vlaEnt))
        (ssadd (vlax-vla-object->ename newVlaObj) ssNew)
      )

      (command "._draworder" ssNew "" "_Front")
      (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))

      (sssetfirst nil ssNew)
      (princ (strcat "\n[CIP] " (itoa (sslength ssNew)) " object(s) duplicated in place on top."))
    )
    (princ "\n[CIP] No objects selected.")
  )

  (setvar 'cmdecho oldCmd)
  (princ)
)


;;; --------------------------------------------------------------------------
;;; 2. SEGMENT & CURVE DUPLICATION WITH LIVE DRAG
;;; --------------------------------------------------------------------------

;; COPYSEG / CS : Duplicate Single Polyline Segment or Line with Live Drag Placement
(defun c:COPYSEG ( / *error* oldEcho ent pickPt ss sx data pts obj pickPtWCS
                     ptOnCurve param idx maxParam p1 p2 bulge ed layer norm
                     elev p1_ocs p2_ocs newEnt basePt isWcs dxfList entType vlaCopy )

  (defun *error* (msg)
    ;; Clean up duplicated entity if cancelled during placement
    (if (and newEnt (entget newEnt))
      (entdel newEnt)
    )
    (if oldEcho (setvar 'cmdecho oldEcho))
    (if (boundp 'CadSetup:UndoReset) (CadSetup:UndoReset))
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[COPYSEG] Error: " msg))
    )
    (princ)
  )

  (setq oldEcho (getvar 'cmdecho))
  (setvar 'cmdecho 0)

  ;; 1. Handle Pre-selection or Prompt for Selection
  (if (setq ss (ssget "_I"))
    (progn
      (setq ent (ssname ss 0))
      ;; Extract coordinate from pre-selection record
      (setq sx (car (ssnamex ss 0)))
      (if (and sx (= (car sx) 1) (setq data (cadddr sx)))
        (progn
          (setq pts (cdr data))
          (if (and pts (vl-consp (car pts)))
            (setq pickPt (list
                           (/ (apply '+ (mapcar 'car pts)) (float (length pts)))
                           (/ (apply '+ (mapcar 'cadr pts)) (float (length pts)))
                           (/ (apply '+ (mapcar 'caddr pts)) (float (length pts)))
                         ))
            (if (and pts (numberp (car pts)))
              (setq pickPt pts)
            )
          )
          (if pickPt (setq isWcs t)) ; Points from ssnamex are in WCS
        )
      )
      (sssetfirst nil nil) ; Clear active selection
      (if (not pickPt)
        (progn
          (setq ent (entsel "\nSelect polyline segment or line to copy: "))
          (if ent
            (setq pickPt (cadr ent)
                  ent    (car ent))
          )
        )
      )
    )
    (progn
      (setq ent (entsel "\nSelect polyline segment or line to copy: "))
      (if ent
        (setq pickPt (cadr ent)
              ent    (car ent))
      )
    )
  )

  ;; 2. Validate Entity and Extract Geometry
  (if (and ent pickPt)
    (progn
      (setq ed        (entget ent)
            entType   (cdr (assoc 0 ed))
            obj       (vlax-ename->vla-object ent)
            pickPtWCS (if isWcs pickPt (trans pickPt 1 0)))

      ;; Calculate closest point on curve for exact drag attachment
      (if (vlax-method-applicable-p obj 'getClosestPointTo)
        (setq ptOnCurve (vlax-curve-getClosestPointTo obj pickPtWCS))
        (setq ptOnCurve pickPtWCS)
      )
      (if (null ptOnCurve) (setq ptOnCurve pickPtWCS))

      ;; 3. Create Duplicate (Segment of Polyline OR Single Line / Curve)
      (if (wcmatch entType "*POLYLINE")
        ;; --- POLYLINE SEGMENT EXTRACTION ---
        (progn
          (setq param (vlax-curve-getParamAtPoint obj ptOnCurve))
          (if (null param)
            (setq param (vlax-curve-getParamAtDist obj (vlax-curve-getDistAtPoint obj ptOnCurve)))
          )

          (if param
            (progn
              (setq idx      (fix (+ param 1e-8))
                    maxParam (fix (vlax-curve-getEndParam obj)))

              ;; Clamp index safely for both open and closed polylines
              (if (>= idx maxParam)
                (setq idx (1- maxParam))
              )
              (if (< idx 0)
                (setq idx 0)
              )

              ;; Extract segment endpoints and bulge
              (setq p1    (vlax-curve-getPointAtParam obj idx)
                    p2    (vlax-curve-getPointAtParam obj (1+ idx))
                    bulge (if (vlax-method-applicable-p obj 'GetBulge)
                            (vla-GetBulge obj idx)
                            0.0))
              (if (null bulge) (setq bulge 0.0))

              (if (and p1 p2)
                (progn
                  ;; Read layer and coordinate system extrusion data
                  (setq layer (cdr (assoc 8 ed))
                        norm  (cdr (assoc 210 ed))
                        elev  (cdr (assoc 38 ed)))
                  (if (null norm) (setq norm '(0.0 0.0 1.0)))

                  ;; Transform WCS coordinates to OCS for LWPOLYLINE creation
                  (setq p1_ocs (trans p1 0 norm)
                        p2_ocs (trans p2 0 norm))
                  (if (null elev) (setq elev (caddr p1_ocs)))

                  ;; Begin Undo & Create Standalone Segment
                  (if (boundp 'CadSetup:UndoStart) (CadSetup:UndoStart))

                  (setq dxfList
                    (vl-remove-if 'null
                      (list
                        '(0 . "LWPOLYLINE")
                        '(100 . "AcDbEntity")
                        (cons 8 layer)
                        (assoc 6 ed)
                        (assoc 48 ed)
                        (assoc 62 ed)
                        (assoc 370 ed)
                        (assoc 420 ed)
                        (assoc 440 ed)
                        '(100 . "AcDbPolyline")
                        '(90 . 2)
                        '(70 . 0)
                        (cons 38 elev)
                        (cons 10 (list (car p1_ocs) (cadr p1_ocs)))
                        (cons 42 bulge)
                        (cons 10 (list (car p2_ocs) (cadr p2_ocs)))
                        (cons 42 0.0)
                        (cons 210 norm)
                      )
                    )
                  )

                  (setq newEnt (entmakex dxfList))

                  ;; Fallback to standard definition if extended visual properties fail
                  (if (not newEnt)
                    (setq newEnt
                      (entmakex
                        (list
                          '(0 . "LWPOLYLINE")
                          '(100 . "AcDbEntity")
                          (cons 8 layer)
                          '(100 . "AcDbPolyline")
                          '(90 . 2)
                          '(70 . 0)
                          (cons 38 elev)
                          (cons 10 (list (car p1_ocs) (cadr p1_ocs)))
                          (cons 42 bulge)
                          (cons 10 (list (car p2_ocs) (cadr p2_ocs)))
                          (cons 42 0.0)
                          (cons 210 norm)
                        )
                      )
                    )
                  )
                )
                (princ "\n[COPYSEG] Could not calculate segment endpoints.")
              )
            )
            (princ "\n[COPYSEG] Could not determine segment on curve.")
          )
        )
        ;; --- SINGLE LINE / STANDALONE ENTITY COPY ---
        (progn
          (if (boundp 'CadSetup:UndoStart) (CadSetup:UndoStart))
          (setq vlaCopy (vl-catch-all-apply 'vla-copy (list obj)))
          (if (and vlaCopy (not (vl-catch-all-error-p vlaCopy)))
            (setq newEnt (vlax-vla-object->ename vlaCopy))
            (princ "\n[COPYSEG] Failed to copy selected object.")
          )
        )
      )

      ;; 4. Move to Destination with Live Drag
      (if newEnt
        (progn
          (setq basePt (trans ptOnCurve 0 1)) ; Drag attaches to exact clicked spot
          (setvar 'cmdecho 1)
          (command "_.move" newEnt "" "_non" basePt)
          (while (> (getvar 'cmdactive) 0)
            (command pause)
          )
          (setq newEnt nil) ; Clean exit, disarm error cancellation delete
          (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))
          (princ (strcat "\n[COPYSEG] " (if (wcmatch entType "*POLYLINE") "Segment" entType) " copied."))
        )
        (progn
          (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))
        )
      )
    )
    (princ "\n[COPYSEG] No object selected.")
  )

  (setvar 'cmdecho oldEcho)
  (princ)
)

;; Command Alias
(defun c:CS ()
  (c:COPYSEG)
)

(if *CadSetup-Debug*
  (princ "\n[07_Copy-Utilities.lsp] Smart duplication and copy utilities loaded.")
)
(princ)
