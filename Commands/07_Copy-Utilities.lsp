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

;; cs:extract-pt - Recursively extracts first 3D coordinate point (3 numbers) from ssnamex structure
(defun cs:extract-pt (data / res)
  (cond
    ;; 3-element list of numbers: 3D point (X Y Z)
    ((and (vl-consp data)
          (= (length data) 3)
          (numberp (car data))
          (numberp (cadr data))
          (numberp (caddr data)))
     data)
    ;; Recursively search sublists
    ((vl-consp data)
     (or (cs:extract-pt (car data))
         (cs:extract-pt (cdr data))))
    (t nil)
  )
)

;; cs:extract-pick-pt - Extracts WCS click coordinate from ssnamex record item
(defun cs:extract-pick-pt (item / candidate pt)
  (if (and (vl-consp item)
           (vl-consp (car item))
           (not (numberp (caar item))))
    (setq item (car item))
  )
  (if (and (vl-consp item)
           (setq candidate (caddr item))
           (vl-consp candidate))
    (progn
      (if (and (vl-consp (car candidate))
               (= (length (car candidate)) 3)
               (numberp (caar candidate))
               (numberp (cadar candidate))
               (numberp (caddar candidate)))
        (setq pt (car candidate))
        (if (and (= (length candidate) 3)
                 (numberp (car candidate))
                 (numberp (cadr candidate))
                 (numberp (caddr candidate)))
          (setq pt candidate)
        )
      )
    )
  )
  (if (null pt)
    (setq pt (cs:extract-pt item))
  )
  pt
)

;; cs:get-ssnamex-item - Robust retrieval of ssnamex element for index idx
(defun cs:get-ssnamex-item (ss idx / item)
  (setq item (vl-catch-all-apply 'ssnamex (list ss idx)))
  (if (or (vl-catch-all-error-p item) (null item))
    (setq item (vl-catch-all-apply 'ssnamex (list ss)))
  )
  (if (and (not (vl-catch-all-error-p item)) (vl-consp item))
    (if (and (vl-consp (car item)) (not (numberp (caar item))))
      (nth idx item)
      item
    )
    nil
  )
)

;; cs:extract-poly-segment - Extracts single segment from any 2D/3D polyline at pickPtWCS
;; Returns (newEnt ptOnCurve) or nil
(defun cs:extract-poly-segment (ent pickPtWCS / obj ed entType isClosed endParam maxIdx
                                                ptOnCurve param idx p1 p2 bulge layer norm elev
                                                p1_ocs p2_ocs dxfList newEnt dist bestIdx minDist
                                                testIdx midPt curDist v vIdx vData)
  (setq ed      (entget ent)
        entType (cdr (assoc 0 ed))
        obj     (vlax-ename->vla-object ent))

  ;; 1. Get closest point on curve (WCS)
  (setq ptOnCurve (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list ent pickPtWCS)))
  (if (or (vl-catch-all-error-p ptOnCurve) (null ptOnCurve))
    (setq ptOnCurve pickPtWCS)
  )

  ;; 2. Get parameter at point
  (setq param (vl-catch-all-apply 'vlax-curve-getParamAtPoint (list ent ptOnCurve)))
  (if (or (vl-catch-all-error-p param) (null param))
    (progn
      (setq dist (vl-catch-all-apply 'vlax-curve-getDistAtPoint (list ent ptOnCurve)))
      (if (and dist (not (vl-catch-all-error-p dist)) (numberp dist))
        (setq param (vl-catch-all-apply 'vlax-curve-getParamAtDist (list ent dist)))
        (setq param nil)
      )
    )
  )
  (if (or (vl-catch-all-error-p param) (null param) (not (numberp param)))
    (setq param nil)
  )

  ;; 3. Determine segment indices
  (setq endParam (vl-catch-all-apply 'vlax-curve-getEndParam (list ent)))
  (if (or (vl-catch-all-error-p endParam) (null endParam) (not (numberp endParam)))
    (setq endParam 1.0)
  )
  (setq maxIdx   (fix endParam)
        isClosed (vlax-curve-isClosed ent))

  ;; If param was found, use it; otherwise use geometric distance fallback
  (if param
    (setq idx (fix (+ param 1e-8)))
    (progn
      (setq bestIdx 0
            minDist 1e30
            testIdx 0)
      (while (< testIdx maxIdx)
        (setq midPt (vlax-curve-getPointAtParam ent (+ testIdx 0.5)))
        (if midPt
          (progn
            (setq curDist (distance ptOnCurve midPt))
            (if (< curDist minDist)
              (setq minDist curDist
                    bestIdx testIdx)
            )
          )
        )
        (setq testIdx (1+ testIdx))
      )
      (setq idx bestIdx)
    )
  )

  ;; Clamp index safely for open and closed polylines
  (if isClosed
    (if (>= idx maxIdx) (setq idx 0))
    (if (>= idx maxIdx) (setq idx (max 0 (1- maxIdx))))
  )
  (if (< idx 0) (setq idx 0))

  ;; 4. Get segment endpoints
  (setq p1 (vlax-curve-getPointAtParam ent idx))
  (setq p2 (if (and isClosed (= (1+ idx) maxIdx))
             (or (vlax-curve-getPointAtParam ent (1+ idx))
                 (vlax-curve-getStartPoint ent))
             (vlax-curve-getPointAtParam ent (1+ idx))))

  (if (and p1 p2)
    (progn
      ;; 5. Determine bulge curvature
      (setq bulge 0.0)
      (if (vlax-method-applicable-p obj 'GetBulge)
        (setq bulge (vla-GetBulge obj idx))
      )
      (if (null bulge) (setq bulge 0.0))

      ;; Also check heavy 2D polyline vertex if bulge is 0
      (if (and (= entType "POLYLINE") (zerop bulge))
        (progn
          (setq v (entnext ent)
                vIdx 0)
          (while (and v (= (cdr (assoc 0 (setq vData (entget v)))) "VERTEX") (< vIdx idx))
            (setq v (entnext v)
                  vIdx (1+ vIdx))
          )
          (if (and v (= (cdr (assoc 0 vData)) "VERTEX"))
            (setq bulge (or (cdr (assoc 42 vData)) 0.0))
          )
        )
      )

      (setq layer (cdr (assoc 8 ed))
            norm  (cdr (assoc 210 ed)))
      (if (null norm) (setq norm '(0.0 0.0 1.0)))

      ;; 6. Create entity:
      ;; If straight segment on 3D polyline or tilted plane:
      (if (and (zerop bulge)
               (or (= entType "POLYLINE")
                   (not (equal norm '(0.0 0.0 1.0) 1e-4))))
        ;; Create 3D straight segment as clean LINE
        (setq newEnt
          (entmakex
            (vl-remove-if 'null
              (list
                '(0 . "LINE")
                '(100 . "AcDbEntity")
                (cons 8 layer)
                (assoc 6 ed)
                (assoc 48 ed)
                (assoc 62 ed)
                (assoc 370 ed)
                (assoc 420 ed)
                (assoc 440 ed)
                '(100 . "AcDbLine")
                (cons 10 p1)
                (cons 11 p2)
              )
            )
          )
        )
        ;; Otherwise: 2D LWPOLYLINE segment (preserves arc bulge)
        (progn
          (setq p1_ocs (trans p1 0 norm)
                p2_ocs (trans p2 0 norm)
                elev   (cdr (assoc 38 ed)))
          (if (null elev) (setq elev (caddr p1_ocs)))

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

          ;; Fallback 1: minimal LWPOLYLINE definition
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

          ;; Fallback 2: LINE if straight segment
          (if (and (not newEnt) (zerop bulge))
            (setq newEnt
              (entmakex
                (list
                  '(0 . "LINE")
                  '(100 . "AcDbEntity")
                  (cons 8 layer)
                  '(100 . "AcDbLine")
                  (cons 10 p1)
                  (cons 11 p2)
                )
              )
            )
          )
        )
      )
    )
  )

  (if newEnt
    (list newEnt ptOnCurve)
    nil
  )
)

;; cs:extract-curve-entity - Duplicates standalone line, arc, circle, ellipse, or spline
;; Returns (newEnt ptOnCurve) or nil
(defun cs:extract-curve-entity (ent pickPtWCS / ed entType obj ptOnCurve vlaCopy newEnt)
  (setq ed      (entget ent)
        entType (cdr (assoc 0 ed))
        obj     (vlax-ename->vla-object ent))

  ;; Closest point on curve for drag attachment
  (setq ptOnCurve (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list ent pickPtWCS)))
  (if (or (vl-catch-all-error-p ptOnCurve) (null ptOnCurve))
    (setq ptOnCurve pickPtWCS)
  )

  (setq vlaCopy (vl-catch-all-apply 'vla-copy (list obj)))
  (if (and vlaCopy (not (vl-catch-all-error-p vlaCopy)))
    (setq newEnt (vlax-vla-object->ename vlaCopy))
    ;; Fallback to entmakex if ActiveX copy fails
    (setq newEnt (entmakex (vl-remove-if '(lambda (x) (vl-position (car x) '(-1 5 330))) ed)))
  )
  (if newEnt
    (list newEnt ptOnCurve)
    nil
  )
)

;; cs:duplicate-segment - High-level router that duplicates a segment or entity,
;; handling nested blocks (via xformMatrix), polylines, lines, and curves
;; Returns (newEnt ptOnCurve) or nil
(defun cs:duplicate-segment (ent pickPtWCS xformMatrix / ed entType res tempEnt
                                                         p1 nxt nxtData p2 layer newEnt)
  (if (and ent (setq ed (entget ent)))
    (progn
      (setq entType (cdr (assoc 0 ed)))

      (cond
        ;; Special Case: Entity picked was a VERTEX (e.g. 3D Polyline vertex via nentselp)
        ((= entType "VERTEX")
         (setq p1      (cdr (assoc 10 ed))
               nxt     (entnext ent)
               nxtData (if nxt (entget nxt))
               p2      (if (and nxtData (= (cdr (assoc 0 nxtData)) "VERTEX"))
                         (cdr (assoc 10 nxtData)))
               layer   (cdr (assoc 8 ed))
               newEnt  nil)
         (if (and p1 p2)
           (progn
             (setq newEnt
               (entmakex
                 (list
                   '(0 . "LINE")
                   '(100 . "AcDbEntity")
                   (cons 8 layer)
                   '(100 . "AcDbLine")
                   (cons 10 p1)
                   (cons 11 p2)
                 )
               )
             )
             (if (and newEnt xformMatrix)
               (vl-catch-all-apply 'vla-transformby
                 (list (vlax-ename->vla-object newEnt) (vlax-tmatrix xformMatrix)))
             )
             (if newEnt (list newEnt pickPtWCS))
           )
         ))

        ;; Case A: Nested inside a block (xformMatrix provided)
        (xformMatrix
          (cond
            ;; Polyline inside block: duplicate, transform to modelspace, extract segment, clean up
            ((wcmatch entType "*POLYLINE")
             (setq tempEnt (entmakex (vl-remove-if '(lambda (x) (vl-position (car x) '(-1 5 330))) ed)))
             (if tempEnt
               (progn
                 (vl-catch-all-apply 'vla-transformby
                   (list (vlax-ename->vla-object tempEnt) (vlax-tmatrix xformMatrix)))
                 (setq res (cs:extract-poly-segment tempEnt pickPtWCS))
                 (entdel tempEnt)
                 res
               )
             ))
            ;; Standalone line, arc, curve inside block: duplicate and transform
            (t
             (setq tempEnt (entmakex (vl-remove-if '(lambda (x) (vl-position (car x) '(-1 5 330))) ed)))
             (if tempEnt
               (progn
                 (vl-catch-all-apply 'vla-transformby
                   (list (vlax-ename->vla-object tempEnt) (vlax-tmatrix xformMatrix)))
                 (list tempEnt pickPtWCS)
               )
             ))
          )
        )

        ;; Case B: Top-level polyline
        ((wcmatch entType "*POLYLINE")
         (cs:extract-poly-segment ent pickPtWCS))

        ;; Case C: Top-level standalone line or curve
        (t
         (cs:extract-curve-entity ent pickPtWCS))
      )
    )
    nil
  )
)

;; COPYSEG / CS : Duplicate Single Polyline Segment or Line with Live Drag Placement
(defun c:COPYSEG ( / *error* oldEcho ss newEnts dragBasePt i len ent sx pt pick
                     pickPtWCS xformMatrix res moveSS basePt startP endP entObj )

  (defun *error* (msg)
    ;; Clean up any duplicated entities if user cancelled during placement
    (if newEnts
      (foreach e newEnts
        (if (and e (entget e)) (entdel e))
      )
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

  ;; 1. Handle Pre-selection (Highlight / PickFirst / Ctrl+Click)
  (setq ss (cadr (ssgetfirst)))
  (if (or (null ss) (zerop (sslength ss)))
    (setq ss (ssget "_I"))
  )

  (if (and ss (> (sslength ss) 0))
    ;; --- PRE-SELECTION PATH (Highlights as input) ---
    (progn
      (setq len (sslength ss)
            i   0)
      (if (boundp 'CadSetup:UndoStart) (CadSetup:UndoStart))

      (while (< i len)
        (setq ent (ssname ss i)
              sx  (cs:get-ssnamex-item ss i)
              pt  (cs:extract-pick-pt sx))

        ;; If pick point was not embedded in ssnamex (e.g. crossing window), compute safe point
        (if (null pt)
          (progn
            (setq startP (vl-catch-all-apply 'vlax-curve-getStartParam (list ent))
                  endP   (vl-catch-all-apply 'vlax-curve-getEndParam (list ent)))
            (if (and (numberp startP) (numberp endP))
              (setq pt (vlax-curve-getPointAtParam ent (/ (+ startP endP) 2.0)))
              (setq pt (getvar 'VIEWCTR))
            )
          )
        )

        (setq pickPtWCS pt)
        (setq res (cs:duplicate-segment ent pickPtWCS nil))
        (if res
          (progn
            (setq newEnts (cons (car res) newEnts))
            (if (null dragBasePt) (setq dragBasePt (cadr res)))
          )
        )
        (setq i (1+ i))
      )
      (sssetfirst nil nil) ; Disarm active selection highlight now that geometry is extracted
    )

    ;; --- INTERACTIVE SELECTION PATH ---
    (progn
      (setq pick (nentselp "\nSelect polyline segment, line, or curve to copy: "))
      (if pick
        (progn
          (setq ent         (car pick)
                pickPtWCS   (cadr pick)
                xformMatrix (caddr pick))

          (if (boundp 'CadSetup:UndoStart) (CadSetup:UndoStart))
          (setq res (cs:duplicate-segment ent pickPtWCS xformMatrix))
          (if res
            (progn
              (setq newEnts (list (car res)))
              (setq dragBasePt (cadr res))
            )
            (princ "\n[COPYSEG] Could not extract segment from selected object.")
          )
        )
      )
    )
  )

  ;; 2. Live Placement Drag
  (if (and newEnts dragBasePt)
    (progn
      (setq moveSS (ssadd))
      (foreach e newEnts (ssadd e moveSS))

      (setq basePt (trans dragBasePt 0 1)) ; Drag attaches to exact clicked spot in UCS
      (setvar 'cmdecho 1)
      (command "_.move" moveSS "" "_non" basePt)
      (while (> (getvar 'cmdactive) 0)
        (command pause)
      )
      (setq newEnts nil) ; Disarm error handler cleanup on successful drop
      (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))
      (princ (strcat "\n[COPYSEG] " (itoa (sslength moveSS)) " segment(s) copied with live placement."))
    )
    (progn
      (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))
      (if (and (null ss) (null pick))
        (princ "\n[COPYSEG] No object selected.")
      )
    )
  )

  (setvar 'cmdecho oldEcho)
  (princ)
)

;; Command Aliases
(defun c:CS       () (c:COPYSEG))
(defun c:CPYSEG   () (c:COPYSEG))
(defun c:CPY      () (c:COPYSEG))
(defun c:COPY-SEG () (c:COPYSEG))
(defun c:CPY-SEG  () (c:COPYSEG))

(if *CadSetup-Debug*
  (princ "\n[07_Copy-Utilities.lsp] Smart duplication and copy utilities loaded.")
)
(princ)
