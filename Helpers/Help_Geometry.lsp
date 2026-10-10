;;; ==========================================================================
;;; Help_Geometry.lsp - Geometric Math, Bounding Boxes & Curve Length Engine
;;; Layer: Helpers (Level 1)
;;; ==========================================================================

(vl-load-com)

;; ===========================================================================
;; BOUNDING BOX CALCULATIONS
;; ===========================================================================

;; CadSetup:GetBoundingBoxDxf - Extracts tight WCS bounding box via DXF group codes
;; Fallback for environments where ActiveX COM automation server is unreachable
(defun CadSetup:GetBoundingBoxDxf (ent / d eType pts p10 p11 r)
  (if (and ent (= (type ent) 'ENAME))
    (progn
      (setq d (entget ent)
            eType (cdr (assoc 0 d)))
      (cond
        ((= eType "LINE")
         (setq p10 (cdr (assoc 10 d))
               p11 (cdr (assoc 11 d)))
         (list (list (min (car p10) (car p11)) (min (cadr p10) (cadr p11)) (min (caddr p10) (caddr p11)))
               (list (max (car p10) (car p11)) (max (cadr p10) (cadr p11)) (max (caddr p10) (caddr p11))))
        )
        ((= eType "LWPOLYLINE")
         (setq pts (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 10)) d)))
         (if pts
           (list (list (apply 'min (mapcar 'car pts)) (apply 'min (mapcar 'cadr pts)) 0.0)
                 (list (apply 'max (mapcar 'car pts)) (apply 'max (mapcar 'cadr pts)) 0.0))
         )
        )
        ((= eType "CIRCLE")
         (setq p10 (cdr (assoc 10 d))
               r   (cdr (assoc 40 d)))
         (list (list (- (car p10) r) (- (cadr p10) r) (caddr p10))
               (list (+ (car p10) r) (+ (cadr p10) r) (caddr p10)))
        )
        ((= eType "ARC")
         (setq p10 (cdr (assoc 10 d))
               r   (cdr (assoc 40 d)))
         (list (list (- (car p10) r) (- (cadr p10) r) (caddr p10))
               (list (+ (car p10) r) (+ (cadr p10) r) (caddr p10)))
        )
        ((or (= eType "POINT") (= eType "INSERT") (= eType "TEXT") (= eType "MTEXT"))
         (if (setq p10 (cdr (assoc 10 d)))
           (list p10 p10)
           nil
         )
        )
        (t
         (if (setq p10 (cdr (assoc 10 d)))
           (list p10 p10)
           nil
         )
        )
      )
    )
    nil
  )
)

;; CadSetup:GetBoundingBox - Gets WCS bounding box for a VLA-Object or ENAME
;; Returns ((minX minY minZ) (maxX maxY maxZ)) or nil
(defun CadSetup:GetBoundingBox (obj / minPt maxPt res)
  (cond
    ((and obj (= (type obj) 'VLA-OBJECT))
     (setq res (vl-catch-all-apply 'vla-getboundingbox (list obj 'minPt 'maxPt)))
     (if (not (vl-catch-all-error-p res))
       (list (vlax-safearray->list minPt)
             (vlax-safearray->list maxPt))
       (if (vlax-vla-object->ename obj)
         (CadSetup:GetBoundingBoxDxf (vlax-vla-object->ename obj))
         nil
       )
     )
    )
    ((and obj (= (type obj) 'ENAME))
     (setq res (vl-catch-all-apply 'vlax-ename->vla-object (list obj)))
     (if (and (not (vl-catch-all-error-p res)) res)
       (CadSetup:GetBoundingBox res)
       (CadSetup:GetBoundingBoxDxf obj)
     )
    )
    (t nil)
  )
)

;; CadSetup:GetBoundingBoxUcs - Collective Bounding Box in Current UCS
;; Takes an AutoCAD selection set (PICKSET) and computes the tightest
;; bounding rectangle transformed into the active UCS coordinate frame.
;; Returns ((minX minY minZ) (maxX maxY maxZ)) in UCS or nil.
(defun CadSetup:GetBoundingBoxUcs (ss / i ent obj bbox pMinW pMaxW ptU allUcsX allUcsY)
  (if (and ss (= (type ss) 'PICKSET) (> (sslength ss) 0))
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i)
              obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
        (setq bbox (if (and (not (vl-catch-all-error-p obj)) obj)
                     (CadSetup:GetBoundingBox obj)
                     (CadSetup:GetBoundingBox ent)))
        (if bbox
          (progn
            (setq pMinW (car bbox)
                  pMaxW (cadr bbox))
            ;; Evaluate all 4 box corners transformed from WCS (0) to Current UCS (1)
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
        (list
          (list (apply 'min allUcsX) (apply 'min allUcsY) 0.0)
          (list (apply 'max allUcsX) (apply 'max allUcsY) 0.0)
        )
        nil
      )
    )
    nil
  )
)

;; ===========================================================================
;; CURVE LENGTH INQUIRIES
;; ===========================================================================

;; CadSetup:GetCurveLength - Returns the true length of any curve object
(defun CadSetup:GetCurveLength (obj / endParam len)
  (if (and obj (= (type obj) 'VLA-OBJECT))
    (progn
      (setq endParam (vl-catch-all-apply 'vlax-curve-getendparam (list obj)))
      (if (not (vl-catch-all-error-p endParam))
        (progn
          (setq len (vl-catch-all-apply 'vlax-curve-getdistatparam (list obj endParam)))
          (if (not (vl-catch-all-error-p len)) len 0.0)
        )
        0.0
      )
    )
    0.0
  )
)

;; CadSetup:GetTotalCurveLength - Sums length of all curve objects in selection set
(defun CadSetup:GetTotalCurveLength (ss / i ent obj total len)
  (setq total 0.0)
  (if (and ss (= (type ss) 'PICKSET))
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i)
              obj (vlax-ename->vla-object ent))
        (setq len (CadSetup:GetCurveLength obj))
        (setq total (+ total len))
        (setq i (1+ i))
      )
    )
  )
  total
)

;; ===========================================================================
;; POINT & VECTOR MATH
;; ===========================================================================

;; CadSetup:MidPoint - Returns the geometric midpoint between two points
(defun CadSetup:MidPoint (p1 p2)
  (if (and p1 p2)
    (list
      (/ (+ (car p1)  (car p2))  2.0)
      (/ (+ (cadr p1) (cadr p2)) 2.0)
      (/ (+ (if (caddr p1) (caddr p1) 0.0)
            (if (caddr p2) (caddr p2) 0.0)) 2.0)
    )
    nil
  )
)

;; ===========================================================================
;; ANNOTATIVE SCALE & MULTIPLIER UTILITIES
;; ===========================================================================

;; CadSetup:GetAnnoScaleRatio - Computes modelspace scale multiplier
;; Derives scale factor from CANNOSCALEVALUE (drawing units per paper unit),
;; falling back to DIMSCALE or 1.0.
(defun CadSetup:GetAnnoScaleRatio ( / cVal dimSc )
  (setq cVal (CadSetup:SafeGetVar "CANNOSCALEVALUE" nil))
  (cond
    ((and (numberp cVal) (> cVal 0.0))
     (/ 1.0 cVal))
    ((and (setq dimSc (CadSetup:SafeGetVar "DIMSCALE" nil)) (numberp dimSc) (> dimSc 0.0))
     dimSc)
    (t 1.0)
  )
)

;; CadSetup:GetAnnoScaleName - Returns active annotative scale name string
(defun CadSetup:GetAnnoScaleName ( / scName )
  (setq scName (CadSetup:SafeGetVar "CANNOSCALE" nil))
  (if (and scName (= (type scName) 'STR) (> (strlen scName) 0))
    scName
    "1:1"
  )
)

(if *CadSetup-Debug*
  (princ "\n[Helpers/Help_Geometry.lsp] Geometry, bounding box & curve math loaded.")
)
(princ)
