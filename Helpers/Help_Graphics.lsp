;;; ==========================================================================
;;; Help_Graphics.lsp - 2D Drawing Primitives, Text, Hatches & Draw Order
;;; Layer: Helpers (Level 1)
;;; ==========================================================================

(vl-load-com)

;; ===========================================================================
;; 1. 2D GEOMETRIC DRAWING PRIMITIVES
;; ===========================================================================

;; CadSetup:DrawBox - Draws closed 2D Lightweight Polyline rectangle between two corner points
(defun CadSetup:DrawBox (p1 p2 layName / mSpace x1 y1 x2 y2 pts poly)
  (setq mSpace (CadSetup:GetModelSpace))
  (if (and mSpace p1 p2)
    (progn
      (setq x1 (car p1)  y1 (cadr p1)
            x2 (car p2)  y2 (cadr p2))
      (setq pts (vlax-make-safearray vlax-vbDouble '(0 . 7)))
      (vlax-safearray-fill pts (list x1 y1 x2 y1 x2 y2 x1 y2))
      (setq poly (vla-AddLightWeightPolyline mSpace pts))
      (vla-put-Closed poly :vlax-true)
      (if (and layName (tblsearch "LAYER" layName))
        (progn
          (if (boundp 'CadSetup:EnsureLayerReady)
            (CadSetup:EnsureLayerReady layName)
          )
          (vla-put-Layer poly layName)
        )
        (vla-put-Layer poly "0")
      )
      poly
    )
    nil
  )
)

;; CadSetup:DrawLine - Draws 2D/3D Line entity between two points
(defun CadSetup:DrawLine (p1 p2 layName / mSpace p1Arr p2Arr lineObj)
  (setq mSpace (CadSetup:GetModelSpace))
  (if (and mSpace p1 p2)
    (progn
      (setq p1Arr (vlax-3d-point p1)
            p2Arr (vlax-3d-point p2))
      (setq lineObj (vla-AddLine mSpace p1Arr p2Arr))
      (if (and layName (tblsearch "LAYER" layName))
        (progn
          (if (boundp 'CadSetup:EnsureLayerReady)
            (CadSetup:EnsureLayerReady layName)
          )
          (vla-put-Layer lineObj layName)
        )
        (vla-put-Layer lineObj "0")
      )
      lineObj
    )
    nil
  )
)

;; ===========================================================================
;; 2. TEXT & ANNOTATION PRIMITIVES
;; ===========================================================================

;; CadSetup:AddText - Places single-line text entity with alignment, height, color, and layer
(defun CadSetup:AddText (pt str hgt colNum layName align / mSpace safeStr txtObj)
  (setq mSpace (CadSetup:GetModelSpace))
  (if (and mSpace pt)
    (progn
      (setq safeStr (if str (vl-princ-to-string str) ""))
      (setq txtObj (vla-AddText mSpace safeStr (vlax-3d-point pt) hgt))
      (if (and layName (tblsearch "LAYER" layName))
        (progn
          (if (boundp 'CadSetup:EnsureLayerReady)
            (CadSetup:EnsureLayerReady layName)
          )
          (vla-put-Layer txtObj layName)
        )
        (vla-put-Layer txtObj "0")
      )
      (if (numberp colNum)
        (vla-put-Color txtObj colNum)
        (vla-put-Color txtObj 7)
      )
      (cond
        ((= align "MIDDLE-LEFT")
         (vla-put-Alignment txtObj acAlignmentMiddleLeft)
         (vla-put-TextAlignmentPoint txtObj (vlax-3d-point pt)))
        ((= align "CENTER")
         (vla-put-Alignment txtObj acAlignmentCenter)
         (vla-put-TextAlignmentPoint txtObj (vlax-3d-point pt)))
      )
      txtObj
    )
    nil
  )
)

;; ===========================================================================
;; 3. TRUECOLOR RGB STYLING
;; ===========================================================================

;; CadSetup:SetEntityRGB - Safely applies 24-bit TrueColor (RGB) override to any entity/ename via ActiveX & DXF
(defun CadSetup:SetEntityRGB (ent rgbList / r g b rgbVal elist enm tcObj)
  (if (and ent rgbList (= (length rgbList) 3))
    (progn
      (setq r (min 255 (max 0 (fix (nth 0 rgbList))))
            g (min 255 (max 0 (fix (nth 1 rgbList))))
            b (min 255 (max 0 (fix (nth 2 rgbList))))
            rgbVal (+ (* r 65536) (* g 256) b))
      ;; ActiveX TrueColor Assignment
      (if (= (type ent) 'VLA-OBJECT)
        (vl-catch-all-apply
          (function
            (lambda ()
              (setq tcObj (vla-get-TrueColor ent))
              (vla-SetRGB tcObj r g b)
              (vla-put-TrueColor ent tcObj)
            )
          )
        )
      )
      ;; DXF Group 420 Direct Modification
      (setq enm (if (= (type ent) 'VLA-OBJECT) (vlax-vla-object->ename ent) ent))
      (if (and enm (entget enm))
        (progn
          (setq elist (entget enm))
          (if (assoc 420 elist)
            (setq elist (subst (cons 420 rgbVal) (assoc 420 elist) elist))
            (setq elist (append elist (list (cons 420 rgbVal))))
          )
          (entmod elist)
          (entupd enm)
        )
      )
      T
    )
    nil
  )
)

;; ===========================================================================
;; 4. DRAW ORDER MANIPULATION
;; ===========================================================================

;; CadSetup:SendToBack - Moves entity to absolute back in draw order
(defun CadSetup:SendToBack (obj / mSpace extDict sortTable arr enm res vObj)
  (setq mSpace (CadSetup:GetModelSpace)
        vObj   (cond ((= (type obj) 'VLA-OBJECT) obj)
                     ((= (type obj) 'ENAME) (vlax-ename->vla-object obj))
                     (t nil)))
  (setq res
    (if (and mSpace vObj)
      (vl-catch-all-apply
        (function
          (lambda ()
            (setq extDict (vla-GetExtensionDictionary mSpace))
            (setq sortTable
              (vl-catch-all-apply
                (function (lambda () (vla-Item extDict "ACAD_SORTENTS")))))
            (if (or (vl-catch-all-error-p sortTable) (null sortTable))
              (setq sortTable
                (vl-catch-all-apply
                  (function (lambda () (vla-AddObject extDict "ACAD_SORTENTS" "AcDbSortentsTable"))))))
            (if (and sortTable (not (vl-catch-all-error-p sortTable)))
              (progn
                (setq arr (vlax-make-safearray vlax-vbObject '(0 . 0)))
                (vlax-safearray-fill arr (list vObj))
                (vla-MoveToBottom sortTable arr)
                T
              )
              nil
            )
          )
        )
      )
    )
  )
  (if (or (vl-catch-all-error-p res) (not res))
    (progn
      (setq enm (cond ((= (type obj) 'VLA-OBJECT) (vlax-vla-object->ename obj))
                      ((= (type obj) 'ENAME) obj)
                      (t nil)))
      (if enm
        (if (vl-symbol-value 'command-s)
          (vl-catch-all-apply 'command-s (list "._draworder" enm "" "_back"))
          (vl-catch-all-apply 'command (list "._draworder" enm "" "_back"))
        )
      )
    )
  )
  T
)

;; CadSetup:BringToFront - Moves entity to absolute front in draw order
(defun CadSetup:BringToFront (obj / mSpace extDict sortTable arr enm res vObj)
  (setq mSpace (CadSetup:GetModelSpace)
        vObj   (cond ((= (type obj) 'VLA-OBJECT) obj)
                     ((= (type obj) 'ENAME) (vlax-ename->vla-object obj))
                     (t nil)))
  (setq res
    (if (and mSpace vObj)
      (vl-catch-all-apply
        (function
          (lambda ()
            (setq extDict (vla-GetExtensionDictionary mSpace))
            (setq sortTable
              (vl-catch-all-apply
                (function (lambda () (vla-Item extDict "ACAD_SORTENTS")))))
            (if (or (vl-catch-all-error-p sortTable) (null sortTable))
              (setq sortTable
                (vl-catch-all-apply
                  (function (lambda () (vla-AddObject extDict "ACAD_SORTENTS" "AcDbSortentsTable"))))))
            (if (and sortTable (not (vl-catch-all-error-p sortTable)))
              (progn
                (setq arr (vlax-make-safearray vlax-vbObject '(0 . 0)))
                (vlax-safearray-fill arr (list vObj))
                (vla-MoveToTop sortTable arr)
                T
              )
              nil
            )
          )
        )
      )
    )
  )
  (if (or (vl-catch-all-error-p res) (not res))
    (progn
      (setq enm (cond ((= (type obj) 'VLA-OBJECT) (vlax-vla-object->ename obj))
                      ((= (type obj) 'ENAME) obj)
                      (t nil)))
      (if enm
        (if (vl-symbol-value 'command-s)
          (vl-catch-all-apply 'command-s (list "._draworder" enm "" "_front"))
          (vl-catch-all-apply 'command (list "._draworder" enm "" "_front"))
        )
      )
    )
  )
  T
)

;; ===========================================================================
;; 5. ASSOCIATIVE HATCH GENERATOR
;; ===========================================================================

;; CadSetup:CreateHatch - Pure ActiveX Associative Hatch Generator with Rotation, Scale & Send-To-Back
(defun CadSetup:CreateHatch (poly pat sc rot col targetLay / acadDoc blk arr res angRad)
  (setq acadDoc   (CadSetup:GetDoc)
        blk       (CadSetup:GetModelSpace)
        pat       (if (and pat (/= pat "")) (strcase (vl-princ-to-string pat)) "SOLID")
        targetLay (if (and targetLay (/= targetLay ""))
                    targetLay
                    (if (= pat "SOLID") "R-HTCH-SOLI" "R-HTCH-GENR"))
        sc        (if (and (numberp sc) (> sc 0)) sc 1.0)
        rot       (if (numberp rot) rot 0.0)
        angRad    (* rot (/ pi 180.0)))

  (if (and acadDoc blk poly (= (type poly) 'VLA-OBJECT))
    (progn
      (setq arr (vlax-safearray-fill (vlax-make-safearray vlax-vbObject '(0 . 0)) (list poly)))

      ;; Ensure target layer exists and is unlocked via database
      (if (and targetLay (not (tblsearch "LAYER" targetLay)))
        (if (boundp 'CadSetup:EnsureLayerFromDb)
          (CadSetup:EnsureLayerFromDb targetLay)
        )
      )
      (if (not (tblsearch "LAYER" targetLay))
        (setq targetLay "0")
      )
      (if (boundp 'CadSetup:EnsureLayerReady)
        (CadSetup:EnsureLayerReady targetLay)
      )

      (setq res
        (vl-catch-all-apply
          (function
            (lambda (/ h hRes)
              (setq h (vla-AddHatch blk 1 pat :vlax-true))
              (if (/= pat "SOLID")
                (progn
                  (vla-put-PatternScale h sc)
                  (if (/= rot 0.0) (vla-put-PatternAngle h angRad))
                )
              )
              (setq hRes (vl-catch-all-apply 'vla-AppendOuterLoop (list h arr)))
              (if (vl-catch-all-error-p hRes)
                (progn
                  (vl-catch-all-apply 'vla-delete (list h))
                  nil
                )
                (progn
                  (setq hRes (vl-catch-all-apply 'vla-Evaluate (list h)))
                  (if (vl-catch-all-error-p hRes)
                    (progn
                      (vl-catch-all-apply 'vla-delete (list h))
                      nil
                    )
                    (progn
                      (vla-put-Layer h targetLay)
                      (if (numberp col) (vla-put-Color h col))
                      (CadSetup:SendToBack h)
                      h
                    )
                  )
                )
              )
            )
          )
        )
      )
      (if (or (vl-catch-all-error-p res) (null res)) nil res)
    )
    nil
  )
)

;; CadSetup:IsClosedBoundary - Checks if an entity is a closed 2D boundary
(defun CadSetup:IsClosedBoundary (ent / obj oName startAng endAng)
  (setq obj (cond
              ((= (type ent) 'VLA-OBJECT) ent)
              ((= (type ent) 'ENAME) (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
              (t nil)))
  (if (and obj (not (vl-catch-all-error-p obj)) (= (type obj) 'VLA-OBJECT))
    (progn
      (setq oName (vl-catch-all-apply 'vlax-get-property (list obj 'ObjectName)))
      (if (vl-catch-all-error-p oName)
        nil
        (cond
          ;; Lightweight Polyline (LWPOLYLINE)
          ((= oName "AcDbPolyline")
           (= (vlax-get-property obj 'Closed) :vlax-true))

          ;; 2D Polyline (Classic POLYLINE)
          ((= oName "AcDb2dPolyline")
           (= (vlax-get-property obj 'Closed) :vlax-true))

          ;; Circle - Always closed
          ((= oName "AcDbCircle")
           T)

          ;; Ellipse - Closed if full 360 degrees
          ((= oName "AcDbEllipse")
           (setq startAng (vl-catch-all-apply 'vlax-get-property (list obj 'StartAngle))
                 endAng   (vl-catch-all-apply 'vlax-get-property (list obj 'EndAngle)))
           (and (not (vl-catch-all-error-p startAng))
                (not (vl-catch-all-error-p endAng))
                (or (and (equal startAng 0.0 1e-4) (equal endAng (* 2.0 pi) 1e-4))
                    (equal (abs (- endAng startAng)) (* 2.0 pi) 1e-4))))

          ;; Spline - Closed if Closed property is true
          ((= oName "AcDbSpline")
           (= (vlax-get-property obj 'Closed) :vlax-true))

          (t nil)
        )
      )
    )
    nil
  )
)

;; CadSetup:EnsureClosedBoundary - Verifies closed boundary; if polyline ends match, closes it
(defun CadSetup:EnsureClosedBoundary (ent / obj oName coords n pStart pEnd)
  (setq obj (cond
              ((= (type ent) 'VLA-OBJECT) ent)
              ((= (type ent) 'ENAME) (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
              (t nil)))
  (if (and obj (not (vl-catch-all-error-p obj)) (= (type obj) 'VLA-OBJECT))
    (progn
      (if (CadSetup:IsClosedBoundary obj)
        obj
        (progn
          (setq oName (vl-catch-all-apply 'vlax-get-property (list obj 'ObjectName)))
          (if (and (not (vl-catch-all-error-p oName)) (= oName "AcDbPolyline"))
            (progn
              (setq coords (vl-catch-all-apply 'vlax-get-property (list obj 'Coordinates)))
              (if (and (not (vl-catch-all-error-p coords))
                       (= (type (vlax-variant-value coords)) 'SAFEARRAY))
                (progn
                  (setq coords (vlax-safearray->list (vlax-variant-value coords)))
                  (setq n (length coords))
                  (if (>= n 6) ; At least 3 vertices
                    (progn
                      (setq pStart (list (car coords) (cadr coords))
                            pEnd   (list (nth (- n 2) coords) (last coords)))
                      (if (equal pStart pEnd 1e-4)
                        (progn
                          (vl-catch-all-apply 'vla-put-Closed (list obj :vlax-true))
                          obj
                        )
                        nil
                      )
                    )
                    nil
                  )
                )
                nil
              )
            )
            nil
          )
        )
      )
    )
    nil
  )
)

;; CadSetup:RemoveExistingHatchesForBoundary - Removes prior hatches to avoid stacking
(defun CadSetup:RemoveExistingHatchesForBoundary (polyObj / ent elist owner hEname hEnt hObj lDataH)
  (setq polyObj (cond
                  ((= (type polyObj) 'VLA-OBJECT) polyObj)
                  ((= (type polyObj) 'ENAME) (vl-catch-all-apply 'vlax-ename->vla-object (list polyObj)))
                  (t nil)))
  (if (and polyObj (not (vl-catch-all-error-p polyObj)) (= (type polyObj) 'VLA-OBJECT))
    (progn
      ;; 1. Delete hatch linked via LData handle
      (setq lDataH (vl-catch-all-apply 'vlax-ldata-get (list polyObj "CadSetup_Hatch")))
      (if (and (not (vl-catch-all-error-p lDataH))
               (stringp lDataH)
               (setq hEname (handent lDataH)))
        (progn
          (setq hObj (vl-catch-all-apply 'vlax-ename->vla-object (list hEname)))
          (if (and hObj (not (vl-catch-all-error-p hObj)) (= (type hObj) 'VLA-OBJECT))
            (vl-catch-all-apply 'vla-delete (list hObj))
          )
          (vl-catch-all-apply 'vlax-ldata-delete (list polyObj "CadSetup_Hatch"))
        )
      )

      ;; 2. Inspect persistent reactors on boundary pointing to HATCH
      (setq ent (vl-catch-all-apply 'vlax-vla-object->ename (list polyObj)))
      (if (and ent (not (vl-catch-all-error-p ent)) (setq elist (entget ent)))
        (progn
          (setq owner (cdr (assoc 330 elist))) ; BlockTableRecord owner
          (foreach pair elist
            (if (and (= (car pair) 330) (/= (cdr pair) owner))
              (progn
                (setq hEname (cdr pair))
                (setq hEnt (vl-catch-all-apply 'entget (list hEname)))
                (if (and (not (vl-catch-all-error-p hEnt))
                         hEnt
                         (= (cdr (assoc 0 hEnt)) "HATCH"))
                  (progn
                    (setq hObj (vl-catch-all-apply 'vlax-ename->vla-object (list hEname)))
                    (if (and hObj (not (vl-catch-all-error-p hObj)) (= (type hObj) 'VLA-OBJECT))
                      (vl-catch-all-apply 'vla-delete (list hObj))
                    )
                  )
                )
              )
            )
          )
        )
      )
      T
    )
    nil
  )
)

;; CadSetup:HatchBoundaryWithMaterial - Fills closed boundary with material hatch definition
(defun CadSetup:HatchBoundaryWithMaterial (boundaryObj matLayer /
                                           dbRow hPat hScl hRot hTrans hTargetLay hObj)
  (setq boundaryObj (CadSetup:EnsureClosedBoundary boundaryObj))
  (if (and boundaryObj
           matLayer
           (= (type matLayer) 'STR)
           (> (strlen matLayer) 0))
    (progn
      ;; Retrieve material hatch definition from Db_Layers.lsp
      (setq dbRow (if (boundp 'CadSetup:GetLayerData) (CadSetup:GetLayerData matLayer) nil))
      (if (null dbRow)
        (setq dbRow (assoc (strcase matLayer) (if (boundp '*CadSetup-Layers-Data*) *CadSetup-Layers-Data* nil)))
      )

      (if dbRow
        (setq hPat   (if (nth 6 dbRow) (vl-princ-to-string (nth 6 dbRow)) "NONE")
              hScl   (if (numberp (nth 7 dbRow)) (nth 7 dbRow) 1.0)
              hRot   (if (numberp (nth 8 dbRow)) (nth 8 dbRow) 0.0)
              hTrans (if (numberp (nth 9 dbRow)) (nth 9 dbRow) 0))
        (setq hPat "NONE" hScl 1.0 hRot 0.0 hTrans 0)
      )

      ;; Proceed only if pattern is valid and not "NONE"
      (if (and hPat
               (/= (strcase hPat) "NONE")
               (/= hPat ""))
        (progn
          ;; Target hatch layer: *DEFAULT-HPLAYER* or "R-HTCH-GENR"
          (setq hTargetLay (if (and (boundp '*DEFAULT-HPLAYER*) *DEFAULT-HPLAYER* (/= *DEFAULT-HPLAYER* ""))
                             *DEFAULT-HPLAYER*
                             "R-HTCH-GENR"))

          ;; Ensure target layer is ready (thawed, on, unlocked)
          (if (boundp 'CadSetup:EnsureLayerReady)
            (CadSetup:EnsureLayerReady hTargetLay)
          )

          ;; Clean replacement: remove any existing hatch on this boundary
          (CadSetup:RemoveExistingHatchesForBoundary boundaryObj)

          ;; Generate associative hatch
          (setq hObj (CadSetup:CreateHatch boundaryObj hPat hScl hRot nil hTargetLay))

          (if hObj
            (progn
              ;; Apply transparency if defined (> 0)
              (if (and (numberp hTrans) (> hTrans 0))
                (CadSetup:SetEntityTransparency hObj hTrans)
              )
              ;; Cross-reference handles via LData
              (vl-catch-all-apply 'vlax-ldata-put (list boundaryObj "CadSetup_Hatch" (vla-get-Handle hObj)))
              (vl-catch-all-apply 'vlax-ldata-put (list hObj "CadSetup_Boundary" (vla-get-Handle boundaryObj)))
              hObj
            )
            nil
          )
        )
        nil
      )
    )
    nil
  )
)

;; CadSetup:SetEntityTransparency - Applies object transparency (0 to 90) safely
(defun CadSetup:SetEntityTransparency (entObj trans / tVal ename res)
  (if (and entObj (numberp trans) (>= trans 0) (<= trans 90))
    (progn
      (setq tVal (fix trans))
      ;; Method 1: ActiveX property (AutoCAD 2011+)
      (setq res (vl-catch-all-apply 'vlax-put-property (list entObj 'EntityTransparency (itoa tVal))))
      (if (vl-catch-all-error-p res)
        ;; Fallback: CHPROP command
        (progn
          (setq ename (cond
                        ((= (type entObj) 'VLA-OBJECT) (vlax-vla-object->ename entObj))
                        ((= (type entObj) 'ENAME) entObj)
                        (t nil)))
          (if ename
            (vl-catch-all-apply
              (function (lambda ()
                (setvar "CMDECHO" 0)
                (command "_.CHPROP" ename "" "_Transparency" tVal "")
              ))
            )
          )
        )
      )
      T
    )
    nil
  )
)

(if *CadSetup-Debug*
  (princ "\n[Helpers/Help_Graphics.lsp] 2D graphics primitives, text, hatch & draw order loaded.")
)
(princ)
