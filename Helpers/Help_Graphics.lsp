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
          (CadSetup:EnsureLayerUnlocked layName)
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
          (CadSetup:EnsureLayerUnlocked layName)
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
          (CadSetup:EnsureLayerUnlocked layName)
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

      ;; Ensure target layer exists and is unlocked
      (if (not (tblsearch "LAYER" targetLay))
        (vl-catch-all-apply 'vla-Add (list (vla-get-Layers acadDoc) targetLay))
      )
      (CadSetup:EnsureLayerUnlocked targetLay)

      (setq res
        (vl-catch-all-apply
          (function
            (lambda (/ h)
              (setq h (vla-AddHatch blk 1 pat :vlax-true))
              (if (/= pat "SOLID")
                (progn
                  (vla-put-PatternScale h sc)
                  (if (/= rot 0.0) (vla-put-PatternAngle h angRad))
                )
              )
              (vla-AppendOuterLoop h arr)
              (vla-Evaluate h)
              (vla-put-Layer h targetLay)
              (if (numberp col) (vla-put-Color h col))
              (CadSetup:SendToBack h)
              h
            )
          )
        )
      )
      (if (vl-catch-all-error-p res) nil res)
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
