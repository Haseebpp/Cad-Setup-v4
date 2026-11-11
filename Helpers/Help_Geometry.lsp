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


;; CadSetup:GetScaleListNames - Returns list of registered annotative scale names
(defun CadSetup:GetScaleListNames ( / dict names item ent name )
  (setq dict (dictsearch (namedobjdict) "ACAD_SCALELIST"))
  (if dict
    (foreach item dict
      (if (= (car item) 350)
        (progn
          (setq ent (entget (cdr item)))
          (setq name (cdr (assoc 300 ent)))
          (if (and name (= (type name) 'STR))
            (setq names (cons name names))
          )
        )
      )
    )
  )
  (reverse names)
)

;; CadSetup:ScaleExistsP - Fast predicate checking if scale name exists in drawing
(defun CadSetup:ScaleExistsP (scaleName / names found)
  (if (and scaleName (= (type scaleName) 'STR))
    (progn
      (setq names (CadSetup:GetScaleListNames))
      (setq found nil)
      (foreach n names
        (if (= (strcase n) (strcase scaleName))
          (setq found T)
        )
      )
      found
    )
    nil
  )
)

;; CadSetup:EnsureAnnoScale - Ensures scale exists in drawing; registers it if missing
;; Returns T if newly registered, nil if it already existed
(defun CadSetup:EnsureAnnoScale (scaleName paperUnits drawingUnits / oldEcho ratioStr colonPos pStr dStr)
  (if (not (CadSetup:ScaleExistsP scaleName))
    (progn
      (setq oldEcho (getvar "CMDECHO"))
      (setvar "CMDECHO" 0)
      (if (and (or (null paperUnits) (null drawingUnits) (and (= paperUnits 1) (= drawingUnits 1)))
               (setq colonPos (vl-string-search ":" scaleName)))
        (progn
          (setq pStr (substr scaleName 1 colonPos)
                dStr (substr scaleName (+ colonPos 2)))
          (setq paperUnits (atof pStr)
                drawingUnits (atof dStr))
        )
      )
      (if (or (null paperUnits) (<= paperUnits 0.0)) (setq paperUnits 1.0))
      (if (or (null drawingUnits) (<= drawingUnits 0.0)) (setq drawingUnits 1.0))
      (setq ratioStr (strcat (rtos paperUnits 2 0) ":" (rtos drawingUnits 2 0)))
      (command "._-scalelistedit" "_add" scaleName ratioStr "_exit")
      (while (> (getvar "CMDACTIVE") 0)
        (command "_exit")
      )
      (setvar "CMDECHO" oldEcho)
      T
    )
    nil
  )
)

;; CadSetup:SetAnnoScale - Safely applies active annotative scale (CANNOSCALE)
(defun CadSetup:SetAnnoScale (scaleName / res)
  (if (and scaleName (= (type scaleName) 'STR))
    (progn
      (setq res (vl-catch-all-apply 'setvar (list "CANNOSCALE" scaleName)))
      (not (vl-catch-all-error-p res))
    )
    nil
  )
)

;; CadSetup:GetActiveViewportObject - Gets active floating viewport VLA-object
(defun CadSetup:GetActiveViewportObject ( / doc actVP ss )
  (setq doc (if (boundp 'CadSetup:GetDoc)
              (CadSetup:GetDoc)
              (vla-get-activedocument (vlax-get-acad-object))))
  (if (and doc (= (getvar "TILEMODE") 0) (> (getvar "CVPORT") 1))
    (progn
      ;; Primary: ActivePViewport property
      (setq actVP (vl-catch-all-apply 'vla-get-activepviewport (list doc)))
      (if (and (not (vl-catch-all-error-p actVP)) actVP)
        actVP
        ;; Fallback: query viewport matching current layout & CVPORT ID
        (progn
          (setq ss (ssget "_X" (list '(0 . "VIEWPORT")
                                     (cons 410 (getvar "CTAB"))
                                     (cons 69 (getvar "CVPORT")))))
          (if (and ss (> (sslength ss) 0))
            (vlax-ename->vla-object (ssname ss 0))
            nil
          )
        )
      )
    )
    nil
  )
)

;; CadSetup:CalculateZoomScaleRatio - Derives raw zoom scale ratio from current view
;; Returns raw float ratio or nil if in pure Paper Space (cvport=1)
(defun CadSetup:CalculateZoomScaleRatio ( / tile cv actVP custSc vpH viewSz scrn wScreen hScreen viewW viewH sc )
  (setq tile (getvar "TILEMODE")
        cv   (getvar "CVPORT"))
  (cond
    ;; 1. Layout Viewport (Floating Model Space: tilemode=0 and cvport>1)
    ((and (= tile 0) (> cv 1))
     (setq actVP (CadSetup:GetActiveViewportObject))
     (cond
       ;; A. Query CustomScale on Viewport object
       ((and actVP
             (setq custSc (vl-catch-all-apply 'vla-get-customscale (list actVP)))
             (not (vl-catch-all-error-p custSc))
             (numberp custSc)
             (> custSc 1e-6))
        (/ 1.0 custSc))
       ;; B. Query Viewport Paper Height vs Model VIEWSIZE
       ((and actVP
             (setq vpH (vl-catch-all-apply 'vla-get-height (list actVP)))
             (not (vl-catch-all-error-p vpH))
             (numberp vpH)
             (> vpH 1e-6)
             (setq viewSz (getvar "VIEWSIZE"))
             (> viewSz 1e-6))
        (/ viewSz vpH))
       ;; C. Fallback: VIEWSIZE vs default viewport height (150mm)
       (t
        (setq viewSz (getvar "VIEWSIZE"))
        (if (and (numberp viewSz) (> viewSz 0.0))
          (/ viewSz 150.0)
          1.0
        ))
     )
    )

    ;; 2. Model Space (tilemode=1 or cvport>1 on Model tab)
    ((or (= tile 1) (and (= tile 0) (> cv 1) (= (strcase (getvar "CTAB")) "MODEL")))
     (setq viewH   (getvar "VIEWSIZE")
           scrn    (getvar "SCREENSIZE")
           wScreen (float (car scrn))
           hScreen (float (cadr scrn)))
     (if (and (numberp viewH) (> viewH 0.0) (> hScreen 0.0))
       (progn
         (setq viewW (* viewH (/ wScreen hScreen)))
         ;; ISO A3 Sheet reference bounds (420mm x 297mm)
         (if (>= viewW viewH)
           (setq sc (max (/ viewW 420.0) (/ viewH 297.0)))
           (setq sc (max (/ viewW 297.0) (/ viewH 420.0)))
         )
         (if (< sc 1.0) 1.0 sc)
       )
       1.0
     )
    )

    ;; 3. Paper Space outside viewports (tilemode=0 and cvport=1)
    (t nil)
  )
)

;; CadSetup:CalculateZoomScaleInteger - Calculates nearest integer scale (1:N, minimum 1)
(defun CadSetup:CalculateZoomScaleInteger (rawRatio)
  (if (and rawRatio (numberp rawRatio) (> rawRatio 0.0))
    (max 1 (fix (+ rawRatio 0.5)))
    1
  )
)

;; CadSetup:ExtractScaleNameFromContext - Extracts scale name string from scale record or sub-entity
(defun CadSetup:ExtractScaleNameFromContext (subEnt / subData name ref)
  (if (and subEnt (= (type subEnt) 'ENAME))
    (progn
      (setq subData (entget subEnt))
      (cond
        ((setq name (cdr (assoc 300 subData)))
         name)
        ((and (setq ref (cdr (assoc 340 subData)))
              (setq name (cdr (assoc 300 (entget ref)))))
         name)
        (t nil)
      )
    )
    nil
  )
)

;; CadSetup:GetEntityAnnoScales - Retrieves list of all annotative scale names assigned to an entity
(defun CadSetup:GetEntityAnnoScales (ent / entObj exd xdEn cdmDict scalesDict itm subEnt scaleName scaleList safeCnt)
  (if (and ent (= (type ent) 'ENAME))
    (progn
      (setq entObj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
      (if (and (not (vl-catch-all-error-p entObj)) entObj)
        (if (= (vlax-get-property entObj 'HasExtensionDictionary) :vlax-true)
          (progn
            (setq exd (vl-catch-all-apply 'vla-getextensiondictionary (list entObj)))
            (if (and (not (vl-catch-all-error-p exd)) exd)
              (setq xdEn (vlax-vla-object->ename exd))
            )
          )
        )
      )
      (if (not xdEn)
        (setq xdEn (cdr (assoc 360 (entget ent))))
      )
      (if (and xdEn
               (setq cdmDict (dictsearch xdEn "AcDbContextDataManager"))
               (setq scalesDict (dictsearch (cdr (assoc -1 cdmDict)) "ACDB_ANNOTATIONSCALES")))
        (progn
          (setq itm nil
                safeCnt 0)
          (while (and (< safeCnt 500)
                      (setq itm (dictnext (cdr (assoc -1 scalesDict)) (null itm))))
            (setq safeCnt (1+ safeCnt))
            (if (setq subEnt (cdr (assoc 340 itm)))
              (progn
                (setq scaleName (CadSetup:ExtractScaleNameFromContext subEnt))
                (if (and scaleName (not (member scaleName scaleList)))
                  (setq scaleList (cons scaleName scaleList))
                )
              )
            )
          )
        )
      )
      (reverse scaleList)
    )
    nil
  )
)

;; CadSetup:GetViewportScaleName - Resolves exact annotative scale name of a Viewport entity or VLA-Object
(defun CadSetup:GetViewportScaleName (vpEnt / vpObj annoProp scName cScale dict item sEnt sName pUnit dUnit matchName intN scaleStr)
  (if vpEnt
    (progn
      (setq vpObj (if (= (type vpEnt) 'VLA-OBJECT)
                    vpEnt
                    (vl-catch-all-apply 'vlax-ename->vla-object (list vpEnt))))
      (if (and vpObj (not (vl-catch-all-error-p vpObj)))
        (progn
          ;; 1. Try reading AnnotationScale property directly
          (if (vlax-property-available-p vpObj 'AnnotationScale)
            (setq annoProp (vl-catch-all-apply 'vlax-get-property (list vpObj 'AnnotationScale)))
          )
          (if (and annoProp (not (vl-catch-all-error-p annoProp)))
            (cond
              ((= (type annoProp) 'VLA-OBJECT)
               (setq scName (vl-catch-all-apply 'vla-get-name (list annoProp)))
               (if (and scName (not (vl-catch-all-error-p scName)) (= (type scName) 'STR))
                 (setq matchName scName)
               )
              )
              ((= (type annoProp) 'STR)
               (setq matchName annoProp)
              )
            )
          )

          ;; 2. Match CustomScale against ACAD_SCALELIST dictionary
          (if (not matchName)
            (progn
              (setq cScale (vl-catch-all-apply 'vla-get-customscale (list vpObj)))
              (if (and (not (vl-catch-all-error-p cScale)) (numberp cScale) (> cScale 1e-6))
                (progn
                  (setq dict (dictsearch (namedobjdict) "ACAD_SCALELIST"))
                  (if dict
                    (foreach item dict
                      (if (and (not matchName) (= (car item) 350))
                        (progn
                          (setq sEnt  (entget (cdr item)))
                          (setq sName (cdr (assoc 300 sEnt))
                                pUnit (cdr (assoc 140 sEnt))
                                dUnit (cdr (assoc 141 sEnt)))
                          (if (and (numberp pUnit) (numberp dUnit) (> dUnit 0.0))
                            (if (equal cScale (/ (float pUnit) (float dUnit)) 1e-4)
                              (setq matchName sName)
                            )
                          )
                        )
                      )
                    )
                  )
                  ;; 3. Calculate 1:N integer scale if not found in dictionary
                  (if (not matchName)
                    (progn
                      (setq intN (max 1 (fix (+ (/ 1.0 cScale) 0.5))))
                      (setq scaleStr (strcat "1:" (itoa intN)))
                      (CadSetup:EnsureAnnoScale scaleStr 1 intN)
                      (setq matchName scaleStr)
                    )
                  )
                )
              )
            )
          )
        )
      )
      matchName
    )
    nil
  )
)

;; CadSetup:SetEntitySingleAnnoScale - Sets entity's annotative scale to targetScale and deletes all other scales
(defun CadSetup:SetEntitySingleAnnoScale (ent targetScale / vlaEnt curScales otherScales s)
  (if (and ent (= (type ent) 'ENAME) targetScale (= (type targetScale) 'STR))
    (progn
      ;; 1. Ensure entity is annotative
      (setq vlaEnt (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
      (if (and (not (vl-catch-all-error-p vlaEnt)) vlaEnt)
        (if (vlax-property-available-p vlaEnt 'Annotative)
          (if (/= (vla-get-annotative vlaEnt) :vlax-true)
            (vla-put-annotative vlaEnt :vlax-true)
          )
        )
      )

      ;; 2. Ensure targetScale is defined in drawing scale list
      (if (not (CadSetup:ScaleExistsP targetScale))
        (CadSetup:EnsureAnnoScale targetScale nil nil)
      )

      ;; 3. Query existing scales attached to entity
      (setq curScales (CadSetup:GetEntityAnnoScales ent))

      ;; 4. If targetScale is not already attached, add it
      (if (not (vl-some '(lambda (x) (= (strcase x) (strcase targetScale))) curScales))
        (progn
          (command "._-objectscale" ent "" "_Add" targetScale "")
          (while (> (getvar "CMDACTIVE") 0)
            (command "")
          )
        )
      )

      ;; 5. Refresh attached scales list and delete all other scales
      (setq curScales (CadSetup:GetEntityAnnoScales ent))
      (setq otherScales (vl-remove-if '(lambda (x) (= (strcase x) (strcase targetScale))) curScales))

      (foreach s otherScales
        (command "._-objectscale" ent "" "_Delete" s "")
        (while (> (getvar "CMDACTIVE") 0)
          (command "")
        )
      )

      (entupd ent)
      T
    )
    nil
  )
)

(if *CadSetup-Debug*
  (princ "\n[Helpers/Help_Geometry.lsp] Geometry, bounding box & curve math loaded.")
)
(princ)
