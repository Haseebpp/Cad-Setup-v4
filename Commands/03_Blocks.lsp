;;; ==========================================================================
;;; 03_Blocks.lsp - Auto Block Generators & In-Place Fast Block Transforms
;;; Layer: Commands (Priority 03)
;;; Author   : Haseeb
;;; ==========================================================================

(vl-load-com)

;;; --------------------------------------------------------------------------
;;; 1. AUTO BLOCK CREATION & VERSION ENGINE (GB, DB, UB)
;;; --------------------------------------------------------------------------

;; CadSetup:FormatBlockTimestamp - Formats current date/time to "DD.MM.YYYY@HH.MM.SS"
(defun CadSetup:FormatBlockTimestamp ( / oldDimzin cdate dotPos dPart tPart y m d hr mi se )
  (setq oldDimzin (getvar "DIMZIN"))
  (setvar "DIMZIN" 0)
  (setq cdate (rtos (getvar "CDATE") 2 6))
  (setvar "DIMZIN" oldDimzin)
  (setq dotPos (vl-string-search "." cdate))
  (if dotPos
    (progn
      (setq dPart (substr cdate 1 dotPos)
            tPart (substr cdate (+ dotPos 2)))
      (while (< (strlen dPart) 8) (setq dPart (strcat "0" dPart)))
      (while (< (strlen tPart) 6) (setq tPart (strcat tPart "0")))
      (setq y  (substr dPart 1 4)
            m  (substr dPart 5 2)
            d  (substr dPart 7 2)
            hr (substr tPart 1 2)
            mi (substr tPart 3 2)
            se (substr tPart 5 2))
      (strcat d "." m "." y "@" hr "." mi "." se)
    )
    (menucmd "M=$(edtime,$(getvar,date),DD.MO.YYYY@HH.MM.SS)")
  )
)

;; CadSetup:GetNextDbHandle - Retrieves next CAD engine database handle padded to 8 hex chars with "$"
(defun CadSetup:GetNextDbHandle ( / h pEnt )
  (setq h (getvar "HANDSEED"))
  (if (or (null h) (not (= (type h) 'STR)) (= h ""))
    ;; Fallback: query engine via transient entity if HANDSEED is unpopulated in current runtime
    (if (setq pEnt (entmake '((0 . "POINT") (10 0.0 0.0 0.0))))
      (progn
        (setq pEnt (entlast))
        (setq h (cdr (assoc 5 (entget pEnt))))
        (entdel pEnt)
      )
    )
  )
  (if (or (null h) (= h ""))
    (setq h "00000001")
  )
  (setq h (strcase h))
  (while (< (strlen h) 8)
    (setq h (strcat "0" h))
  )
  (strcat "$" h)
)

;; CadSetup:FormatVersion - Formats integer version to 2-digit string prefixed with "v" (e.g. 1 -> "v01")
(defun CadSetup:FormatVersion (v / vStr)
  (setq vStr (itoa (max 1 v)))
  (if (< (strlen vStr) 2)
    (setq vStr (strcat "0" vStr))
  )
  (strcat "v" vStr)
)

;; CadSetup:BuildBlockName - Assembles standard block name: TYPE[$HANDLE.vXX]_MOD[...]__INIT[...]
(defun CadSetup:BuildBlockName (typeTag handleStr versionNum modStamp initStamp)
  (strcat typeTag "[" handleStr "." (CadSetup:FormatVersion versionNum) "]_MOD[" modStamp "]_INIT[" initStamp "]")
)

;; CadSetup:ParseBlockName - Deconstructs a standard block name into an alist or returns nil
(defun CadSetup:ParseBlockName (blkName / pMod pInit pB1 pDot pB2 typeTag handleStr verStr verNum modStamp initStamp)
  (setq pMod  (vl-string-search "_MOD[" blkName)
        pInit (vl-string-search "_INIT[" blkName)
        pB1   (vl-string-search "[" blkName)
        pDot  (vl-string-search "." blkName)
        pB2   (vl-string-search "]" blkName))
  (if (and pMod pInit pB1 pDot pB2
           (< pB1 pDot)
           (< pDot pB2)
           (< pB2 pMod)
           (< pMod pInit))
    (progn
      (setq typeTag   (substr blkName 1 pB1)
            handleStr (substr blkName (+ pB1 2) (- pDot (+ pB1 1)))
            verStr    (substr blkName (+ pDot 2) (- pB2 (+ pDot 1)))
            modStamp  (substr blkName (+ pMod 6) (- pInit (+ pMod 6 1)))
            initStamp (substr blkName (+ pInit 7) (- (strlen blkName) (+ pInit 7))))
      (if (and (> (strlen verStr) 1) (wcmatch (strcase verStr) "V*"))
        (setq verNum (atoi (substr verStr 2)))
        (setq verNum (atoi verStr))
      )
      (list (cons 'TYPE typeTag)
            (cons 'HANDLE handleStr)
            (cons 'VERSION verNum)
            (cons 'MOD modStamp)
            (cons 'INIT initStamp))
    )
    nil
  )
)

;; CadSetup:EnsureUniqueBlockName - Guarantees block name does not collide with existing definitions
(defun CadSetup:EnsureUniqueBlockName (blkName / baseName count)
  (if (tblsearch "BLOCK" blkName)
    (progn
      (setq baseName blkName
            count 1)
      (while (tblsearch "BLOCK" (strcat baseName "_" (itoa count)))
        (setq count (1+ count))
      )
      (strcat baseName "_" (itoa count))
    )
    blkName
  )
)

;; GB : Auto-create block at World Origin (0,0,0) with standard GRP name
(defun c:GB (/ *error* ss handle ts blkName oldCmd oldAtt)
  (defun *error* (msg)
    (if oldAtt (setvar 'attreq oldAtt))
    (if oldCmd (setvar 'cmdecho oldCmd))
    (if (boundp 'CadSetup:UndoReset) (CadSetup:UndoReset))
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[GB] Error: " msg))
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
      (princ "\nSelect objects to convert into Block at Origin [GB]: ")
      (setq ss (ssget))
    )
  )

  (if ss
    (progn
      (if (boundp 'CadSetup:UndoStart) (CadSetup:UndoStart))

      (setq handle  (CadSetup:GetNextDbHandle)
            ts      (CadSetup:FormatBlockTimestamp)
            blkName (CadSetup:BuildBlockName "GRP" handle 1 ts ts)
            blkName (CadSetup:EnsureUniqueBlockName blkName))

      ;; Define block at World Origin (0,0,0) and re-insert in place
      (command "._-block" blkName "_non" '(0.0 0.0 0.0) ss "")
      (command "._-insert" blkName "_non" '(0.0 0.0 0.0) 1.0 1.0 0.0)

      (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))
      (princ (strcat "\n[GB] Block created: \"" blkName "\" at World Origin (0,0,0)."))
    )
    (princ "\n[GB] No objects selected.")
  )

  (setvar 'attreq oldAtt)
  (setvar 'cmdecho oldCmd)
  (princ)
)

;; G : Fast alias for GB (Group Block at World Origin)
(defun c:G () (c:GB))

;; DB : Auto-create block at Bottom-Left (Current UCS) with standard DYN name
(defun c:DB (/ *error* ss bbox basePt handle ts blkName oldCmd oldAtt)
  (defun *error* (msg)
    (if oldAtt (setvar 'attreq oldAtt))
    (if oldCmd (setvar 'cmdecho oldCmd))
    (if (boundp 'CadSetup:UndoReset) (CadSetup:UndoReset))
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[DB] Error: " msg))
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
      (princ "\nSelect objects to convert into Block at Bottom-Left [DB]: ")
      (setq ss (ssget))
    )
  )

  (if ss
    (progn
      (if (boundp 'CadSetup:UndoStart) (CadSetup:UndoStart))

      ;; Collective bottom-left bounding coordinate in current UCS
      (setq bbox (if (boundp 'CadSetup:GetBoundingBoxUcs) (CadSetup:GetBoundingBoxUcs ss) nil))
      (if bbox
        (setq basePt (car bbox))
        (setq basePt (getvar 'insbase))
      )

      (setq handle  (CadSetup:GetNextDbHandle)
            ts      (CadSetup:FormatBlockTimestamp)
            blkName (CadSetup:BuildBlockName "DYN" handle 1 ts ts)
            blkName (CadSetup:EnsureUniqueBlockName blkName))

      ;; Define block at Bottom-Left base point and re-insert in place
      (command "._-block" blkName "_non" basePt ss "")
      (command "._-insert" blkName "_non" basePt 1.0 1.0 0.0)

      (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))
      (princ (strcat "\n[DB] Block created: \"" blkName "\" at Bottom-Left base point."))
    )
    (princ "\n[DB] No objects selected.")
  )

  (setvar 'attreq oldAtt)
  (setvar 'cmdecho oldCmd)
  (princ)
)

;; UB : Update Block / Bump Version (.v01 -> .v02), refresh MOD timestamp, preserve INIT & handle
(defun c:UB (/ *error* ent ss obj blkName parsed curVer newVer curInit newMod newName
               typeChoice newType handle oldCmd)
  (defun *error* (msg)
    (if oldCmd (setvar 'cmdecho oldCmd))
    (if (boundp 'CadSetup:UndoReset) (CadSetup:UndoReset))
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[UB] Error: " msg))
    )
    (princ)
  )

  (setq oldCmd (getvar 'cmdecho))
  (setvar 'cmdecho 0)

  ;; Check pre-selection or prompt for block reference
  (setq ss (ssget "_I" '((0 . "INSERT"))))
  (if (and ss (> (sslength ss) 0))
    (setq ent (ssname ss 0))
    (progn
      (setvar 'cmdecho oldCmd)
      (setq ent (car (entsel "\nSelect block reference to update/bump version [UB]: ")))
      (setvar 'cmdecho 0)
    )
  )

  (if ent
    (progn
      (setq d (entget ent))
      (if (= (cdr (assoc 0 d)) "INSERT")
        (progn
          ;; Retrieve effective block definition name (handles dynamic blocks via COM if available, DXF fallback)
          (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
          (setq blkName
            (if (and (not (vl-catch-all-error-p obj)) obj)
              (if (vlax-property-available-p obj 'EffectiveName)
                (vla-get-EffectiveName obj)
                (vla-get-Name obj)
              )
              (cdr (assoc 2 d))
            )
          )

          (setq parsed (CadSetup:ParseBlockName blkName))
          (if (boundp 'CadSetup:UndoStart) (CadSetup:UndoStart))

          (if parsed
            ;; 1. Standard GRP/DYN block: Bump version and update MOD timestamp
            (progn
              (setq curVer  (cdr (assoc 'VERSION parsed))
                    newVer  (1+ curVer)
                    curInit (cdr (assoc 'INIT parsed))
                    newMod  (CadSetup:FormatBlockTimestamp)
                    newName (CadSetup:BuildBlockName
                              (cdr (assoc 'TYPE parsed))
                              (cdr (assoc 'HANDLE parsed))
                              newVer
                              newMod
                              curInit))

              (command "._-rename" "_block" blkName newName)
              (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))
              (princ (strcat "\n[UB] Block version bumped to ." (CadSetup:FormatVersion newVer) ":"))
              (princ (strcat "\n     Old: \"" blkName "\""))
              (princ (strcat "\n     New: \"" newName "\""))
            )
            ;; 2. Non-Standard block: Convert to standard naming convention
            (progn
              (initget "GRP DYN Cancel")
              (setq typeChoice (getkword (strcat "\nBlock \"" blkName "\" is non-standard. Convert to [GRP/DYN/Cancel] <GRP>: ")))
              (if (or (null typeChoice) (= typeChoice "")) (setq typeChoice "GRP"))

              (if (/= typeChoice "Cancel")
                (progn
                  (setq handle  (CadSetup:GetNextDbHandle)
                        newMod  (CadSetup:FormatBlockTimestamp)
                        newName (CadSetup:BuildBlockName typeChoice handle 1 newMod newMod))

                  (command "._-rename" "_block" blkName newName)
                  (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))
                  (princ (strcat "\n[UB] Non-standard block converted to standard " typeChoice ":"))
                  (princ (strcat "\n     Old: \"" blkName "\""))
                  (princ (strcat "\n     New: \"" newName "\""))
                )
                (progn
                  (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))
                  (princ "\n[UB] Operation cancelled.")
                )
              )
            )
          )
        )
        (princ "\n[UB] Selected entity is not a block reference.")
      )
    )
    (princ "\n[UB] No block reference selected.")
  )

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
(defun c:RH  () (_ExecTransform 'FLIP_H))
(defun c:RBV () (_ExecTransform 'FLIP_V))
(defun c:RV  () (_ExecTransform 'FLIP_V))


;;; --------------------------------------------------------------------------
;;; 3. STANDARD JOINERY BLOCK PROCEDURAL ENGINE & DCL PALETTE
;;; --------------------------------------------------------------------------

;; CadSetup:EnsureStandardBlock - Generates standard block on layer "0" with BYBLOCK properties
(defun CadSetup:EnsureStandardBlock (blkName / acadApp doc blocks blkObj bData geom entStr p1 p2 pList closed ptArr polyObj lObj cObj)
  ;; Ensure Database/Db_Blocks.lsp is loaded in memory
  (if (or (not (boundp 'CadSetup:GetBlockData)) (null *CadSetup-Standard-Blocks-Data*))
    (cond
      ((and (boundp 'CadSetup:GetDir) CadSetup:GetDir (findfile (strcat (CadSetup:GetDir) "\\Database\\Db_Blocks.lsp")))
       (load (strcat (CadSetup:GetDir) "\\Database\\Db_Blocks.lsp")))
      ((findfile "Db_Blocks.lsp")
       (load (findfile "Db_Blocks.lsp")))
      ((findfile "Database\\Db_Blocks.lsp")
       (load (findfile "Database\\Db_Blocks.lsp")))
      ((findfile "d:\\Cad-Setup\\Cad-Setup-v4\\Database\\Db_Blocks.lsp")
       (load "d:\\Cad-Setup\\Cad-Setup-v4\\Database\\Db_Blocks.lsp"))
    )
  )

  (setq acadApp (vlax-get-acad-object)
        doc     (vla-get-ActiveDocument acadApp)
        blocks  (vla-get-Blocks doc))

  ;; Check if block exists in drawing
  (setq blkObj (vl-catch-all-apply 'vla-Item (list blocks blkName)))
  (if (or (vl-catch-all-error-p blkObj) (null blkObj))
    ;; Block doesn't exist: create it in drawing database
    (setq blkObj (vl-catch-all-apply 'vla-Add (list blocks (vlax-3d-point '(0.0 0.0 0.0)) blkName)))
  )

  (if (and (not (vl-catch-all-error-p blkObj)) blkObj)
    (progn
      ;; Standard block data lookup
      (setq bData (if (boundp 'CadSetup:GetBlockData) (CadSetup:GetBlockData blkName) nil))
      (if (and bData (setq geom (nth 6 bData)))
        (progn
          ;; Self-healing: If block has 0 entities or fewer than defined, clear and re-populate!
          (if (< (vla-get-Count blkObj) (length geom))
            (progn
              ;; Clear any stale or corrupt entities from earlier runs
              (vlax-for ent blkObj
                (vl-catch-all-apply 'vla-Delete (list ent))
              )

              (foreach elem geom
                (setq entStr (strcase (vl-princ-to-string (car elem))))
                (cond
                  ;; Line primitive
                  ((wcmatch entStr "*LINE*")
                   (setq p1 (cadr elem)
                         p2 (caddr elem))
                   (setq lObj (vl-catch-all-apply 'vla-AddLine
                                (list blkObj
                                      (vlax-3d-point (list (float (car p1)) (float (cadr p1)) 0.0))
                                      (vlax-3d-point (list (float (car p2)) (float (cadr p2)) 0.0)))))
                   (if (and (not (vl-catch-all-error-p lObj)) lObj)
                     (progn
                       (vla-put-Layer lObj "0")
                       (vla-put-Color lObj 0) ; BYBLOCK
                     )
                   )
                  )

                  ;; Circle primitive
                  ((wcmatch entStr "*CIRC*")
                   (setq p1 (cadr elem)
                         r  (caddr elem))
                   (setq cObj (vl-catch-all-apply 'vla-AddCircle
                                (list blkObj
                                      (vlax-3d-point (list (float (car p1)) (float (cadr p1)) 0.0))
                                      (float r))))
                   (if (and (not (vl-catch-all-error-p cObj)) cObj)
                     (progn
                       (vla-put-Layer cObj "0")
                       (vla-put-Color cObj 0) ; BYBLOCK
                     )
                   )
                  )

                  ;; Rectangle primitive
                  ((wcmatch entStr "*RECT*")
                   (setq p1 (cadr elem)
                         p2 (caddr elem))
                   (setq ptArr (vlax-make-safearray vlax-vbDouble '(0 . 7)))
                   (vlax-safearray-fill ptArr
                     (list (float (car p1)) (float (cadr p1))
                           (float (car p2)) (float (cadr p1))
                           (float (car p2)) (float (cadr p2))
                           (float (car p1)) (float (cadr p2))))
                   (setq polyObj (vl-catch-all-apply 'vla-AddLightWeightPolyline (list blkObj ptArr)))
                   (if (vl-catch-all-error-p polyObj)
                     (setq polyObj (vl-catch-all-apply 'vla-AddLightWeightPolyline
                                     (list blkObj (vlax-make-variant ptArr (logior vlax-vbarray vlax-vbDouble))))))
                   (if (and (not (vl-catch-all-error-p polyObj)) polyObj)
                     (progn
                       (vla-put-Closed polyObj :vlax-true)
                       (vla-put-Layer polyObj "0")
                       (vla-put-Color polyObj 0) ; BYBLOCK
                     )
                   )
                  )

                  ;; Closed or open polygon primitive
                  ((wcmatch entStr "*POLY*")
                   (setq pList  (cadr elem)
                         closed (caddr elem))
                   (setq ptArr (vlax-make-safearray vlax-vbDouble (cons 0 (1- (* (length pList) 2)))))
                   (vlax-safearray-fill ptArr (mapcar 'float (apply 'append pList)))
                   (setq polyObj (vl-catch-all-apply 'vla-AddLightWeightPolyline (list blkObj ptArr)))
                   (if (vl-catch-all-error-p polyObj)
                     (setq polyObj (vl-catch-all-apply 'vla-AddLightWeightPolyline
                                     (list blkObj (vlax-make-variant ptArr (logior vlax-vbarray vlax-vbDouble))))))
                   (if (and (not (vl-catch-all-error-p polyObj)) polyObj)
                     (progn
                       (if closed (vla-put-Closed polyObj :vlax-true))
                       (vla-put-Layer polyObj "0")
                       (vla-put-Color polyObj 0) ; BYBLOCK
                     )
                   )
                  )
                )
              )
              (if *CadSetup-Debug*
                (princ (strcat "\n[BL Debug] Populated " (itoa (vla-get-Count blkObj)) " entities in \"" blkName "\"."))
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

;; CadSetup:RenderBlockPreview - Draws dynamic 2D vector wireframe onto DCL image tile
(defun CadSetup:RenderBlockPreview (tileKey blkName / bData bbox geom imgW imgH minX minY maxX maxY
                                                      rangeX rangeY scl pad offX offY
                                                      _toPixelX _toPixelY _drawCircle entStr p1 p2 pList closed i px0 py0)
  (setq imgW (dimx_tile tileKey)
        imgH (dimy_tile tileKey))
  (start_image tileKey)
  (fill_image 0 0 imgW imgH 250) ; Dark background

  (setq bData (if (boundp 'CadSetup:GetBlockData) (CadSetup:GetBlockData blkName) nil))
  (if (and bData 
           (setq bbox (nth 5 bData)) 
           (setq geom (nth 6 bData))
           (listp bbox)
           (listp (car bbox)))
    (progn
      (setq minX (caar bbox)
            minY (cadar bbox)
            maxX (caadr bbox)
            maxY (cadadr bbox))

      ;; Include origin (0,0) in bounding box to ensure base point is visible
      (setq minX (min minX -2.0)
            minY (min minY -2.0)
            maxX (max maxX 2.0)
            maxY (max maxY 2.0))

      (setq rangeX (- maxX minX)
            rangeY (- maxY minY))
      (if (<= rangeX 0.0) (setq rangeX 1.0))
      (if (<= rangeY 0.0) (setq rangeY 1.0))

      (setq pad 16.0)
      (setq scl (min (/ (- (float imgW) (* 2.0 pad)) rangeX)
                     (/ (- (float imgH) (* 2.0 pad)) rangeY)))

      ;; Centering offsets
      (setq offX (+ pad (/ (- (- (float imgW) (* 2.0 pad)) (* rangeX scl)) 2.0))
            offY (+ pad (/ (- (- (float imgH) (* 2.0 pad)) (* rangeY scl)) 2.0)))

      ;; Coordinate transformation helpers (Y is inverted in DCL image tiles)
      (defun _toPixelX (x) (fix (+ offX (* (- x minX) scl))))
      (defun _toPixelY (y) (fix (- (- (float imgH) offY) (* (- y minY) scl))))

      ;; 1. Draw origin crosshairs (+) at (0, 0) in Yellow (color 2) and center tick in Red (color 1)
      (setq px0 (_toPixelX 0.0)
            py0 (_toPixelY 0.0))
      (vector_image (max 0 (- px0 8)) py0 (min (1- imgW) (+ px0 8)) py0 2)
      (vector_image px0 (max 0 (- py0 8)) px0 (min (1- imgH) (+ py0 8)) 2)
      (vector_image (max 0 (- px0 3)) py0 (min (1- imgW) (+ px0 3)) py0 1)
      (vector_image px0 (max 0 (- py0 3)) px0 (min (1- imgH) (+ py0 3)) 1)

      ;; Circle drawing helper (24-point smooth polygon)
      (defun _drawCircle (cx cy r col / segs step ang x0 y0 x1 y1 j pxA pyA pxB pyB)
        (setq segs 24
              step (/ (* 2.0 pi) (float segs))
              j 0)
        (while (< j segs)
          (setq ang (* (float j) step)
                x0 (+ cx (* r (cos ang)))
                y0 (+ cy (* r (sin ang)))
                x1 (+ cx (* r (cos (+ ang step))))
                y1 (+ cy (* r (sin (+ ang step)))))
          (setq pxA (_toPixelX x0)
                pyA (_toPixelY y0)
                pxB (_toPixelX x1)
                pyB (_toPixelY y1))
          (vector_image pxA pyA pxB pyB col)
          (setq j (1+ j))
        )
      )

      ;; 2. Render all geometric primitives in crisp White (color 7) & Cyan (color 4)
      (foreach elem geom
        (setq entStr (strcase (vl-princ-to-string (car elem))))
        (cond
          ((wcmatch entStr "*LINE*")
           (setq p1 (cadr elem)
                 p2 (caddr elem))
           (vector_image (_toPixelX (car p1)) (_toPixelY (cadr p1))
                         (_toPixelX (car p2)) (_toPixelY (cadr p2)) 7)
          )

          ((wcmatch entStr "*CIRC*")
           (setq p1 (cadr elem)
                 p2 (caddr elem))
           (_drawCircle (car p1) (cadr p1) p2 4)
          )

          ((wcmatch entStr "*RECT*")
           (setq p1 (cadr elem)
                 p2 (caddr elem))
           (vector_image (_toPixelX (car p1)) (_toPixelY (cadr p1)) (_toPixelX (car p2)) (_toPixelY (cadr p1)) 7)
           (vector_image (_toPixelX (car p2)) (_toPixelY (cadr p1)) (_toPixelX (car p2)) (_toPixelY (cadr p2)) 7)
           (vector_image (_toPixelX (car p2)) (_toPixelY (cadr p2)) (_toPixelX (car p1)) (_toPixelY (cadr p2)) 7)
           (vector_image (_toPixelX (car p1)) (_toPixelY (cadr p2)) (_toPixelX (car p1)) (_toPixelY (cadr p1)) 7)
          )

          ((wcmatch entStr "*POLY*")
           (setq pList  (cadr elem)
                 closed (caddr elem)
                 i 0)
           (while (< i (1- (length pList)))
             (setq p1 (nth i pList)
                   p2 (nth (1+ i) pList))
             (vector_image (_toPixelX (car p1)) (_toPixelY (cadr p1))
                           (_toPixelX (car p2)) (_toPixelY (cadr p2)) 7)
             (setq i (1+ i))
           )
           (if (and closed (> (length pList) 2))
             (vector_image (_toPixelX (caar (reverse pList))) (_toPixelY (cadar (reverse pList)))
                           (_toPixelX (caar pList)) (_toPixelY (cadar pList)) 7)
           )
          )
        )
      )
    )
    (progn
      ;; Generic placeholder preview for drawing blocks without procedural vectors
      (setq px0 (/ imgW 2) py0 (/ imgH 2))
      (vector_image (- px0 25) (- py0 25) (+ px0 25) (- py0 25) 8)
      (vector_image (+ px0 25) (- py0 25) (+ px0 25) (+ py0 25) 8)
      (vector_image (+ px0 25) (+ py0 25) (- px0 25) (+ py0 25) 8)
      (vector_image (- px0 25) (+ py0 25) (- px0 25) (- py0 25) 8)
      (vector_image (- px0 25) (- py0 25) (+ px0 25) (+ py0 25) 8)
      (vector_image (- px0 25) (+ py0 25) (+ px0 25) (- py0 25) 8)
      (vector_image px0 (- py0 35) px0 (+ py0 35) 1)
      (vector_image (- px0 35) py0 (+ px0 35) py0 1)
    )
  )
  (end_image)
)

;; CadSetup:GetActiveDrawingBlocks - Retrieves list of user blocks defined in drawing
(defun CadSetup:GetActiveDrawingBlocks ( / bEntry bList bName )
  (setq bList nil)
  (setq bEntry (tblnext "BLOCK" t))
  (while bEntry
    (setq bName (cdr (assoc 2 bEntry)))
    (if (and bName 
             (/= (substr bName 1 1) "*")
             (not (wcmatch (strcase bName) "*_SPACE*")))
      (setq bList (cons bName bList))
    )
    (setq bEntry (tblnext "BLOCK" nil))
  )
  (if bList
    (acad_strlsort bList)
    nil
  )
)

;; CadSetup:InsertBlockInteractive - Interactive placement loop with auto-layer & undo wrapping
(defun CadSetup:InsertBlockInteractive (blkName targetLayer scaleVal rotVal continuousMode smartLayerMode /
                                        *error* oldEcho oldLayer pt loop acadApp acadDoc ms insPt blkRef)
  (defun *error* (msg)
    (if oldEcho  (setvar "CMDECHO" oldEcho))
    (if oldLayer (setvar "CLAYER" oldLayer))
    (if (boundp 'CadSetup:UndoReset) (CadSetup:UndoReset))
    (if (and msg (not (wcmatch (strcase msg t) "*break*,*cancel*,*exit*,*quit*")))
      (princ (strcat "\n[BL] Error: " msg))
    )
    (princ)
  )

  (setq oldEcho  (getvar "CMDECHO")
        oldLayer (getvar "CLAYER"))
  (setvar "CMDECHO" 0)

  ;; 1. Ensure standard block exists and has geometry in the drawing database (with self-healing)
  (CadSetup:EnsureStandardBlock blkName)

  ;; 2. Ensure target layer exists in drawing if smartLayerMode is active
  (if (and smartLayerMode targetLayer (/= targetLayer "") (/= targetLayer "0"))
    (progn
      (cond
        ((wcmatch (strcase targetLayer) "*HARD*")
         (if (boundp 'CadSetup:EnsureHardwareLayersLoaded) (CadSetup:EnsureHardwareLayersLoaded)))
        ((wcmatch (strcase targetLayer) "*LINE*")
         (if (boundp 'CadSetup:EnsureLineLayersLoaded) (CadSetup:EnsureLineLayersLoaded)))
        ((wcmatch (strcase targetLayer) "*MAT*")
         (if (boundp 'CadSetup:EnsureMaterialLayersLoaded) (CadSetup:EnsureMaterialLayersLoaded)))
        ((wcmatch (strcase targetLayer) "*ANNO*")
         (if (boundp 'CadSetup:EnsureAnnotationLayersLoaded) (CadSetup:EnsureAnnotationLayersLoaded)))
      )
      (CadSetup:SetCurrentLayerSafe targetLayer)
      (princ (strcat "\n[BL] Target layer: " targetLayer))
    )
  )

  (princ (strcat "\n[BL] Inserting \"" blkName "\" (Scale: " (rtos scaleVal 2 2) ", Rot: " (rtos rotVal 2 1) " deg)..."))
  (if continuousMode
    (princ "\n[BL] Continuous mode active: Click points to place components, press Enter or Esc to finish.")
  )

  (setq acadApp (vlax-get-acad-object)
        acadDoc (vla-get-ActiveDocument acadApp)
        ms      (vla-get-ModelSpace acadDoc))

  (setq loop T)
  (while loop
    (setq pt (getpoint (if continuousMode "\nSpecify insertion point [or Enter to finish]: " "\nSpecify insertion point: ")))
    (if pt
      (progn
        (if (boundp 'CadSetup:UndoStart) (CadSetup:UndoStart))
        
        ;; Insert using ActiveX for rock-solid reliability across all UCS, units, and OSNAP
        (setq insPt (vlax-3d-point (trans pt 1 0))) ; Transform picked point from Current UCS to WCS
        (setq blkRef (vl-catch-all-apply 'vla-InsertBlock
                       (list ms insPt blkName (float scaleVal) (float scaleVal) (float scaleVal)
                             (* (float rotVal) (/ pi 180.0)))))
        
        (if (and (not (vl-catch-all-error-p blkRef)) blkRef)
          (progn
            (if (and smartLayerMode targetLayer (/= targetLayer "") (/= targetLayer "0"))
              (vla-put-Layer blkRef targetLayer)
            )
            (vla-Update blkRef)
            ;; Highlight placed entity for immediate visual confirmation
            (vl-catch-all-apply 'redraw (list (vlax-vla-object->ename blkRef) 3))
            (princ (strcat "\n[BL] Placed \"" blkName "\" on layer [" (vla-get-Layer blkRef) "]."))
          )
          ;; Fallback to native command if COM had any issue
          (progn
            (setvar "CMDECHO" 1)
            (command "._-insert" blkName "_non" pt scaleVal scaleVal rotVal)
            (setvar "CMDECHO" 0)
            (princ (strcat "\n[BL] Placed \"" blkName "\" via command."))
          )
        )

        (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))
        (if (not continuousMode)
          (setq loop nil)
        )
      )
      (setq loop nil)
    )
  )

  ;; Restore original layer & echo
  (setvar "CLAYER" oldLayer)
  (setvar "CMDECHO" oldEcho)
  (princ (strcat "\n[BL] Finished insertion. Restored active layer to " oldLayer "."))
  (princ)
)


;; CadSetup:OpenBlockPalette - Visual DCL Dialog Controller for Standard Joinery Palette
(defun CadSetup:OpenBlockPalette ( / *error* dclPath dclId act allStdBld categories
                                     curCat filteredItems selIdx selBlk bData
                                     scaleVal rotVal contVal smartLayVal
                                     updateSelection updateCategoryList )
  (defun *error* (msg)
    (if (and dclId (>= dclId 0))
      (vl-catch-all-apply 'unload_dialog (list dclId))
    )
    (if (and msg (not (wcmatch (strcase msg t) "*break*,*cancel*,*exit*,*quit*")))
      (princ (strcat "\n[BL Dialog Error]: " msg))
    )
    (princ)
  )

  ;; Locate DCL
  (setq dclPath nil)
  (cond
    ((and (boundp 'CadSetup:GetDir) CadSetup:GetDir 
          (setq dclPath (strcat (CadSetup:GetDir) "\\UI\\BLOCK-PALETTE.dcl")) 
          (findfile dclPath))
     dclPath)
    ((setq dclPath (findfile "BLOCK-PALETTE.dcl"))
     dclPath)
    ((setq dclPath (findfile "UI\\BLOCK-PALETTE.dcl"))
     dclPath)
    ((setq dclPath (findfile "d:\\Cad-Setup\\Cad-Setup-v4\\UI\\BLOCK-PALETTE.dcl"))
     dclPath)
  )

  (if (or (null dclPath) (not (findfile dclPath)))
    (progn
      (princ "\n[BL Error] Unable to find UI/BLOCK-PALETTE.dcl.")
      (princ)
    )
    (progn
      (setq dclId (load_dialog dclPath))
      (if (not (new_dialog "block_palette_dialog" dclId))
        (progn
          (unload_dialog dclId)
          (princ "\n[BL Error] Failed to initialize block_palette_dialog.")
          (princ)
        )
        (progn
          ;; Ensure Database/Db_Blocks.lsp is loaded in memory
          (if (or (not (boundp 'CadSetup:GetAllStandardBlocks)) (null *CadSetup-Standard-Blocks-Data*))
            (cond
              ((and (boundp 'CadSetup:GetDir) CadSetup:GetDir (findfile (strcat (CadSetup:GetDir) "\\Database\\Db_Blocks.lsp")))
               (load (strcat (CadSetup:GetDir) "\\Database\\Db_Blocks.lsp")))
              ((findfile "Db_Blocks.lsp")
               (load (findfile "Db_Blocks.lsp")))
              ((findfile "Database\\Db_Blocks.lsp")
               (load (findfile "Database\\Db_Blocks.lsp")))
              ((findfile "d:\\Cad-Setup\\Cad-Setup-v4\\Database\\Db_Blocks.lsp")
               (load "d:\\Cad-Setup\\Cad-Setup-v4\\Database\\Db_Blocks.lsp"))
            )
          )

          ;; 1. Categories
          (setq allStdBld (if (boundp 'CadSetup:GetAllStandardBlocks) (CadSetup:GetAllStandardBlocks) nil))
          (setq categories (if (boundp 'CadSetup:GetBlockCategories) (CadSetup:GetBlockCategories) '("All Standards")))
          (setq categories (append categories '("Active Drawing Blocks")))

          (start_list "pop_category")
          (mapcar 'add_list categories)
          (end_list)
          (setq curCat (car categories))
          (set_tile "pop_category" "0")

          ;; Defaults for insertion params
          (setq scaleVal 1.0
                rotVal 0.0
                contVal 0
                smartLayVal 1)
          (set_tile "eb_scale" "1.0")
          (set_tile "eb_rotation" "0.0")
          (set_tile "tog_continuous" "0")
          (set_tile "tog_smart_layer" "1")

          ;; Subroutine to update component list when category changes
          (defun updateCategoryList (cat / items drawBlocks)
            (cond
              ((= cat "All Standards")
               (setq filteredItems (mapcar 'car allStdBld))
              )
              ((= cat "Active Drawing Blocks")
               (setq drawBlocks (CadSetup:GetActiveDrawingBlocks))
               (setq filteredItems (if drawBlocks drawBlocks '("<No User Blocks in Drawing>")))
              )
              (t
               (setq filteredItems nil)
               (foreach item allStdBld
                 (if (= (cadr item) cat)
                   (setq filteredItems (cons (car item) filteredItems))
                 )
               )
               (setq filteredItems (reverse filteredItems))
              )
            )
            (start_list "lst_blocks")
            (mapcar 'add_list filteredItems)
            (end_list)
            (setq selIdx 0)
            (set_tile "lst_blocks" "0")
            (updateSelection (car filteredItems))
          )

          ;; Subroutine to update card & preview
          (defun updateSelection (blkName / bRow bDesc bLayer bSpecs bCat)
            (setq selBlk blkName)
            (setq bRow (if (boundp 'CadSetup:GetBlockData) (CadSetup:GetBlockData blkName) nil))
            (if bRow
              (progn
                (setq bCat   (nth 1 bRow)
                      bDesc  (nth 2 bRow)
                      bLayer (nth 3 bRow)
                      bSpecs (nth 4 bRow))
                (set_tile "txt_blk_name" (strcat "Component: " blkName))
                (set_tile "txt_blk_cat" (strcat "Category: " bCat))
                (set_tile "txt_blk_layer" (strcat "Target Layer: " bLayer))
                (set_tile "txt_blk_specs" (strcat "Dimensions: " bSpecs))
                (set_tile "txt_blk_desc" (strcat "Description: " bDesc))
              )
              (progn
                (set_tile "txt_blk_name" (strcat "Component: " (if blkName blkName "-")))
                (set_tile "txt_blk_cat" "Category: Drawing Block")
                (set_tile "txt_blk_layer" "Target Layer: Current Working Layer")
                (set_tile "txt_blk_specs" "Dimensions: User Custom Geometry")
                (set_tile "txt_blk_desc" "Description: Custom block defined in active drawing.")
              )
            )
            (CadSetup:RenderBlockPreview "img_preview" blkName)
          )

          ;; Initialize list
          (updateCategoryList curCat)

          ;; Event Handlers
          (action_tile "pop_category"
            "(setq curCat (nth (atoi $value) categories))
             (updateCategoryList curCat)"
          )

          (action_tile "lst_blocks"
            "(setq selIdx (atoi $value)
                   selBlk (nth selIdx filteredItems))
             (updateSelection selBlk)
             (if (= $reason 4) (done_dialog 1))"
          )

          (action_tile "eb_scale"
            "(setq scaleVal (distof $value))
             (if (or (null scaleVal) (<= scaleVal 0.0))
               (progn (setq scaleVal 1.0) (set_tile \"eb_scale\" \"1.0\"))
             )"
          )

          (action_tile "eb_rotation"
            "(setq rotVal (distof $value))
             (if (null rotVal)
               (progn (setq rotVal 0.0) (set_tile \"eb_rotation\" \"0.0\"))
             )"
          )

          (action_tile "btn_rot_0"   "(setq rotVal 0.0)   (set_tile \"eb_rotation\" \"0.0\")")
          (action_tile "btn_rot_90"  "(setq rotVal 90.0)  (set_tile \"eb_rotation\" \"90.0\")")
          (action_tile "btn_rot_180" "(setq rotVal 180.0) (set_tile \"eb_rotation\" \"180.0\")")
          (action_tile "btn_rot_270" "(setq rotVal 270.0) (set_tile \"eb_rotation\" \"270.0\")")

          (action_tile "tog_smart_layer" "(setq smartLayVal (atoi $value))")
          (action_tile "tog_continuous"  "(setq contVal (atoi $value))")

          (action_tile "btn_insert" "(done_dialog 1)")
          (action_tile "cancel" "(done_dialog 0)")

          (setq act (start_dialog))
          (unload_dialog dclId)

          ;; On OK/Insert
          (if (and (= act 1) selBlk (/= selBlk "<No User Blocks in Drawing>"))
            (progn
              (setq bData (if (boundp 'CadSetup:GetBlockData) (CadSetup:GetBlockData selBlk) nil))
              (CadSetup:InsertBlockInteractive
                selBlk
                (if bData (nth 3 bData) nil)
                scaleVal
                rotVal
                (= contVal 1)
                (= smartLayVal 1)
              )
            )
          )
        )
      )
    )
  )
  (princ)
)

;; Command Aliases for Block Palette
(defun c:BL () (CadSetup:OpenBlockPalette))
(defun c:8  () (c:BL))


(if *CadSetup-Debug*
  (princ "\n[03_Blocks.lsp] Block creation, transform, and Visual Joinery Palette loaded (GB/G, DB, UB, RB, RBH/RH, RBV/RV, BL, 8).")
)
(princ)
