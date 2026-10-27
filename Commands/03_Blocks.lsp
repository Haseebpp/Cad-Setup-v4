;;; ==========================================================================
;;; 03_Blocks.lsp - Auto Block Generators & In-Place Fast Block Transforms
;;; Layer: Commands (Priority 03)
;;; Author   : Haseeb
;;; ==========================================================================

(vl-load-com)

;;; --------------------------------------------------------------------------
;;; 1. AUTO BLOCK CREATION & UNIQUE BLOCK ENGINE (GB, DB, DSL, UB)
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
(defun CadSetup:EnsureUniqueBlockName (blkName / baseName count pUnd numPart)
  (if (tblsearch "BLOCK" blkName)
    (progn
      (setq baseName blkName
            count 1)
      ;; If name already ends with _<number>, extract baseName and start counter from that number + 1
      (setq pUnd (vl-string-position (ascii "_") blkName 0 T))
      (if (and pUnd (> pUnd 0) (< pUnd (1- (strlen blkName))))
        (progn
          (setq numPart (substr blkName (+ pUnd 2)))
          (if (vl-every '(lambda (c) (and (>= c 48) (<= c 57))) (vl-string->list numPart))
            (setq baseName (substr blkName 1 pUnd)
                  count    (1+ (atoi numPart)))
          )
        )
      )
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

;; CadSetup:GetSsBoundingBox - Collective bounding box of selection set in current UCS
(defun CadSetup:GetSsBoundingBox (ss / bbox i ent obj minPt maxPt p1 p2 ll ur allMin allMax)
  (if (and (boundp 'CadSetup:GetBoundingBoxUcs) (setq bbox (CadSetup:GetBoundingBoxUcs ss)))
    bbox
    (progn
      (setq allMin nil allMax nil)
      (repeat (setq i (sslength ss))
        (setq ent (ssname ss (setq i (1- i)))
              obj (vlax-ename->vla-object ent))
        (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list obj 'minPt 'maxPt))))
          (progn
            (setq p1 (vlax-safearray->list minPt)
                  p2 (vlax-safearray->list maxPt))
            ;; Transform from WCS to Current UCS
            (setq p1 (trans p1 0 1)
                  p2 (trans p2 0 1))
            (setq ll (list (min (car p1) (car p2)) (min (cadr p1) (cadr p2)) (min (caddr p1) (caddr p2)))
                  ur (list (max (car p1) (car p2)) (max (cadr p1) (cadr p2)) (max (caddr p1) (caddr p2))))
            (setq allMin (if allMin
                           (list (min (car allMin) (car ll))
                                 (min (cadr allMin) (cadr ll))
                                 (min (caddr allMin) (caddr ll)))
                           ll)
                  allMax (if allMax
                           (list (max (car allMax) (car ur))
                                 (max (cadr allMax) (cadr ur))
                                 (max (caddr allMax) (caddr ur)))
                           ur))
          )
        )
      )
      (if (and allMin allMax)
        (list allMin allMax)
        nil
      )
    )
  )
)

;; DSL : Dynamic Stretch Linear - Auto-create dynamic block with Width & Height linear stretch
(defun c:DSL (/ *error* ss bbox minPt maxPt basePt w h tol dimOffset
                handle ts blkName oldCmd oldAtt oldOsm oldBlkLock oldCmdDia oldActionBar
                ptLabelX ptFrameX1 ptFrameX2 ptActX entBefore pX
                ptLabelY ptFrameY1 ptFrameY2 ptActY pY newEnt)
  (defun *error* (msg)
    ;; If trapped inside Block Editor on failure, close and discard
    (if (and (getvar "BLOCKEDITOR") (= (getvar "BLOCKEDITOR") 1))
      (vl-catch-all-apply 'vl-cmdf (list "._BCLOSE" "_Discard"))
    )
    (if oldActionBar (setvar 'bactionbarmode oldActionBar))
    (if oldAtt (setvar 'attreq oldAtt))
    (if oldCmd (setvar 'cmdecho oldCmd))
    (if oldOsm (setvar 'osmode oldOsm))
    (if oldBlkLock (setvar 'blockeditlock oldBlkLock))
    (if oldCmdDia (setvar 'cmddia oldCmdDia))
    (if (boundp 'CadSetup:UndoReset) (CadSetup:UndoReset))
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[DSL] Error: " msg))
    )
    (princ)
  )

  (setq oldCmd       (getvar 'cmdecho)
        oldAtt       (getvar 'attreq)
        oldOsm       (getvar 'osmode)
        oldBlkLock   (getvar 'blockeditlock)
        oldCmdDia    (getvar 'cmddia)
        oldActionBar (if (getvar 'bactionbarmode) (getvar 'bactionbarmode) 0))

  (setvar 'cmdecho 0)
  (setvar 'attreq 0)

  (setq ss (ssget "_I"))
  (if (not ss)
    (progn
      (princ "\nSelect objects to convert into Dynamic Stretch Block [DSL]: ")
      (setq ss (ssget))
    )
  )

  (if ss
    (progn
      ;; Calculate collective bounding box in current UCS
      (setq bbox (CadSetup:GetSsBoundingBox ss))
      (if (not bbox)
        (progn
          (princ "\n[DSL] Error: Could not determine bounding box for selection.")
          (setvar 'cmdecho oldCmd)
          (setvar 'attreq oldAtt)
          (exit)
        )
      )

      (setq minPt  (car bbox)
            maxPt  (cadr bbox)
            basePt (list (car minPt) (cadr minPt) 0.0)
            w      (- (car maxPt) (car minPt))
            h      (- (cadr maxPt) (cadr minPt)))

      (if (or (<= w 0.001) (<= h 0.001))
        (progn
          (princ "\n[DSL] Error: Objects must have 2D extent (Width and Height > 0.001).")
          (setvar 'cmdecho oldCmd)
          (setvar 'attreq oldAtt)
          (exit)
        )
      )

      (if (boundp 'CadSetup:UndoStart) (CadSetup:UndoStart))

      ;; Generate unique standard DYN block name
      (setq handle  (CadSetup:GetNextDbHandle)
            ts      (CadSetup:FormatBlockTimestamp)
            blkName (CadSetup:BuildBlockName "DYN" handle 1 ts ts)
            blkName (CadSetup:EnsureUniqueBlockName blkName))

      ;; Define standard block at Bottom-Left base point (entities converted to block definition)
      (setvar 'osmode 0)
      (command "._-block" blkName "_non" basePt ss "")

      ;; Prepare dynamic block parameters & action geometry (inside Block Editor, basePt is at 0,0,0)
      (setq tol       0.001
            dimOffset (max 1.0 (* 0.08 (max w h))))

      ;; Geometry for Horizontal (Width) Stretch along bottom edge:
      ;; Grip at (W, 0, 0), stretching right half: [W/2 - tol, W + tol] across full height [-tol, H + tol]
      (setq ptLabelX   (list (/ w 2.0) (- 0.0 dimOffset) 0.0)
            ptFrameX1  (list (- (/ w 2.0) tol) (- 0.0 tol) 0.0)
            ptFrameX2  (list (+ w tol) (+ h tol) 0.0)
            ptActX     (list (* 0.75 w) (* -1.5 dimOffset) 0.0))

      ;; Geometry for Vertical (Height) Stretch along left edge:
      ;; Grip at (0, H, 0), stretching top half: [H/2 - tol, H + tol] across full width [-tol, W + tol]
      (setq ptLabelY   (list (- 0.0 dimOffset) (/ h 2.0) 0.0)
            ptFrameY1  (list (- 0.0 tol) (- (/ h 2.0) tol) 0.0)
            ptFrameY2  (list (+ w tol) (+ h tol) 0.0)
            ptActY     (list (* -1.5 dimOffset) (* 0.75 h) 0.0))

      ;; Lock BACTIONBARMODE=0 (classic icons with action-location prompt), unlock BEDIT, suppress dialogs
      (if (getvar 'bactionbarmode) (setvar 'bactionbarmode 0))
      (setvar 'blockeditlock 0)
      (setvar 'cmddia 0)

      ;; Open Block Editor for dynamic authoring
      (command "._BEDIT" blkName)

      (if (= (getvar "BLOCKEDITOR") 1)
        (progn
          ;; Frame the entire block plus generous margin so crossing selection (_C) never clips off-screen
          (command "._ZOOM" "_Window"
                   (list (* -3.0 dimOffset) (* -3.0 dimOffset) 0.0)
                   (list (+ w (* 3.0 dimOffset)) (+ h (* 3.0 dimOffset)) 0.0))

          ;; 1. Horizontal Linear Parameter ("Width") on bottom edge with 1 grip on right (W, 0, 0)
          (setq entBefore (entlast))
          (if (not (vl-cmdf "._BPARAMETER" "_Linear" "_Name" "Width" '(0.0 0.0 0.0) (list w 0.0 0.0) ptLabelX 1))
            (command "._BPARAMETER" "_Linear" '(0.0 0.0 0.0) (list w 0.0 0.0) ptLabelX 1)
          )
          (setq pX (entlast))

          ;; Horizontal Stretch Action (Right half stretch) via deterministic single command pipeline
          (if (and pX (not (eq pX entBefore)))
            (command "._BACTIONTOOL" "_Stretch"
                     (list pX (list (/ w 2.0) 0.0 0.0))  ; parameter selection
                     (list w 0.0 0.0)                    ; parameter point (grip)
                     ptFrameX1                           ; stretch frame corner 1
                     ptFrameX2                           ; stretch frame corner 2
                     "_C" ptFrameX1 ptFrameX2 ""         ; objects crossing window
                     ptActX)                             ; action icon location
          )

          ;; 2. Vertical Linear Parameter ("Height") on left edge with 1 grip on top (0, H, 0)
          (setq entBefore (entlast))
          (if (not (vl-cmdf "._BPARAMETER" "_Linear" "_Name" "Height" '(0.0 0.0 0.0) (list 0.0 h 0.0) ptLabelY 1))
            (command "._BPARAMETER" "_Linear" '(0.0 0.0 0.0) (list 0.0 h 0.0) ptLabelY 1)
          )
          (setq pY (entlast))

          ;; Vertical Stretch Action (Top half stretch) via deterministic single command pipeline
          (if (and pY (not (eq pY entBefore)))
            (command "._BACTIONTOOL" "_Stretch"
                     (list pY (list 0.0 (/ h 2.0) 0.0))  ; parameter selection
                     (list 0.0 h 0.0)                    ; parameter point (grip)
                     ptFrameY1                           ; stretch frame corner 1
                     ptFrameY2                           ; stretch frame corner 2
                     "_C" ptFrameY1 ptFrameY2 ""         ; objects crossing window
                     ptActY)                             ; action icon location
          )

          ;; Save and close Block Editor
          (command "._BSAVE")
          (command "._BCLOSE")
          (if (= (getvar "BLOCKEDITOR") 1)
            (command "._BCLOSE" "_Yes")
          )
        )
      )

      ;; Restore blockeditlock, cmddia, osmode, and bactionbarmode
      (if (getvar 'bactionbarmode) (setvar 'bactionbarmode oldActionBar))
      (setvar 'blockeditlock oldBlkLock)
      (setvar 'cmddia oldCmdDia)
      (setvar 'osmode oldOsm)

      ;; Re-insert the newly created dynamic block in-place at base point
      (command "._-insert" blkName "_non" basePt 1.0 1.0 0.0)

      ;; Select/highlight the newly inserted block reference
      (setq newEnt (entlast))
      (if newEnt
        (sssetfirst nil (ssadd newEnt (ssadd)))
      )

      (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))
      (princ (strcat "\n[DSL] Dynamic Stretch Block created: \"" blkName "\" [W: " (rtos w 2 2) " x H: " (rtos h 2 2) "]."))
    )
    (princ "\n[DSL] No objects selected.")
  )

  (if (getvar 'bactionbarmode) (setvar 'bactionbarmode oldActionBar))
  (setvar 'attreq oldAtt)
  (setvar 'cmdecho oldCmd)
  (setvar 'osmode oldOsm)
  (setvar 'blockeditlock oldBlkLock)
  (setvar 'cmddia oldCmdDia)
  (princ)
)

;; CadSetup:CloneBlockDefinition - Duplicates an existing block definition to a new unique name.
;; Employs fast in-memory ActiveX deep-cloning for static blocks and WBLOCK/INSERT for dynamic blocks.
(defun CadSetup:CloneBlockDefinition (sourceName destName isDynamic / acadApp doc blocks srcDef
                                             newDef origin objList sa res tempDwg oldDia oldEcho oldExp subObj)
  (setq acadApp (vlax-get-acad-object)
        doc     (vla-get-ActiveDocument acadApp)
        blocks  (vla-get-Blocks doc))

  ;; 1. If static block, attempt fast in-memory ActiveX DeepClone
  (if (not isDynamic)
    (progn
      (setq srcDef (vl-catch-all-apply 'vla-Item (list blocks sourceName)))
      (if (and (not (vl-catch-all-error-p srcDef)) srcDef)
        (progn
          (setq origin (vla-get-Origin srcDef)
                newDef (vl-catch-all-apply 'vla-Add (list blocks origin destName)))
          (if (and (not (vl-catch-all-error-p newDef)) newDef)
            (progn
              (setq objList nil)
              (vlax-for subObj srcDef
                (setq objList (cons subObj objList))
              )
              (if objList
                (progn
                  (setq sa (vlax-make-safearray vlax-vbObject (cons 0 (1- (length objList)))))
                  (vlax-safearray-fill sa (reverse objList))
                  (setq res (vl-catch-all-apply 'vla-CopyObjects (list doc sa newDef)))
                  (if (or (vl-catch-all-error-p res) (= (vla-get-Count newDef) 0))
                    (progn
                      ;; DeepClone failed or empty: delete incomplete definition to allow WBLOCK fallback
                      (vl-catch-all-apply 'vla-Delete (list newDef))
                      (setq newDef nil)
                    )
                  )
                )
                ;; If source definition had 0 entities, empty newDef is valid
                T
              )
            )
          )
        )
      )
    )
  )

  ;; 2. If dynamic block or if ActiveX DeepClone failed: clone via full DWG database WBLOCK/INSERT
  (if (or isDynamic (null newDef))
    (progn
      (setq tempDwg (vl-filename-mktemp "cs_blk_" (getvar "tempprefix") ".dwg")
            oldDia  (getvar "FILEDIA")
            oldEcho (getvar "CMDECHO")
            oldExp  (getvar "EXPERT"))
      (if (findfile tempDwg) (vl-file-delete tempDwg))
      (setvar "FILEDIA" 0)
      (setvar "CMDECHO" 0)
      (setvar "EXPERT" 2)

      ;; Write out complete source block definition (preserves all dynamic actions/parameters/metadata)
      (command "._-wblock" tempDwg sourceName)

      ;; Read into drawing as new block definition destName
      (if (findfile tempDwg)
        (progn
          (command "._-insert" (strcat destName "=" tempDwg))
          (while (> (getvar "CMDACTIVE") 0)
            (command)
          )
          (if (findfile tempDwg)
            (vl-file-delete tempDwg)
          )
        )
      )

      (setvar "EXPERT" oldExp)
      (setvar "FILEDIA" oldDia)
      (setvar "CMDECHO" oldEcho)
    )
  )

  (if (tblsearch "BLOCK" destName) T nil)
)

;; CadSetup:ReassignBlockReference - Updates the selected block reference to point to newName
;; Preserves position, scales, rotation, layer, color, linetype, lineweight, transparency,
;; dynamic block parameters, and attribute text values.
(defun CadSetup:ReassignBlockReference (ent newName isDynamic / acadApp doc obj inPlaceDone
                                              d insPt xScale yScale zScale rot lay col lt lw tr
                                              owner hasAtt dynPropValues attValues newRef match)
  (setq acadApp (vlax-get-acad-object)
        doc     (vla-get-ActiveDocument acadApp)
        obj     (vlax-ename->vla-object ent))

  (setq inPlaceDone nil)

  ;; 1. For static blocks, attempt in-place block definition redirection
  (if (not isDynamic)
    (progn
      (vl-catch-all-apply 'vla-put-Name (list obj newName))
      (if (= (strcase (vla-get-Name obj)) (strcase newName))
        (progn
          (vla-update obj)
          (setq inPlaceDone T)
        )
      )
      (if (not inPlaceDone)
        (progn
          (setq d (entget ent))
          (setq d (subst (cons 2 newName) (assoc 2 d) d))
          (if (entmod d)
            (progn
              (entupd ent)
              (setq inPlaceDone T)
            )
          )
        )
      )
    )
  )

  ;; 2. If dynamic or in-place was not supported: perform seamless replacement
  (if inPlaceDone
    ent
    (progn
      (setq insPt   (vla-get-InsertionPoint obj)
            xScale  (vla-get-XScaleFactor obj)
            yScale  (vla-get-YScaleFactor obj)
            zScale  (vla-get-ZScaleFactor obj)
            rot     (vla-get-Rotation obj)
            lay     (vla-get-Layer obj)
            col     (if (vlax-property-available-p obj 'Color) (vla-get-Color obj) nil)
            lt      (if (vlax-property-available-p obj 'Linetype) (vla-get-Linetype obj) nil)
            lw      (if (vlax-property-available-p obj 'Lineweight) (vla-get-Lineweight obj) nil)
            tr      (if (vlax-property-available-p obj 'Transparency) (vla-get-Transparency obj) nil)
            owner   (vla-ObjectIdToObject doc (vla-get-OwnerID obj))
            hasAtt  (= (vla-get-HasAttributes obj) :vlax-true))

      ;; Capture dynamic property values if dynamic block
      (setq dynPropValues nil)
      (if isDynamic
        (vl-catch-all-apply
          '(lambda ()
             (foreach p (vlax-invoke obj 'GetDynamicBlockProperties)
               (if (= (vla-get-ReadOnly p) :vlax-false)
                 (setq dynPropValues (cons (cons (vla-get-PropertyName p) (vlax-get p 'Value)) dynPropValues))
               )
             )
           )
        )
      )

      ;; Capture attribute values if block has attributes
      (setq attValues nil)
      (if hasAtt
        (vl-catch-all-apply
          '(lambda ()
             (foreach att (vlax-invoke obj 'GetAttributes)
               (setq attValues (cons (cons (strcase (vla-get-TagString att)) (vla-get-TextString att)) attValues))
             )
           )
        )
      )

      ;; Insert new instance of newName in same space
      (setq newRef (vl-catch-all-apply 'vla-InsertBlock
                     (list owner insPt newName xScale yScale zScale rot)))

      (if (and (not (vl-catch-all-error-p newRef)) newRef)
        (progn
          (vla-put-Layer newRef lay)
          (if col (vl-catch-all-apply 'vla-put-Color (list newRef col)))
          (if lt  (vl-catch-all-apply 'vla-put-Linetype (list newRef lt)))
          (if lw  (vl-catch-all-apply 'vla-put-Lineweight (list newRef lw)))
          (if (and tr (vlax-property-available-p newRef 'Transparency))
            (vl-catch-all-apply 'vla-put-Transparency (list newRef tr))
          )

          ;; Reapply dynamic properties
          (if (and isDynamic dynPropValues)
            (vl-catch-all-apply
              '(lambda ()
                 (foreach dp (vlax-invoke newRef 'GetDynamicBlockProperties)
                   (if (and (= (vla-get-ReadOnly dp) :vlax-false)
                            (setq match (assoc (vla-get-PropertyName dp) dynPropValues)))
                     (vl-catch-all-apply 'vlax-put (list dp 'Value (cdr match)))
                   )
                 )
               )
            )
          )

          ;; Reapply attribute values
          (if (and attValues (= (vla-get-HasAttributes newRef) :vlax-true))
            (vl-catch-all-apply
              '(lambda ()
                 (foreach nAtt (vlax-invoke newRef 'GetAttributes)
                   (setq match (assoc (strcase (vla-get-TagString nAtt)) attValues))
                   (if match
                     (vla-put-TextString nAtt (cdr match))
                   )
                 )
               )
            )
          )

          ;; Delete the old block reference
          (vla-Delete obj)

          ;; Return the newly created block reference ename
          (vlax-vla-object->ename newRef)
        )
        nil
      )
    )
  )
)

;; CadSetup:MakeBlockUnique - Makes a single block reference unique by creating a duplicate definition
;; and redirecting ONLY the selected block reference to the new definition.
;; Returns (list updatedEname originalName uniqueName) on success, nil on cancel/failure.
(defun CadSetup:MakeBlockUnique (ent / d obj blkName isDynamic parsed curVer newVer curInit newMod
                                       newName typeChoice handle cloneOk updatedEnt)
  (if (and ent (setq d (entget ent)) (= (cdr (assoc 0 d)) "INSERT"))
    (progn
      (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
      (if (and (not (vl-catch-all-error-p obj)) obj)
        (progn
          ;; Retrieve effective block definition name (resolves dynamic block anonymous *U names)
          (setq blkName
            (if (vlax-property-available-p obj 'EffectiveName)
              (vla-get-EffectiveName obj)
              (vla-get-Name obj)
            )
          )
          (if (or (null blkName) (= blkName ""))
            (setq blkName (cdr (assoc 2 d)))
          )

          ;; Guard: Do not allow making Xrefs unique
          (if (or (and (tblsearch "BLOCK" blkName)
                       (= (logand (cdr (assoc 70 (tblsearch "BLOCK" blkName))) 4) 4))
                  (and (vlax-property-available-p obj 'IsXref)
                       (= (vla-get-IsXref obj) :vlax-true)))
            (progn
              (princ "\n[UB] Error: Selected block reference is an External Reference (Xref) and cannot be made unique.")
              nil
            )
            (progn
              (setq isDynamic (and (vlax-property-available-p obj 'IsDynamicBlock)
                                   (= (vla-get-IsDynamicBlock obj) :vlax-true)))

              (setq parsed (CadSetup:ParseBlockName blkName))

              (if parsed
                ;; 1. Standard CadSetup block: Bump version and refresh MOD timestamp
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

                  (while (tblsearch "BLOCK" newName)
                    (setq newVer  (1+ newVer)
                          newName (CadSetup:BuildBlockName
                                    (cdr (assoc 'TYPE parsed))
                                    (cdr (assoc 'HANDLE parsed))
                                    newVer
                                    newMod
                                    curInit)))
                )
                ;; 2. Non-Standard block: Keep name with unique suffix, or convert to GRP/DYN
                (progn
                  (initget "Keep GRP DYN Cancel")
                  (setq typeChoice (getkword (strcat "\nBlock \"" blkName "\" is non-standard. Make unique as [Keep name/GRP/DYN/Cancel] <Keep>: ")))
                  (if (or (null typeChoice) (= typeChoice "")) (setq typeChoice "Keep"))

                  (cond
                    ((= typeChoice "Keep")
                     (setq newName (CadSetup:EnsureUniqueBlockName (strcat blkName "_1"))))
                    ((or (= typeChoice "GRP") (= typeChoice "DYN"))
                     (setq handle  (CadSetup:GetNextDbHandle)
                           newMod  (CadSetup:FormatBlockTimestamp)
                           newName (CadSetup:BuildBlockName typeChoice handle 1 newMod newMod)
                           newName (CadSetup:EnsureUniqueBlockName newName)))
                    (T
                     (setq newName nil))
                  )
                )
              )

              (if newName
                (progn
                  ;; Clone block definition
                  (setq cloneOk (CadSetup:CloneBlockDefinition blkName newName isDynamic))
                  (if cloneOk
                    (progn
                      ;; Reassign selected block reference only
                      (setq updatedEnt (CadSetup:ReassignBlockReference ent newName isDynamic))
                      (if updatedEnt
                        (list updatedEnt blkName newName)
                        (progn
                          (princ "\n[UB] Error: Failed to reassign selected block reference to unique definition.")
                          nil
                        )
                      )
                    )
                    (progn
                      (princ (strcat "\n[UB] Error: Failed to clone block definition \"" blkName "\" to \"" newName "\"."))
                      nil
                    )
                  )
                )
                nil ; user cancelled
              )
            )
          )
        )
        (progn
          (princ "\n[UB] Error: Failed to access block reference COM object.")
          nil
        )
      )
    )
    (progn
      (princ "\n[UB] Selected entity is not a block reference.")
      nil
    )
  )
)

;; UB : Unique Block - Creates a unique copy of the block definition and updates the selected block only
(defun c:UB (/ *error* ss ent count newEnts res oldCmd i)
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
    nil ; ss already contains pre-selected block reference(s)
    (progn
      (setvar 'cmdecho oldCmd)
      (setq ent (car (entsel "\nSelect block reference to make unique [UB]: ")))
      (setvar 'cmdecho 0)
      (if ent
        (if (= (cdr (assoc 0 (entget ent))) "INSERT")
          (setq ss (ssadd ent (ssadd)))
          (princ "\n[UB] Selected entity is not a block reference.")
        )
      )
    )
  )

  (if (and ss (> (sslength ss) 0))
    (progn
      (if (boundp 'CadSetup:UndoStart) (CadSetup:UndoStart))
      (setq count 0
            newEnts (ssadd))
      (repeat (setq i (sslength ss))
        (setq ent (ssname ss (setq i (1- i))))
        (setq res (CadSetup:MakeBlockUnique ent))
        (if res
          (progn
            (setq count (1+ count))
            (if (car res)
              (ssadd (car res) newEnts)
            )
            (princ (strcat "\n[UB] Block made unique (selected block reference updated):"))
            (princ (strcat "\n     Original: \"" (cadr res) "\""))
            (princ (strcat "\n     Unique  : \"" (caddr res) "\""))
          )
        )
      )
      (if (> count 0)
        (sssetfirst nil newEnts)
      )
      (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))
      (if (> count 1)
        (princ (strcat "\n[UB] Successfully made " (itoa count) " block references unique."))
      )
    )
    (if (not ent)
      (princ "\n[UB] No block reference selected.")
    )
  )

  (setvar 'cmdecho oldCmd)
  (princ)
)

;; Command Aliases for Unique Block
(defun c:UNIQUEBLOCK  () (c:UB))
(defun c:UNIQUE-BLOCK () (c:UB))
(defun c:UBLK         () (c:UB))


;;; --------------------------------------------------------------------------
;;; 1b. GROUP ISOLATE EDIT SESSION (GIS, GUN)
;;; --------------------------------------------------------------------------
;; Workflow:
;;   GIS : Validate a single non-dynamic block -> Explode (X) in place -> Isolate (IS)
;;         the exploded objects. Edit freely (draw / copy / paste / delete).
;;   GUN : Re-group surviving exploded objects + ANY objects created during the session
;;         into a new block at World Origin (same as GB/G) -> Unisolate (UN).
;; Naming on GUN:
;;   - Standard CadSetup block -> same TYPE + HANDLE, version bumped, new MOD, same INIT (like UB).
;;   - Non-standard block      -> brand-new GRP[$HANDLE.v01] name.
;; Session data lives in memory per drawing (*CadSetup-GIS-Session*); it is lost on close.

;; Do not reset an active session when the suite is hot-reloaded (RCS)
(if (not (boundp '*CadSetup-GIS-Session*)) (setq *CadSetup-GIS-Session* nil))

;; CadSetup:PadHandle - Upper-cases and left-pads a hex handle to 16 chars for string comparison
(defun CadSetup:PadHandle (h)
  (setq h (strcase h))
  (while (< (strlen h) 16) (setq h (strcat "0" h)))
  h
)

;; CadSetup:HandleGreaterP - T if hex handle h1 was created after hex handle h2
(defun CadSetup:HandleGreaterP (h1 h2)
  (> (CadSetup:PadHandle h1) (CadSetup:PadHandle h2))
)

;; CadSetup:GetCurrentSpaceName - Returns the DXF 410 value of the active space ("Model" or layout name)
(defun CadSetup:GetCurrentSpaceName ()
  (if (or (= (getvar 'tilemode) 1) (> (getvar 'cvport) 1))
    "Model"
    (getvar 'ctab)
  )
)

;; CadSetup:GisValidateInsert - Returns nil if the entity can be group-isolated,
;; otherwise a string describing why it was rejected.
(defun CadSetup:GisValidateInsert (ent / d obj blkName blkRec layRec cols rows sx sy sz e hasEnt hasAttDef)
  (setq d (entget ent))
  (if (/= (cdr (assoc 0 d)) "INSERT")
    "Selected object is not a block reference."
    (progn
      (setq obj     (vlax-ename->vla-object ent)
            blkName (cdr (assoc 2 d))
            blkRec  (tblsearch "BLOCK" blkName)
            layRec  (tblsearch "LAYER" (cdr (assoc 8 d)))
            cols    (cdr (assoc 70 d))
            rows    (cdr (assoc 71 d))
            sx      (cond ((cdr (assoc 41 d))) (1.0))
            sy      (cond ((cdr (assoc 42 d))) (1.0))
            sz      (cond ((cdr (assoc 43 d))) (1.0)))
      ;; Scan definition for content and constant/hidden attribute definitions
      (if blkRec
        (progn
          (setq e (cdr (assoc -2 blkRec)))
          (while e
            (setq hasEnt T)
            (if (= (cdr (assoc 0 (entget e))) "ATTDEF") (setq hasAttDef T))
            (setq e (entnext e))
          )
        )
      )
      (cond
        ((or (and blkRec (= (logand (cdr (assoc 70 blkRec)) 4) 4))
             (and (vlax-property-available-p obj 'IsXref)
                  (= (vla-get-IsXref obj) :vlax-true)))
         "Selected block is an External Reference (Xref).")
        ((and (vlax-property-available-p obj 'IsDynamicBlock)
              (= (vla-get-IsDynamicBlock obj) :vlax-true))
         "Selected block is a Dynamic Block. GIS works on static blocks only.")
        ((or (and cols (> cols 1)) (and rows (> rows 1)))
         "Selected block is a MINSERT (array block) and cannot be exploded.")
        ((or (= (cdr (assoc 66 d)) 1) hasAttDef)
         "Selected block contains attributes (exploding would turn values back into tags).")
        ((not (and (equal sx sy 1e-8) (equal sx sz 1e-8) (> sx 0.0)))
         "Selected block is non-uniformly scaled or mirrored (explode could distort geometry).")
        ((and layRec (= (logand (cdr (assoc 70 layRec)) 4) 4))
         (strcat "Selected block is on locked layer \"" (cdr (assoc 8 d)) "\"."))
        ((not hasEnt)
         "Selected block definition is empty.")
        (T nil)
      )
    )
  )
)

;; CadSetup:ExecuteGis - Core execution of Group Isolate on a single validated block entity
(defun CadSetup:ExecuteGis (ent / *error* oldCmd reason d obj res expEnts marker ssIso h)
  (defun *error* (msg)
    (if oldCmd (setvar 'cmdecho oldCmd))
    (if (boundp 'CadSetup:UndoReset) (CadSetup:UndoReset))
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[GIS] Error: " msg))
    )
    (princ)
  )

  (setq oldCmd (getvar 'cmdecho))
  (setvar 'cmdecho 0)

  (cond
    (*CadSetup-GIS-Session*
     (princ (strcat "\n[WARN] [GIS] A group-isolate session is already active for \""
                    (cdr (assoc 'NAME *CadSetup-GIS-Session*))
                    "\". Run GUN (or UN) to finish it first."))
    )
    ((not ent)
     (princ "\n[GIS] No block selected.")
    )
    ((setq reason (CadSetup:GisValidateInsert ent))
     (princ (strcat "\n[GIS] " reason))
    )
    (T
     (if (boundp 'CadSetup:UndoStart) (CadSetup:UndoStart))

     (setq d   (entget ent)
           obj (vlax-ename->vla-object ent)
           res (vl-catch-all-apply 'vla-Explode (list obj)))

     (if (or (vl-catch-all-error-p res)
             (vl-catch-all-error-p
               (setq expEnts (vl-catch-all-apply
                               '(lambda (v) (mapcar 'vlax-vla-object->ename
                                                    (vlax-safearray->list (vlax-variant-value v))))
                               (list res))))
             (null expEnts))
       (progn
         (princ (strcat "\n[GIS] Error: Failed to explode block"
                        (if (vl-catch-all-error-p res) (strcat ": " (vl-catch-all-error-message res)) ".")))
         (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))
       )
       (progn
         ;; Highest handle among exploded objects = marker for objects created later
         (setq marker "0" ssIso (ssadd))
         (foreach e expEnts
           (ssadd e ssIso)
           (setq h (cdr (assoc 5 (entget e))))
           (if (CadSetup:HandleGreaterP h marker) (setq marker h))
         )

         ;; Remove the original block reference (vla-Explode keeps the source)
         (vla-Delete obj)

         (setq *CadSetup-GIS-Session*
                (list (cons 'NAME   (cdr (assoc 2 d)))
                      (cons 'LAYER  (cdr (assoc 8 d)))
                      (cons 'SPACE  (cond ((cdr (assoc 410 d))) ((CadSetup:GetCurrentSpaceName))))
                      (cons 'ENTS   expEnts)
                      (cons 'MARKER marker)))

         (command "_.ISOLATEOBJECTS" ssIso "")

         (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))
         (princ (strcat "\n[IS/GIS] Exploded and isolated \"" (cdr (assoc 2 d)) "\" ("
                        (itoa (length expEnts)) " object(s)). Edit, then run UN (or GUN) to regroup."))
       )
     )
    )
  )

  (setvar 'cmdecho oldCmd)
  (princ)
)

;; GIS : Group Isolate - Explode a single static block in place and isolate its contents for editing
(defun c:GIS (/ oldCmd ss ent)
  (setq oldCmd (getvar 'cmdecho))
  (setvar 'cmdecho 0)

  (if *CadSetup-GIS-Session*
    (princ (strcat "\n[WARN] [GIS] A group-isolate session is already active for \""
                   (cdr (assoc 'NAME *CadSetup-GIS-Session*))
                   "\". Run GUN (or UN) to finish it first."))
    (progn
      ;; 1. Resolve a single selected object (pre-selection or pick)
      (setq ss (ssget "_I"))
      (cond
        ((and ss (> (sslength ss) 1))
         (princ (strcat "\n[GIS] Select a single block only (" (itoa (sslength ss)) " objects selected)."))
        )
        (ss (setq ent (ssname ss 0)))
        (T
         (setvar 'cmdecho oldCmd)
         (setq ent (car (entsel "\nSelect a single block to group-isolate [GIS]: ")))
         (setvar 'cmdecho 0)
         (if (not ent) (princ "\n[GIS] No block selected."))
        )
      )
      (sssetfirst nil nil)

      ;; 2. Run GIS core
      (if ent (CadSetup:ExecuteGis ent))
    )
  )

  (setvar 'cmdecho oldCmd)
  (princ)
)

;; GUN : Group Unisolate - Regroup the GIS session objects into a block at World Origin and unisolate
(defun c:GUN (/ *error* oldCmd oldAtt sess space marker ss all i e d parsed newVer newMod newName newEnt layName)
  (defun *error* (msg)
    (if oldAtt (setvar 'attreq oldAtt))
    (if oldCmd (setvar 'cmdecho oldCmd))
    (if (boundp 'CadSetup:UndoReset) (CadSetup:UndoReset))
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[GUN] Error: " msg))
    )
    (princ)
  )

  (setq oldCmd (getvar 'cmdecho)
        oldAtt (getvar 'attreq)
        sess   *CadSetup-GIS-Session*)
  (setvar 'cmdecho 0)
  (setvar 'attreq 0)

  (cond
    ;; No session: behave like plain UN
    ((null sess)
     (princ "\n[Notice] [GUN] No active group-isolate session. Running UNISOLATEOBJECTS.")
     (command "_.UNISOLATEOBJECTS")
    )

    ;; Session belongs to a different space
    ((/= (strcase (setq space (cdr (assoc 'SPACE sess)))) (strcase (CadSetup:GetCurrentSpaceName)))
     (princ (strcat "\n[WARN] [GUN] The active session was started in \"" space
                    "\". Switch to that space and run GUN again."))
    )

    (T
     ;; 1. Collect surviving exploded objects
     (setq marker (cdr (assoc 'MARKER sess))
           ss     (ssadd))
     (foreach e (cdr (assoc 'ENTS sess))
       (if (entget e) (ssadd e ss))
     )

     ;; 2. Collect every object created after the explode in the same space
     (if (setq all (ssget "_X" (list (cons 410 space))))
       (repeat (setq i (sslength all))
         (setq e (ssname all (setq i (1- i)))
               d (entget e))
         (if (and (/= (cdr (assoc 0 d)) "VIEWPORT")
                  (CadSetup:HandleGreaterP (cdr (assoc 5 d)) marker))
           (ssadd e ss)
         )
       )
     )

     (if (= (sslength ss) 0)
       (progn
         (princ "\n[WARN] [GUN] Nothing left to regroup (all session objects were deleted). Session closed.")
         (command "_.UNISOLATEOBJECTS")
         (setq *CadSetup-GIS-Session* nil)
       )
       (progn
         (if (boundp 'CadSetup:UndoStart) (CadSetup:UndoStart))

         ;; 3. Resolve new block name (version bump for standard names, new GRP otherwise)
         (setq newMod (CadSetup:FormatBlockTimestamp)
               parsed (CadSetup:ParseBlockName (cdr (assoc 'NAME sess))))
         (if parsed
           (progn
             (setq newVer  (1+ (cdr (assoc 'VERSION parsed)))
                   newName (CadSetup:BuildBlockName (cdr (assoc 'TYPE parsed)) (cdr (assoc 'HANDLE parsed))
                                                    newVer newMod (cdr (assoc 'INIT parsed))))
             (while (tblsearch "BLOCK" newName)
               (setq newVer  (1+ newVer)
                     newName (CadSetup:BuildBlockName (cdr (assoc 'TYPE parsed)) (cdr (assoc 'HANDLE parsed))
                                                      newVer newMod (cdr (assoc 'INIT parsed)))))
           )
           (setq newName (CadSetup:EnsureUniqueBlockName
                           (CadSetup:BuildBlockName "GRP" (CadSetup:GetNextDbHandle) 1 newMod newMod)))
         )

         ;; 4. Define block at World Origin and re-insert in place (identical to GB)
         (command "._-block" newName "_non" '(0.0 0.0 0.0) ss "")

         (if (not (tblsearch "BLOCK" newName))
           (progn
             (princ (strcat "\n[GUN] Error: Failed to create block \"" newName "\". Session kept; fix and retry GUN."))
             (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))
           )
           (progn
             (command "._-insert" newName "_non" '(0.0 0.0 0.0) 1.0 1.0 0.0)
             (setq newEnt (entlast))

             ;; Restore the original block reference layer
             (setq layName (cdr (assoc 'LAYER sess)))
             (if (and newEnt layName (tblsearch "LAYER" layName))
               (vl-catch-all-apply 'vla-put-Layer (list (vlax-ename->vla-object newEnt) layName))
             )

             (command "_.UNISOLATEOBJECTS")
             (setq *CadSetup-GIS-Session* nil)

             (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))
             (princ (strcat "\n[GUN] Regrouped " (itoa (sslength ss)) " object(s):"))
             (princ (strcat "\n     Original: \"" (cdr (assoc 'NAME sess)) "\""))
             (princ (strcat "\n     New     : \"" newName "\""))
             (if newEnt (sssetfirst nil (ssadd newEnt)))
           )
         )
       )
     )
    )
  )

  (setvar 'attreq oldAtt)
  (setvar 'cmdecho oldCmd)
  (princ)
)

;; Command Aliases for Group Isolate / Unisolate
(defun c:GROUPISOLATE    () (c:GIS))
(defun c:GROUP-ISOLATE   () (c:GIS))
(defun c:GROUPUNISOLATE  () (c:GUN))
(defun c:GROUP-UNISOLATE () (c:GUN))


;;; --------------------------------------------------------------------------
;;; 2. FAST IN-PLACE BLOCK TRANSFORMS (RB, RBH, RBV)
;;; --------------------------------------------------------------------------

;; Helper: Extract Bounding Box Center using Geometry Helpers
(defun _FastMidPt (obj / bbox insPt)
  (setq bbox (CadSetup:GetBoundingBox obj))
  (if bbox
    (CadSetup:MidPoint (car bbox) (cadr bbox))
    (progn
      (setq insPt (vl-catch-all-apply 'vla-get-InsertionPoint (list obj)))
      (if (and (not (vl-catch-all-error-p insPt)) insPt)
        (vlax-safearray->list (vlax-variant-value insPt))
        nil
      )
    )
  )
)

;; Master Transform Engine
(defun _ExecTransform (mode / *error* echo ss i ent obj mid rot vecH vecV pt2 new-obj res)
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

  ;; Check pre-selection first, fallback to prompt with locked-layer filter
  (setq ss (ssget "_I" '((0 . "INSERT"))))
  (if (not ss)
    (setq ss (ssget "_:L" '((0 . "INSERT"))))
  )

  (if ss
    (repeat (setq i (sslength ss))
      (setq ent (ssname ss (setq i (1- i)))
            obj (vlax-ename->vla-object ent))
      
      (if (setq mid (_FastMidPt obj))
        (cond
          ;; 90-Degree Clockwise Rotation around center
          ((eq mode 'ROTATE)
           (setq res (vl-catch-all-apply 'vla-rotate (list obj (vlax-3d-point mid) (- (/ pi 2.0)))))
           (if (not (vl-catch-all-error-p res))
             (vla-update obj)
           )
          )

          ;; Flip Horizontal: Mirror across block's local vertical axis (flips along original horizontal)
          ((eq mode 'FLIP_H)
           (setq rot (vl-catch-all-apply 'vla-get-Rotation (list obj)))
           (if (or (vl-catch-all-error-p rot) (null rot))
             (setq rot (cdr (assoc 50 (entget ent))))
           )
           (if (null rot) (setq rot 0.0))

           ;; Vector along block's local vertical axis in WCS
           (setq vecV (vl-catch-all-apply 'trans (list (list (- (sin rot)) (cos rot) 0.0) ent 0 1)))
           (if (or (vl-catch-all-error-p vecV) (null vecV))
             (setq vecV (list (- (sin rot)) (cos rot) 0.0))
           )

           (setq pt2 (mapcar '+ mid (mapcar '(lambda (x) (* x 100.0)) vecV)))
           (setq new-obj (vl-catch-all-apply 'vla-mirror (list obj (vlax-3d-point mid) (vlax-3d-point pt2))))
           (if (not (vl-catch-all-error-p new-obj))
             (progn
               (vla-delete obj)
               (vla-update new-obj)
             )
           )
          )

          ;; Flip Vertical: Mirror across block's local horizontal axis (flips along original vertical)
          ((eq mode 'FLIP_V)
           (setq rot (vl-catch-all-apply 'vla-get-Rotation (list obj)))
           (if (or (vl-catch-all-error-p rot) (null rot))
             (setq rot (cdr (assoc 50 (entget ent))))
           )
           (if (null rot) (setq rot 0.0))

           ;; Vector along block's local horizontal axis in WCS
           (setq vecH (vl-catch-all-apply 'trans (list (list (cos rot) (sin rot) 0.0) ent 0 1)))
           (if (or (vl-catch-all-error-p vecH) (null vecH))
             (setq vecH (list (cos rot) (sin rot) 0.0))
           )

           (setq pt2 (mapcar '+ mid (mapcar '(lambda (x) (* x 100.0)) vecH)))
           (setq new-obj (vl-catch-all-apply 'vla-mirror (list obj (vlax-3d-point mid) (vlax-3d-point pt2))))
           (if (not (vl-catch-all-error-p new-obj))
             (progn
               (vla-delete obj)
               (vla-update new-obj)
             )
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
  (princ "\n[03_Blocks.lsp] Block creation (GB/G, DB, DSL, UB [Unique Block]), transforms (RB, RBH/RH, RBV/RV), and Visual Joinery Palette loaded.")
)
(princ)
