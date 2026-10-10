;;; ==========================================================================
;;; 06_Utilities.lsp - Drawing Cleanup, Measurement, System Fixes & Utilities
;;; Layer: Commands (Priority 06)
;;; Author   : Haseeb
;;; Commands : TC, BBOX, CTRANS, FL0, HSCALE (HSC), HROT (HR), PUA (QA), FIXSELECT, FIXBOX, WF, WR, LOAD-STYLES (LST)
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
;;; 2. BOUNDING BOX & GEOMETRY BOUNDS
;;; --------------------------------------------------------------------------

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

;; HSC / HSCALE : Multiply Hatch Scale by an Input Factor
(defun c:HSCALE ( / *error* oldCmd ss factor i ent obj patName patType curVal newVal
                    scaledCount solidCount lockedCount totalCount hasHatch err )
  (setq oldCmd (getvar "CMDECHO"))

  ;; 1. Localized Error Handler & Safe Stack Reset
  (defun *error* (msg)
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if (boundp 'CadSetup:UndoReset) (CadSetup:UndoReset))
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[HSCALE] Error: " msg))
    )
    (princ)
  )

  (setvar "CMDECHO" 0)

  ;; 2. Get Selection (Support Noun/Verb pre-selection or interactive prompt)
  (setq ss (ssget "_I"))
  (if ss
    (sssetfirst nil nil) ; Clear active grip selection for clean command interaction
    (progn
      (princ "\nSelect hatches to scale (or window containing hatches): ")
      (setq ss (ssget))
    )
  )

  (if (not ss)
    (princ "\n[HSCALE] No objects selected.")
    (progn
      ;; 3. Scan selection to ensure at least one hatch is present
      (setq totalCount (sslength ss)
            i 0
            hasHatch nil)
      (while (and (< i totalCount) (not hasHatch))
        (setq ent (ssname ss i))
        (if (= (cdr (assoc 0 (entget ent))) "HATCH")
          (setq hasHatch T)
        )
        (setq i (1+ i))
      )

      (if (not hasHatch)
        (princ "\n[HSCALE] No hatch entities found in selection.")
        (progn
          ;; 4. Prompt for Scale Multiplier (Disallow <= 0, remember session default)
          (if (not (and (boundp '*CadSetup-HatchScaleFactor*)
                        (numberp *CadSetup-HatchScaleFactor*)
                        (> *CadSetup-HatchScaleFactor* 0)))
            (setq *CadSetup-HatchScaleFactor* 2.0)
          )
          (initget 6) ; Bit 2 (no 0) + Bit 4 (no negative numbers)
          (setq factor (getreal (strcat "\nEnter hatch scale multiplier <"
                                        (rtos *CadSetup-HatchScaleFactor* 2 2)
                                        ">: ")))
          (if (null factor)
            (setq factor *CadSetup-HatchScaleFactor*)
            (setq *CadSetup-HatchScaleFactor* factor)
          )

          ;; 5. Begin Atomic Undo Transaction
          (if (boundp 'CadSetup:UndoStart) (CadSetup:UndoStart))

          (setq scaledCount 0
                solidCount  0
                lockedCount 0
                i           0)

          ;; 6. Process Entities
          (while (< i totalCount)
            (setq ent (ssname ss i)
                  i   (1+ i))
            (if (= (cdr (assoc 0 (entget ent))) "HATCH")
              (progn
                (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
                (if (and obj (not (vl-catch-all-error-p obj)))
                  (progn
                    (setq patName (strcase (vl-catch-all-apply 'vla-get-PatternName (list obj))))
                    (if (vl-catch-all-error-p patName) (setq patName ""))

                    ;; Detect solid fills or gradient objects (which do not support pattern scale)
                    (if (or (= patName "SOLID")
                            (and (vlax-property-available-p obj 'HatchObjectType)
                                 (= (vla-get-HatchObjectType obj) 1)))
                      (setq solidCount (1+ solidCount))
                      (progn
                        ;; Determine pattern type: 1 = UserDefined (PatternSpace), else PatternScale
                        (setq patType (vl-catch-all-apply 'vla-get-PatternType (list obj)))
                        (if (and (not (vl-catch-all-error-p patType)) (= patType 1))
                          ;; User-defined pattern -> multiply line spacing (PatternSpace)
                          (progn
                            (setq curVal (vl-catch-all-apply 'vla-get-PatternSpace (list obj)))
                            (if (and (numberp curVal) (> curVal 0))
                              (progn
                                (setq newVal (* curVal factor))
                                (setq err (vl-catch-all-apply 'vla-put-PatternSpace (list obj newVal)))
                                (if (vl-catch-all-error-p err)
                                  (setq lockedCount (1+ lockedCount))
                                  (progn
                                    (vl-catch-all-apply 'vla-Evaluate (list obj))
                                    (setq scaledCount (1+ scaledCount))
                                  )
                                )
                              )
                            )
                          )
                          ;; Predefined or Custom pattern -> multiply PatternScale
                          (progn
                            (setq curVal (vl-catch-all-apply 'vla-get-PatternScale (list obj)))
                            (if (and (numberp curVal) (> curVal 0))
                              (progn
                                (setq newVal (* curVal factor))
                                (setq err (vl-catch-all-apply 'vla-put-PatternScale (list obj newVal)))
                                (if (vl-catch-all-error-p err)
                                  (setq lockedCount (1+ lockedCount))
                                  (progn
                                    (vl-catch-all-apply 'vla-Evaluate (list obj))
                                    (setq scaledCount (1+ scaledCount))
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )

          ;; 7. Close Atomic Undo Transaction
          (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))

          ;; 8. User Feedback Summary
          (princ (strcat "\n[HSCALE] Scaled " (itoa scaledCount) " hatch(es) by multiplier " (rtos factor 2 2) "."))
          (if (> solidCount 0)
            (princ (strcat " (" (itoa solidCount) " solid/gradient fill(s) skipped)"))
          )
          (if (> lockedCount 0)
            (princ (strcat " (" (itoa lockedCount) " hatch(es) on locked layers skipped)"))
          )
        )
      )
    )
  )

  (setvar "CMDECHO" oldCmd)
  (princ)
)

;; Command alias
(defun c:HSC () (c:HSCALE))

;; HR / HROT / HROTATE : Rotate Selected Hatches (Relative Delta or Absolute Angle)
(defun c:HROT ( / *error* oldCmd ss inp isAbsolute deltaAngle absAngle i ent obj patName
                  curAngleRad curAngleDeg newAngleDeg newAngleRad scaledCount solidCount
                  lockedCount totalCount hasHatch err )
  (setq oldCmd (getvar "CMDECHO"))

  ;; 1. Localized Error Handler & Safe Stack Reset
  (defun *error* (msg)
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if (boundp 'CadSetup:UndoReset) (CadSetup:UndoReset))
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[HROT] Error: " msg))
    )
    (princ)
  )

  (setvar "CMDECHO" 0)

  ;; 2. Get Selection (Support Noun/Verb pre-selection or interactive prompt)
  (setq ss (ssget "_I"))
  (if ss
    (sssetfirst nil nil) ; Clear active grip selection for clean command interaction
    (progn
      (princ "\nSelect hatches to rotate (or window containing hatches): ")
      (setq ss (ssget))
    )
  )

  (if (not ss)
    (princ "\n[HROT] No objects selected.")
    (progn
      ;; 3. Scan selection to ensure at least one hatch is present
      (setq totalCount (sslength ss)
            i 0
            hasHatch nil)
      (while (and (< i totalCount) (not hasHatch))
        (setq ent (ssname ss i))
        (if (= (cdr (assoc 0 (entget ent))) "HATCH")
          (setq hasHatch T)
        )
        (setq i (1+ i))
      )

      (if (not hasHatch)
        (princ "\n[HROT] No hatch entities found in selection.")
        (progn
          ;; 4. Initialize session defaults
          (if (not (and (boundp '*CadSetup-HatchRotDelta*)
                        (numberp *CadSetup-HatchRotDelta*)))
            (setq *CadSetup-HatchRotDelta* 45.0)
          )
          (if (not (and (boundp '*CadSetup-HatchRotAbs*)
                        (numberp *CadSetup-HatchRotAbs*)))
            (setq *CadSetup-HatchRotAbs* 0.0)
          )

          ;; Prompt for rotation delta or [Absolute] keyword
          (initget "Absolute")
          (setq inp (getreal (strcat "\nEnter rotation angle to add in degrees or [Absolute] <"
                                     (rtos *CadSetup-HatchRotDelta* 2 1)
                                     ">: ")))

          (cond
            ((= inp "Absolute")
             (setq absAngle (getreal (strcat "\nEnter absolute hatch rotation angle in degrees <"
                                             (rtos *CadSetup-HatchRotAbs* 2 1)
                                             ">: ")))
             (if (null absAngle) (setq absAngle *CadSetup-HatchRotAbs*))
             (setq *CadSetup-HatchRotAbs* absAngle
                   isAbsolute T))
            ((numberp inp)
             (setq deltaAngle inp
                   *CadSetup-HatchRotDelta* inp
                   isAbsolute nil))
            (t
             (setq deltaAngle *CadSetup-HatchRotDelta*
                   isAbsolute nil))
          )

          ;; 5. Begin Atomic Undo Transaction
          (if (boundp 'CadSetup:UndoStart) (CadSetup:UndoStart))

          (setq scaledCount 0
                solidCount  0
                lockedCount 0
                i           0)

          ;; 6. Process Entities
          (while (< i totalCount)
            (setq ent (ssname ss i)
                  i   (1+ i))
            (if (= (cdr (assoc 0 (entget ent))) "HATCH")
              (progn
                (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
                (if (and obj (not (vl-catch-all-error-p obj)))
                  (progn
                    (setq patName (strcase (vl-catch-all-apply 'vla-get-PatternName (list obj))))
                    (if (vl-catch-all-error-p patName) (setq patName ""))

                    ;; Detect solid fills or gradient objects (which do not support pattern rotation)
                    (if (or (= patName "SOLID")
                            (and (vlax-property-available-p obj 'HatchObjectType)
                                 (= (vla-get-HatchObjectType obj) 1)))
                      (setq solidCount (1+ solidCount))
                      (progn
                        (setq curAngleRad (vl-catch-all-apply 'vla-get-PatternAngle (list obj)))
                        (if (and (not (vl-catch-all-error-p curAngleRad)) (numberp curAngleRad))
                          (progn
                            (if isAbsolute
                              (setq newAngleDeg absAngle)
                              (progn
                                (setq curAngleDeg (* curAngleRad (/ 180.0 pi)))
                                (setq newAngleDeg (+ curAngleDeg deltaAngle))
                              )
                            )
                            ;; Convert to radians
                            (setq newAngleRad (* newAngleDeg (/ pi 180.0)))
                            (setq err (vl-catch-all-apply 'vla-put-PatternAngle (list obj newAngleRad)))
                            (if (vl-catch-all-error-p err)
                              (setq lockedCount (1+ lockedCount))
                              (progn
                                (vl-catch-all-apply 'vla-Evaluate (list obj))
                                (setq scaledCount (1+ scaledCount))
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )

          ;; 7. Close Atomic Undo Transaction
          (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))

          ;; 8. User Feedback Summary
          (if isAbsolute
            (princ (strcat "\n[HROT] Set rotation to " (rtos absAngle 2 1) "° for " (itoa scaledCount) " hatch(es)."))
            (princ (strcat "\n[HROT] Rotated " (itoa scaledCount) " hatch(es) by "
                           (if (>= deltaAngle 0) "+" "") (rtos deltaAngle 2 1) "°."))
          )
          (if (> solidCount 0)
            (princ (strcat " (" (itoa solidCount) " solid/gradient fill(s) skipped)"))
          )
          (if (> lockedCount 0)
            (princ (strcat " (" (itoa lockedCount) " hatch(es) on locked layers skipped)"))
          )
        )
      )
    )
  )

  (setvar "CMDECHO" oldCmd)
  (princ)
)

;; Command aliases
(defun c:HROTATE () (c:HROT))
(defun c:HR      () (c:HROT))


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
(defun c:FIXSELECT ( / *error* )
  (defun *error* (msg)
    (if (boundp 'CadSetup:UndoReset) (CadSetup:UndoReset))
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[FIXSELECT] Error: " msg))
    )
    (princ)
  )

  (if (boundp 'CadSetup:UndoStart) (CadSetup:UndoStart))
  (CadSetup:SafeSetVar "PICKFIRST" 1)
  (CadSetup:SafeSetVar "PICKADD" 2)
  (CadSetup:SafeSetVar "PICKAUTO" 5)
  (CadSetup:SafeSetVar "HIGHLIGHT" 1)
  (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))
  (princ "\n[FIXSELECT] Selection environment restored (PICKFIRST=1, PICKADD=2, PICKAUTO=5, HIGHLIGHT=1).")
  (princ)
)

;; FIXBOX : Restore File, Command & Attribute Dialog Boxes
(defun c:FIXBOX ( / *error* )
  (defun *error* (msg)
    (if (boundp 'CadSetup:UndoReset) (CadSetup:UndoReset))
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[FIXBOX] Error: " msg))
    )
    (princ)
  )

  (if (boundp 'CadSetup:UndoStart) (CadSetup:UndoStart))
  (CadSetup:SafeSetVar "FILEDIA" 1)
  (CadSetup:SafeSetVar "CMDDIA" 1)
  (CadSetup:SafeSetVar "ATTDIA" 1)
  (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))
  (princ "\n[FIXBOX] Dialog boxes restored (FILEDIA=1, CMDDIA=1, ATTDIA=1).")
  (princ)
)

;; WF : Wipeout Frame Toggle (Cycle: Visible & Printable -> Draft -> Hidden)
(defun c:WF (/ *error* oldecho curVal newVal)
  (setq oldecho (getvar "CMDECHO"))

  (defun *error* (msg)
    (if oldecho (setvar "CMDECHO" oldecho))
    (if (boundp 'CadSetup:UndoReset) (CadSetup:UndoReset))
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[WF] Error: " msg))
    )
    (princ)
  )

  (setvar "CMDECHO" 0)
  (if (boundp 'CadSetup:UndoStart) (CadSetup:UndoStart))
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
  (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))
  (setvar "CMDECHO" oldecho)
  (princ)
)

;;; --------------------------------------------------------------------------
;;; 8. WIPEOUT TOOLS
;;; --------------------------------------------------------------------------

;; WR / WIPEOUTRECTANGLE : Interactively draw a rectangle and convert to wipeout
(defun c:WIPEOUTRECTANGLE ( / *error* lastEnt newEnt )
  (defun *error* (msg)
    (CadSetup:UndoReset)
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[WR] Error: " msg))
    )
    (princ)
  )
  (setq lastEnt (entlast))
  (CadSetup:UndoStart)
  (command "_.rectang")
  (while (> (getvar 'cmdactive) 0)
    (command pause)
  )
  (setq newEnt (entlast))
  (if (and newEnt (not (eq newEnt lastEnt)))
    (command "_.wipeout" "_p" newEnt "_y")
  )
  (CadSetup:UndoEnd)
  (princ)
)

(defun c:WR ()
  (c:WIPEOUTRECTANGLE)
)

;;; --------------------------------------------------------------------------
;;; 9. PRODUCTION TEXT & DIMENSION STYLES LOADER
;;; --------------------------------------------------------------------------

;; CadSetup:LoadStandardStyles - Generates annotative text styles & dimstyles
(defun CadSetup:LoadStandardStyles ( / oldEcho )
  (setq oldEcho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  ;; -------------------------------------------------------------------------
  ;; A. TYPOGRAPHY (Annotative & Scalable)
  ;; -------------------------------------------------------------------------
  (if (not (tblsearch "style" "ARCH-TEXT"))
    (command "-style" "ARCH-TEXT" "arial.ttf" "0.0" "1.0" "0" "_N" "_N")
  )
  (if (not (tblsearch "style" "ARCH-TITLE"))
    (command "-style" "ARCH-TITLE" "arialbd.ttf" "0.0" "1.0" "0" "_N" "_N")
  )

  ;; -------------------------------------------------------------------------
  ;; B. ANNOTATIVE DIMENSION STYLES
  ;; -------------------------------------------------------------------------
  (setvar "DIMTXSTY" "ARCH-TEXT")
  (setvar "DIMTXT"   2.5)         ; Plotted height = 2.5 mm
  (setvar "DIMTAD"   1)           ; Text above line
  (setvar "DIMJUST"  0)           ; Centered
  (setvar "DIMGAP"   0.8)         ; Gap between line and text
  (setvar "DIMEXE"   1.2)         ; Extension past dim line
  (setvar "DIMEXO"   1.0)         ; Extension line origin offset
  (setvar "DIMLUNIT" 2)           ; Decimal
  (setvar "DIMDEC"   0)           ; 0 decimal places
  (setvar "DIMZIN"   8)           ; Suppress trailing zeros
  (setvar "DIMCLRD"  256)         ; ByLayer
  (setvar "DIMCLRE"  256)         ; ByLayer
  (setvar "DIMCLRT"  256)         ; ByLayer
  (setvar "DIMLWD"   18)          ; 0.18 mm
  (setvar "DIMLWE"   18)          ; 0.18 mm
  (setvar "DIMTOFL"  1)           ; Force line between points

  ;; B1. Save ARCH-TICK (Annotative architectural tick)
  (setvar "DIMBLK" "_ArchTick")
  (setvar "DIMASZ" 1.5)
  (if (tblsearch "dimstyle" "ARCH-TICK")
    (command "-dimstyle" "_save" "ARCH-TICK" "_yes")
    (command "-dimstyle" "_save" "ARCH-TICK")
  )

  ;; B2. Save ARCH-ARROW (Annotative closed arrow)
  (setvar "DIMBLK" ".")
  (setvar "DIMASZ" 2.2)
  (if (tblsearch "dimstyle" "ARCH-ARROW")
    (command "-dimstyle" "_save" "ARCH-ARROW" "_yes")
    (command "-dimstyle" "_save" "ARCH-ARROW")
  )

  (command "-dimstyle" "_restore" "ARCH-TICK")
  (setvar "CMDECHO" oldEcho)
  T
)

;; LOAD-STYLES / LST : Loads production text and dimension styles on demand
(defun c:LOAD-STYLES ( / *error* )
  (defun *error* (msg)
    (CadSetup:UndoReset)
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[LST] Error: " msg))
    )
    (princ)
  )

  (CadSetup:UndoStart)
  (princ "\n[LST] Loading production typography and dimension styles...")
  (CadSetup:LoadStandardStyles)
  (CadSetup:UndoEnd)
  (princ "\n[OK] Standard Text Styles (ARCH-TEXT, ARCH-TITLE) & Dimstyles (ARCH-TICK, ARCH-ARROW) loaded.")
  (princ)
)

(defun c:LST () (c:LOAD-STYLES))

(if *CadSetup-Debug*
  (princ "\n[06_Utilities.lsp] Productivity utilities, styles loader and system repair tools loaded.")
)
(princ)