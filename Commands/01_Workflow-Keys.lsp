;;; ==========================================================================
;;; 01_Workflow-Keys.lsp - Numbered Rapid Workflow Keys (1, 2, 3, 4...)
;;; Layer: Commands (Priority 01)
;;; Author   : Haseeb
;;; ==========================================================================
;;;
;;; SUMMARY:
;;;   Provides ergonomic single-digit drafting shortcuts (1, 2, 3, 4) with
;;;   automatic layer creation, target layer isolation, and automatic
;;;   restoration of the drafter's previous active layer upon completion or Esc.
;;;
;;; KEY MAPPINGS:
;;;   [1] / HL : Help Line (Draws on "01-HELP-LINE", restores previous layer)
;;;   [2] / VP : Viewport Boundary (Draws frame on "02-VIEW-PORT", generates
;;;              dynamic ISO A3 scale, title & metadata MText, center snap node)
;;;   [3] / GL : Grid Line (Draws on "03-GRID-LINE", restores previous layer)
;;;   [4] / ML : Material Line (Draws on "MATERIAL-LINE", restores previous layer)
;;;
;;; ==========================================================================

(vl-load-com)

;;; --------------------------------------------------------------------------
;;; 0. WORKFLOW LAYER CONFIGURATION
;;; --------------------------------------------------------------------------
(setq *WF-LAYER-HL* "01-HELP-LINE")     ;; Key 1: Construction / Help Line
(setq *WF-LAYER-VP* "02-VIEW-PORT")     ;; Key 2: Viewport Boundary & Metadata
(setq *WF-LAYER-GL* "03-GRID-LINE")     ;; Key 3: Structural / Layout Grid Line
(setq *WF-LAYER-ML* "MATERIAL-LINE")    ;; Key 4: Material / Profile Line


;;; --------------------------------------------------------------------------
;;; 1. HELP LINE / CONSTRUCTION LINE (1 / HL)
;;; --------------------------------------------------------------------------
(defun c:HL ( / *error* oldLayer oldEcho lay )
  (setq lay (if *WF-LAYER-HL* *WF-LAYER-HL* "01-HELP-LINE"))

  (defun *error* (msg)
    (if oldEcho (setvar "CMDECHO" oldEcho))
    (if oldLayer (setvar "CLAYER" oldLayer))
    (if (and msg (not (wcmatch (strcase msg t) "*break*,*cancel*,*exit*")))
      (princ (strcat "\n[HL] Error: " msg))
    )
    (princ)
  )

  (setq oldEcho  (getvar "CMDECHO")
        oldLayer (getvar "CLAYER"))
  (setvar "CMDECHO" 0)

  ;; Safely set current layer using helper
  (CadSetup:SetCurrentLayerSafe lay)

  (setvar "CMDECHO" 1)
  (command "._line")
  (while (> (getvar "CMDACTIVE") 0)
    (command pause)
  )

  (setvar "CMDECHO" 0)
  (setvar "CLAYER" oldLayer)
  (setvar "CMDECHO" oldEcho)

  (princ (strcat "\n[HL] Completed. Restored layer: " oldLayer))
  (princ)
)

(defun c:1 () (c:HL))


;;; --------------------------------------------------------------------------
;;; 2. VIEWPORT BOUNDARY & METADATA TAGGER (2 / VP)
;;; --------------------------------------------------------------------------
(defun c:VP ( / *error* oldLayer oldEcho lastEnt newEnt bbox
                p1 p2 w h sc th offsetIn insInX insInY offsetOut insOutX insOutY
                vpName cdate dotPos dPart tPart dtStr areaStr ratioStr scVal refScStr
                midPt lay )

  (setq lay (if *WF-LAYER-VP* *WF-LAYER-VP* "02-VIEW-PORT"))

  (defun *error* (msg)
    (if oldEcho (setvar "CMDECHO" oldEcho))
    (if oldLayer 
      (progn
        (setvar "CLAYER" oldLayer)
        (princ (strcat "\n[VP] Restored layer: " oldLayer))
      )
    )
    (if (and msg (not (wcmatch (strcase msg t) "*break*,*cancel*,*exit*")))
      (princ (strcat "\n[VP] Error: " msg))
    )
    (princ)
  )

  (setq oldEcho  (getvar "CMDECHO")
        oldLayer (getvar "CLAYER"))
  (setvar "CMDECHO" 0)

  (CadSetup:SetCurrentLayerSafe lay)

  (setq lastEnt (entlast))

  (setvar "CMDECHO" 1)
  (command "._rectang")
  (while (> (getvar "CMDACTIVE") 0)
    (command pause)
  )
  (setvar "CMDECHO" 0)

  (setq newEnt (entlast))
  (if (and newEnt (not (eq lastEnt newEnt)))
    (progn
      (setq bbox (CadSetup:GetBoundingBox (vlax-ename->vla-object newEnt)))
      (if bbox
        (progn
          (setq p1 (car bbox)
                p2 (cadr bbox))

          (setq w (abs (- (car p2) (car p1)))
                h (abs (- (cadr p2) (cadr p1))))

          (if (and (> w 0.0) (> h 0.0))
            (progn
              ;; 1. Prompt for Viewport Title
              (setq vpName (getstring t "\nEnter Viewport Name <VIEW PORT>: "))
              (if (or (null vpName) (= vpName ""))
                (setq vpName "VIEW PORT")
              )

              ;; 2. A3 Dynamic Scale Calculation (ISO A3 = 420 x 297 mm)
              (if (>= w h)
                (setq sc (max (/ w 420.0) (/ h 297.0)))
                (setq sc (max (/ w 297.0) (/ h 420.0)))
              )
              (if (< sc 1.0) (setq sc 1.0))

              (setq th (* sc 3.0))

              ;; 3. Metadata Computations
              (setq cdate (rtos (getvar "CDATE") 2 6))
              (setq dotPos (vl-string-search "." cdate))
              (if dotPos
                (progn
                  (setq dPart (substr cdate 1 dotPos)
                        tPart (substr cdate (+ dotPos 2)))
                  (while (< (strlen tPart) 4) (setq tPart (strcat tPart "0")))
                  (setq dtStr (strcat (substr dPart 1 4) "-" (substr dPart 5 2) "-" (substr dPart 7 2)
                                      "  " (substr tPart 1 2) ":" (substr tPart 3 2)))
                )
                (setq dtStr (substr cdate 1 10))
              )

              (setq areaStr  (rtos (/ (* w h) 1000000.0) 2 2))
              (setq ratioStr (rtos (/ w h) 2 2))

              (setq scVal    (fix (+ 0.5 sc)))
              (if (< scVal 1) (setq scVal 1))
              (setq refScStr (itoa scVal))

              ;; 4. Inside Block Tag
              (setq offsetIn (* th 1.2)
                    insInX   (+ (car p1) offsetIn)
                    insInY   (+ (cadr p1) offsetIn))

              (entmake
                (list
                  '(0 . "MTEXT")
                  '(100 . "AcDbEntity")
                  '(100 . "AcDbMText")
                  (cons 8 lay)
                  (list 10 insInX insInY 0.0)
                  (cons 40 th)
                  '(71 . 7)
                  (cons 1 (strcat "{\\fArial|b1;\\H1.5x;" vpName "}\\P"
                                  "{\\fArial|b0;\\H0.8x;DIM: " (rtos w 2 0) " \\U+00D7 " (rtos h 2 0) " mm}"))
                )
              )

              ;; 5. Outside Footer Strip
              (setq offsetOut (* th 0.75)
                    insOutX   (car p1)
                    insOutY   (- (cadr p1) offsetOut))

              (entmake
                (list
                  '(0 . "MTEXT")
                  '(100 . "AcDbEntity")
                  '(100 . "AcDbMText")
                  (cons 8 lay)
                  (list 10 insOutX insOutY 0.0)
                  (cons 40 th)
                  '(71 . 1)
                  (cons 1 (strcat "{\\fArial|b0;\\H0.7x;"
                                  "REF SCALE: ~1:" refScStr
                                  "   |   AREA: " areaStr " m\\U+00B2"
                                  "   |   RATIO: " ratioStr ":1"
                                  "   |   DATE: " dtStr "}"))
                )
              )

              ;; 6. Drafting Center Guide Point using MidPoint helper
              (setq midPt (CadSetup:MidPoint p1 p2))
              (entmake
                (list
                  '(0 . "POINT")
                  (cons 8 lay)
                  (cons 10 midPt)
                )
              )
            )
          )
        )
      )
    )
  )

  (setvar "CLAYER" oldLayer)
  (setvar "CMDECHO" oldEcho)

  (princ (strcat "\n[VP] Completed. Restored layer: " oldLayer))
  (princ)
)

(defun c:2 () (c:VP))


;;; --------------------------------------------------------------------------
;;; 3. GRID LINE (3 / GL)
;;; --------------------------------------------------------------------------
(defun c:GL ( / *error* oldLayer oldEcho lay )
  (setq lay (if *WF-LAYER-GL* *WF-LAYER-GL* "03-GRID-LINE"))

  (defun *error* (msg)
    (if oldEcho (setvar "CMDECHO" oldEcho))
    (if oldLayer (setvar "CLAYER" oldLayer))
    (if (and msg (not (wcmatch (strcase msg t) "*break*,*cancel*,*exit*")))
      (princ (strcat "\n[GL] Error: " msg))
    )
    (princ)
  )

  (setq oldEcho  (getvar "CMDECHO")
        oldLayer (getvar "CLAYER"))
  (setvar "CMDECHO" 0)

  (CadSetup:SetCurrentLayerSafe lay)

  (setvar "CMDECHO" 1)
  (command "._line")
  (while (> (getvar "CMDACTIVE") 0)
    (command pause)
  )

  (setvar "CMDECHO" 0)
  (setvar "CLAYER" oldLayer)
  (setvar "CMDECHO" oldEcho)

  (princ (strcat "\n[GL] Completed. Restored layer: " oldLayer))
  (princ)
)

(defun c:3 () (c:GL))


;;; --------------------------------------------------------------------------
;;; 4. MATERIAL LINE (4 / ML)
;;; --------------------------------------------------------------------------
(defun c:ML ( / *error* oldLayer oldEcho lay )
  (setq lay (if *WF-LAYER-ML* *WF-LAYER-ML* "MATERIAL-LINE"))

  (defun *error* (msg)
    (if oldEcho (setvar "CMDECHO" oldEcho))
    (if oldLayer (setvar "CLAYER" oldLayer))
    (if (and msg (not (wcmatch (strcase msg t) "*break*,*cancel*,*exit*")))
      (princ (strcat "\n[ML] Error: " msg))
    )
    (princ)
  )

  (setq oldEcho  (getvar "CMDECHO")
        oldLayer (getvar "CLAYER"))
  (setvar "CMDECHO" 0)

  (CadSetup:SetCurrentLayerSafe lay)

  (setvar "CMDECHO" 1)
  (command "._line")
  (while (> (getvar "CMDACTIVE") 0)
    (command pause)
  )

  (setvar "CMDECHO" 0)
  (setvar "CLAYER" oldLayer)
  (setvar "CMDECHO" oldEcho)

  (princ (strcat "\n[ML] Completed. Restored layer: " oldLayer))
  (princ)
)

(defun c:4 () (c:ML))


(if *CadSetup-Debug*
  (princ "\n[01_Workflow-Keys.lsp] Workflow keys (1=HL, 2=VP, 3=GL, 4=ML) loaded.")
)
(princ)
