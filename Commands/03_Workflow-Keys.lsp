;;; ==========================================================================
;;; WORKFLOW-KEYS.LSP - Numbered Rapid Workflow Keys (1, 2, 3, 4...)
;;; ==========================================================================
;;; Category : Rapid Production & Layer-Isolated Workflow Hotkeys
;;; Author   : Haseeb
;;; ==========================================================================
;;;
;;; SUMMARY:
;;;   Provides ergonomic single-digit drafting shortcuts (1, 2, 3, 4) with
;;;   automatic layer creation, target layer isolation, and automatic
;;;   restoration of the drafter's previous active layer upon completion or Esc.
;;;
;;; KEY MAPPINGS:
;;;   [1] / HL : Help Line (Draws on "1-HELP-LINE", restores previous layer)
;;;   [2] / VP : Viewport Boundary (Draws frame on "2-VIEW-PORT", generates
;;;              dynamic ISO A3 scale, title & metadata MText, center snap node)
;;;   [3] / GL : Grid Line (Draws on "3-GRID-LINE", restores previous layer)
;;;   [4] / ML : Material Line (Draws on "4-MAT-LINE", restores previous layer)
;;;
;;; ==========================================================================

(vl-load-com)

;;; --------------------------------------------------------------------------
;;; 0. WORKFLOW LAYER CONFIGURATION
;;; --------------------------------------------------------------------------
;; Layer names can be customized here to match any drawing template/standard:
(setq *WF-LAYER-HL* "1-HELP-LINE")     ;; Key 1: Construction / Help Line
(setq *WF-LAYER-VP* "2-VIEW-PORT")     ;; Key 2: Viewport Boundary & Metadata
(setq *WF-LAYER-GL* "3-GRID-LINE")     ;; Key 3: Structural / Layout Grid Line
(setq *WF-LAYER-ML* "4-MAT-LINE")      ;; Key 4: Material / Profile Line


;;; --------------------------------------------------------------------------
;;; 1. HELP LINE / CONSTRUCTION LINE (1 / HL)
;;; --------------------------------------------------------------------------
(defun c:HL ( / *error* oldLayer oldEcho lay )
  (setq lay (if *WF-LAYER-HL* *WF-LAYER-HL* "1-HELP-LINE"))

  ;; Local error handler to restore settings if canceled via Esc
  (defun *error* (msg)
    (if oldEcho (setvar "CMDECHO" oldEcho))
    (if oldLayer (setvar "CLAYER" oldLayer))
    (if (and msg (not (wcmatch (strcase msg t) "*break*,*cancel*,*exit*")))
      (princ (strcat "\n[HL] Error: " msg))
    )
    (princ)
  )

  ;; Save current environment
  (setq oldEcho  (getvar "CMDECHO")
        oldLayer (getvar "CLAYER"))
  (setvar "CMDECHO" 0)

  ;; Create layer if it doesn't exist and set it current
  (command "._-layer" "_m" lay "")

  ;; Launch LINE command and pause for all user clicks until completed
  (setvar "CMDECHO" 1)
  (command "._line")
  (while (> (getvar "CMDACTIVE") 0)
    (command pause)
  )

  ;; Restore original settings
  (setvar "CMDECHO" 0)
  (setvar "CLAYER" oldLayer)
  (setvar "CMDECHO" oldEcho)

  (princ (strcat "\n[HL] Completed. Restored layer: " oldLayer))
  (princ)
)

;; Command alias: typing 1 executes c:HL
(defun c:1 () (c:HL))


;;; --------------------------------------------------------------------------
;;; 2. VIEWPORT BOUNDARY & METADATA TAGGER (2 / VP)
;;; --------------------------------------------------------------------------
(defun c:VP ( / *error* oldLayer oldEcho lastEnt newEnt minPt maxPt 
                p1 p2 w h sc th offsetIn insInX insInY offsetOut insOutX insOutY
                vpName cdate dotPos dPart tPart dtStr areaStr ratioStr scVal refScStr
                midX midY lay )

  (setq lay (if *WF-LAYER-VP* *WF-LAYER-VP* "2-VIEW-PORT"))

  ;; Local error handler
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

  ;; Save current environment
  (setq oldEcho  (getvar "CMDECHO")
        oldLayer (getvar "CLAYER"))
  (setvar "CMDECHO" 0)

  ;; Create layer if missing and make it active
  (command "._-layer" "_m" lay "")

  ;; Track last entity before drawing
  (setq lastEnt (entlast))

  ;; Draw the guide boundary rectangle
  (setvar "CMDECHO" 1)
  (command "._rectang")
  (while (> (getvar "CMDACTIVE") 0)
    (command pause)
  )
  (setvar "CMDECHO" 0)

  ;; Validate drawn entity
  (setq newEnt (entlast))
  (if (and newEnt (not (eq lastEnt newEnt)))
    (progn
      ;; Retrieve lower-left (p1) and upper-right (p2) corners
      (vla-getboundingbox (vlax-ename->vla-object newEnt) 'minPt 'maxPt)
      (setq p1 (vlax-safearray->list minPt)
            p2 (vlax-safearray->list maxPt))

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

          ;; Base readable text height (3.0 mm plotted height at A3 scale)
          (setq th (* sc 3.0))

          ;; 3. Metadata Computations
          ;; Date & Time from CDATE (YYYYMMDD.HHMMSS)
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

          ;; Surface Area in m²
          (setq areaStr  (rtos (/ (* w h) 1000000.0) 2 2))

          ;; Aspect Ratio (W / H)
          (setq ratioStr (rtos (/ w h) 2 2))

          ;; Nearest Approximate A3 Fit Scale
          (setq scVal    (fix (+ 0.5 sc)))
          (if (< scVal 1) (setq scVal 1))
          (setq refScStr (itoa scVal))

          ;; 4. Inside Block (Bold Name + Small Size Tag)
          ;; Insertion: Bottom-Left with clearance offset
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
              '(71 . 7) ; Attachment: Bottom-Left
              (cons 1 (strcat "{\\fArial|b1;\\H1.5x;" vpName "}\\P"
                              "{\\fArial|b0;\\H0.8x;DIM: " (rtos w 2 0) " \\U+00D7 " (rtos h 2 0) " mm}"))
            )
          )

          ;; 5. Outside Footer Strip (Metadata Bar Underneath)
          ;; Insertion: Top-Left positioned just below the bottom frame line
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
              '(71 . 1) ; Attachment: Top-Left (hangs downward)
              (cons 1 (strcat "{\\fArial|b0;\\H0.7x;"
                              "REF SCALE: ~1:" refScStr
                              "   |   AREA: " areaStr " m\\U+00B2"
                              "   |   RATIO: " ratioStr ":1"
                              "   |   DATE: " dtStr "}"))
            )
          )

          ;; 6. Drafting Center Guide Point (Snap Aid)
          ;; Adds an OSNAP "Node" at the exact center of the bounding box
          (setq midX (/ (+ (car p1) (car p2)) 2.0)
                midY (/ (+ (cadr p1) (cadr p2)) 2.0))
          (entmake
            (list
              '(0 . "POINT")
              (cons 8 lay)
              (list 10 midX midY 0.0)
            )
          )
        )
      )
    )
  )

  ;; Restore original layer and settings
  (setvar "CLAYER" oldLayer)
  (setvar "CMDECHO" oldEcho)

  ;; Print confirmation to command prompt
  (princ (strcat "\n[VP] Completed. Restored layer: " oldLayer))
  (princ)
)

;; Command alias: typing 2 executes c:VP
(defun c:2 () (c:VP))


;;; --------------------------------------------------------------------------
;;; 3. GRID LINE (3 / GL)
;;; --------------------------------------------------------------------------
(defun c:GL ( / *error* oldLayer oldEcho lay )
  (setq lay (if *WF-LAYER-GL* *WF-LAYER-GL* "3-GRID-LINE"))

  ;; Local error handler to restore settings if canceled via Esc
  (defun *error* (msg)
    (if oldEcho (setvar "CMDECHO" oldEcho))
    (if oldLayer (setvar "CLAYER" oldLayer))
    (if (and msg (not (wcmatch (strcase msg t) "*break*,*cancel*,*exit*")))
      (princ (strcat "\n[GL] Error: " msg))
    )
    (princ)
  )

  ;; Save current environment
  (setq oldEcho  (getvar "CMDECHO")
        oldLayer (getvar "CLAYER"))
  (setvar "CMDECHO" 0)

  ;; Create layer if it doesn't exist and set it current
  (command "._-layer" "_m" lay "")

  ;; Launch LINE command and pause for all user clicks until completed
  (setvar "CMDECHO" 1)
  (command "._line")
  (while (> (getvar "CMDACTIVE") 0)
    (command pause)
  )

  ;; Restore original settings
  (setvar "CMDECHO" 0)
  (setvar "CLAYER" oldLayer)
  (setvar "CMDECHO" oldEcho)

  (princ (strcat "\n[GL] Completed. Restored layer: " oldLayer))
  (princ)
)

;; Command alias: typing 3 executes c:GL
(defun c:3 () (c:GL))


;;; --------------------------------------------------------------------------
;;; 4. MATERIAL LINE (4 / ML)
;;; --------------------------------------------------------------------------
(defun c:ML ( / *error* oldLayer oldEcho lay )
  (setq lay (if *WF-LAYER-ML* *WF-LAYER-ML* "4-MAT-LINE"))

  ;; Local error handler to restore settings if canceled via Esc
  (defun *error* (msg)
    (if oldEcho (setvar "CMDECHO" oldEcho))
    (if oldLayer (setvar "CLAYER" oldLayer))
    (if (and msg (not (wcmatch (strcase msg t) "*break*,*cancel*,*exit*")))
      (princ (strcat "\n[ML] Error: " msg))
    )
    (princ)
  )

  ;; Save current environment
  (setq oldEcho  (getvar "CMDECHO")
        oldLayer (getvar "CLAYER"))
  (setvar "CMDECHO" 0)

  ;; Create layer if it doesn't exist and set it current
  (command "._-layer" "_m" lay "")

  ;; Launch LINE command and pause for all user clicks until completed
  (setvar "CMDECHO" 1)
  (command "._line")
  (while (> (getvar "CMDACTIVE") 0)
    (command pause)
  )

  ;; Restore original settings
  (setvar "CMDECHO" 0)
  (setvar "CLAYER" oldLayer)
  (setvar "CMDECHO" oldEcho)

  (princ (strcat "\n[ML] Completed. Restored layer: " oldLayer))
  (princ)
)

;; Command alias: typing 4 executes c:ML
(defun c:4 () (c:ML))


(princ "\n[03_Workflow-Keys.lsp] Workflow keys (1=HL, 2=VP, 3=GL, 4=ML) loaded.")
(princ)
