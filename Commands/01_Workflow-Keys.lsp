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
;;;   [3] / GL : Grid Line & System Generator (Interactive DCL grid maker,
;;;              custom bay parsing, auto-bubbles, dimensions & single-line mode)
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
;;; 1. HELP LINE / CONSTRUCTION LINE WITH LIVE POINT NODES (1 / HL)
;;; --------------------------------------------------------------------------
(vl-load-com)

(defun c:HL ( / *error* oldLayer oldEcho lay pt1 pt2 history top
                firstPtEnt lEnt pEnt startPt _mkPoint _mkLine )
  (setq lay (if *WF-LAYER-HL* *WF-LAYER-HL* "01-HELP-LINE"))

  (defun *error* (msg)
    (if oldEcho  (setvar "CMDECHO" oldEcho))
    (if oldLayer (setvar "CLAYER" oldLayer))
    (if (and msg (not (wcmatch (strcase msg t) "*break*,*cancel*,*exit*")))
      (princ (strcat "\n[HL] Error: " msg))
    )
    (princ)
  )

  ;; Helper: Create point entity on help layer (translates UCS -> WCS)
  (defun _mkPoint (p)
    (entmake
      (list
        '(0 . "POINT")
        (cons 8 lay)
        (cons 10 (trans p 1 0))
      )
    )
    (entlast)
  )

  ;; Helper: Create line entity on help layer (translates UCS -> WCS)
  (defun _mkLine (p1 p2)
    (entmake
      (list
        '(0 . "LINE")
        (cons 8 lay)
        (cons 10 (trans p1 1 0))
        (cons 11 (trans p2 1 0))
      )
    )
    (entlast)
  )

  (setq oldEcho  (getvar "CMDECHO")
        oldLayer (getvar "CLAYER"))
  (setvar "CMDECHO" 0)

  ;; Safely set current layer using helper
  (CadSetup:SetCurrentLayerSafe lay)

  ;; Interactive loop
  (setq pt1 (getpoint "\nSpecify first point: "))
  (if pt1
    (progn
      ;; Live point on the very first click
      (setq firstPtEnt (_mkPoint pt1)
            history    (list (list nil nil firstPtEnt pt1)))

      (while pt1
        (if (> (length history) 1)
          (initget "Undo Close")
          (initget "Undo")
        )
        ;; Native rubber-band line from pt1 to cursor
        (setq pt2 (getpoint pt1 (if (> (length history) 1)
                                  "\nSpecify next point or [Close/Undo]: "
                                  "\nSpecify next point or [Undo]: ")))
        (cond
          ;; Undo handling
          ((= pt2 "Undo")
           (setq top     (car history)
                 history (cdr history))
           (if (cadr top)  (entdel (cadr top)))   ; delete line segment
           (if (caddr top) (entdel (caddr top)))  ; delete point node
           (if history
             (setq pt1 (last (car history)))
             (progn
               ;; Undid the initial click; prompt for start point again
               (setq pt1 (getpoint "\nSpecify first point: "))
               (if pt1
                 (setq firstPtEnt (_mkPoint pt1)
                       history    (list (list nil nil firstPtEnt pt1)))
               )
             )
           )
          )

          ;; Close polygon handling
          ((= pt2 "Close")
           (setq startPt (last (last history)))
           (_mkLine pt1 startPt)
           (setq pt1 nil)
          )

          ;; Next point picked
          ((listp pt2)
           (setq lEnt (_mkLine pt1 pt2)
                 pEnt (_mkPoint pt2))
           (setq history (cons (list pt1 lEnt pEnt pt2) history))
           (setq pt1 pt2)
          )

          ;; Enter / Space / nil to exit
          (t
           (setq pt1 nil)
          )
        )
      )
    )
  )

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
;;; 3. STRUCTURAL & ARCHITECTURAL GRID SYSTEM GENERATOR (3 / GL)
;;; --------------------------------------------------------------------------

;; AutoLISP compatibility utility for fboundp (function bound predicate)
(if (not (boundp 'fboundp))
  (defun fboundp (sym)
    (and (symbolp sym)
         (boundp sym)
         (member (type (vl-symbol-value sym)) '(SUBR USUBR EXRXSUBR)))
  )
)

;; Global single-line and grid session attributes
(if (null *GL-SINGLE-TAG*)
  (setq *GL-SINGLE-TAG* "1")
)
(if (null *GL-BUBBLE-RAD*)
  (setq *GL-BUBBLE-RAD* 450.0)
)
(if (null *GL-GRID-EXT*)
  (setq *GL-GRID-EXT* 1000.0)
)
(if (null *GL-TEXT-HT*)
  (setq *GL-TEXT-HT* 300.0)
)
(if (null *GL-BUBBLE-POS*)
  (setq *GL-BUBBLE-POS* "End")
)

;; Helper: Parse spacing string into list of real bay distances
;; Handles: "4*6000", "6000, 5000, 7500", "3*6000, 4500, 2*3000"
(defun CadSetup:ParseGridSpacings (str / len i ch curToken tokens starPos count val res)
  (if (or (null str) (/= (type str) 'STR))
    nil
    (progn
      ;; Strip spaces around '*' (e.g. "4 * 6000" -> "4*6000")
      (while (vl-string-search " *" str)
        (setq str (vl-string-subst "*" " *" str))
      )
      (while (vl-string-search "* " str)
        (setq str (vl-string-subst "*" "* " str))
      )
      (setq len (strlen str)
            i 1
            curToken ""
            tokens nil)
      (while (<= i len)
        (setq ch (substr str i 1))
        (if (vl-position ch '(" " "," ";" "\t"))
          (progn
            (if (/= curToken "")
              (setq tokens (cons curToken tokens)
                    curToken ""))
          )
          (setq curToken (strcat curToken ch))
        )
        (setq i (1+ i))
      )
      (if (/= curToken "")
        (setq tokens (cons curToken tokens))
      )
      (setq tokens (reverse tokens))

      (setq res nil)
      (foreach tok tokens
        (setq starPos (vl-string-search "*" tok))
        (if starPos
          (progn
            (setq count (atoi (substr tok 1 starPos))
                  val   (distof (substr tok (+ starPos 2))))
            (if (and (> count 0) val (> val 0.0))
              (repeat count
                (setq res (cons (float val) res))
              )
            )
          )
          (progn
            (setq val (distof tok))
            (if (and val (> val 0.0))
              (setq res (cons (float val) res))
            )
          )
        )
      )
      (reverse res)
    )
  )
)

;; Helper: Auto-increment alpha strings ("A"->"B", "Z"->"AA", "AA"->"AB")
(defun CadSetup:IncrementAlpha (str / len lastChar rest)
  (setq len (strlen str))
  (if (<= len 0)
    "A"
    (progn
      (setq lastChar (ascii (strcase (substr str len 1)))
            rest     (substr str 1 (1- len)))
      (cond
        ((= lastChar 90) ;; 'Z'
         (if (= rest "")
           "AA"
           (strcat (CadSetup:IncrementAlpha rest) "A")
         ))
        ((and (>= lastChar 65) (< lastChar 90))
         (strcat rest (chr (1+ lastChar))))
        (t (strcat str "1"))
      )
    )
  )
)

;; Helper: Smart tag incrementer for numbers, letters, or compound tags ("1"->"2", "A"->"B", "GL-01"->"GL-02")
(defun CadSetup:IncrementGridTag (tag / len i prefix suffix nextNum nextNumStr)
  (if (or (null tag) (= tag ""))
    "1"
    (progn
      (setq len (strlen tag))
      (if (numberp (read tag))
        (itoa (1+ (atoi tag)))
        (progn
          (setq i len)
          (while (and (> i 0) (<= 48 (ascii (substr tag i 1)) 57))
            (setq i (1- i))
          )
          (if (< i len)
            (progn
              (setq prefix (substr tag 1 i)
                    suffix (substr tag (1+ i))
                    nextNum (1+ (atoi suffix))
                    nextNumStr (itoa nextNum))
              ;; Preserve zero-padding if any
              (while (< (strlen nextNumStr) (strlen suffix))
                (setq nextNumStr (strcat "0" nextNumStr))
              )
              (strcat prefix nextNumStr)
            )
            (CadSetup:IncrementAlpha tag)
          )
        )
      )
    )
  )
)

;; Helper: Interactive Sub-Menu Loop for Single Grid Line Attributes
(defun CadSetup:SingleGridSettingsPrompt ( / optVal inputVal loop )
  (princ (strcat "\n[GL Settings] Current: Placement=" *GL-BUBBLE-POS*
                 ", Radius=" (rtos *GL-BUBBLE-RAD* 2 1)
                 ", Extension=" (rtos *GL-GRID-EXT* 2 1)
                 ", TextHeight=" (rtos *GL-TEXT-HT* 2 1)))
  (setq loop T)
  (while loop
    (initget "Placement Radius Extension Text eXit P R E T X")
    (setq optVal (getkword "\nGrid Settings [Placement/Radius/Extension/Text/eXit] <eXit>: "))
    (cond
      ((or (= optVal "Placement") (= optVal "P"))
       (initget "End Start Both None E S B N")
       (setq inputVal (getkword (strcat "\nBubble Placement [End/Start/Both/None] <" *GL-BUBBLE-POS* ">: ")))
       (cond
         ((or (= inputVal "End") (= inputVal "E"))   (setq *GL-BUBBLE-POS* "End"))
         ((or (= inputVal "Start") (= inputVal "S")) (setq *GL-BUBBLE-POS* "Start"))
         ((or (= inputVal "Both") (= inputVal "B"))  (setq *GL-BUBBLE-POS* "Both"))
         ((or (= inputVal "None") (= inputVal "N"))  (setq *GL-BUBBLE-POS* "None"))
       )
       (princ (strcat "\n[GL Settings] Placement -> " *GL-BUBBLE-POS*))
      )
      ((or (= optVal "Radius") (= optVal "R"))
       (setq inputVal (getdist (strcat "\nBubble Radius <" (rtos *GL-BUBBLE-RAD* 2 1) ">: ")))
       (if (and inputVal (> inputVal 0.0))
         (setq *GL-BUBBLE-RAD* inputVal)
       )
       (princ (strcat "\n[GL Settings] Radius -> " (rtos *GL-BUBBLE-RAD* 2 1)))
      )
      ((or (= optVal "Extension") (= optVal "E"))
       (setq inputVal (getdist (strcat "\nLine Extension (Overshoot) <" (rtos *GL-GRID-EXT* 2 1) ">: ")))
       (if (and inputVal (> inputVal 0.0))
         (setq *GL-GRID-EXT* inputVal)
       )
       (princ (strcat "\n[GL Settings] Extension -> " (rtos *GL-GRID-EXT* 2 1)))
      )
      ((or (= optVal "Text") (= optVal "T"))
       (setq inputVal (getdist (strcat "\nText Height <" (rtos *GL-TEXT-HT* 2 1) ">: ")))
       (if (and inputVal (> inputVal 0.0))
         (setq *GL-TEXT-HT* inputVal)
       )
       (princ (strcat "\n[GL Settings] Text Height -> " (rtos *GL-TEXT-HT* 2 1)))
      )
      (t
       (setq loop nil)
      )
    )
  )
  (princ)
)

;; Helper: Draw Single Grid Line with Auto-Increment Bubble & Tags
(defun CadSetup:DrawSingleGridLine ( / *error* oldLayer oldEcho pt1 pt2 ang rad ext th pos
                                       lbl centerPt lineStart lineEnd mainLoop )
  (defun *error* (msg)
    (if oldEcho (setvar "CMDECHO" oldEcho))
    (if oldLayer (setvar "CLAYER" oldLayer))
    (CadSetup:UndoEnd)
    (if (and msg (not (wcmatch (strcase msg t) "*break*,*cancel*,*exit*")))
      (princ (strcat "\n[GL-Single] Error: " msg))
    )
    (princ)
  )

  (setq oldEcho  (getvar "CMDECHO")
        oldLayer (getvar "CLAYER"))
  (setvar "CMDECHO" 0)

  ;; Ensure production layers
  (if (and (boundp 'CadSetup:EnsureLayer) CadSetup:EnsureLayer)
    (progn
      (CadSetup:EnsureLayer "03-GRID-LINE" 8 "100,100,100" "CONTINUOUS" 18 T "NONE" 1.0 0.0 0 nil "Structural Grid Lines")
      (CadSetup:EnsureLayer "R-ANNO-SYMB"  2 "170,115,0"   "CONTINUOUS" 25 T "NONE" 1.0 0.0 0 nil "Grid Bubbles & Markers")
    )
  )

  (princ "\n--- Single Grid Line Mode (Press Enter/Esc at prompt to finish) ---")
  (setq mainLoop T)
  (while mainLoop
    (initget 128 "Settings S")
    (setq pt1 (getpoint "\n[GL-Single] Specify Start Point or [Settings]: "))
    (cond
      ((or (eq pt1 "Settings") (eq pt1 "S"))
       (CadSetup:SingleGridSettingsPrompt)
      )
      ((listp pt1)
       (if (setq pt2 (getpoint pt1 "\n[GL-Single] Specify End Point: "))
         (progn
           (setq lbl (getstring t (strcat "\n[GL-Single] Enter Grid Bubble Label <" *GL-SINGLE-TAG* ">: ")))
           (if (or (null lbl) (= lbl ""))
             (setq lbl *GL-SINGLE-TAG*)
           )

           (CadSetup:UndoStart)

           (setq rad *GL-BUBBLE-RAD*
                 ext *GL-GRID-EXT*
                 th  *GL-TEXT-HT*
                 pos *GL-BUBBLE-POS*
                 ang (angle pt1 pt2))

           ;; Calculate line endpoints based on overshoot extension
           (setq lineStart (if (or (= pos "Start") (= pos "Both") (= pos "None"))
                             (polar pt1 (+ ang pi) ext)
                             pt1))
           (setq lineEnd   (if (or (= pos "End") (= pos "Both") (= pos "None"))
                             (polar pt2 ang ext)
                             pt2))

           ;; 1. Draw Grid Line on 03-GRID-LINE
           (entmake
             (list
               '(0 . "LINE")
               '(100 . "AcDbEntity")
               (cons 8 "03-GRID-LINE")
               '(100 . "AcDbLine")
               (cons 10 (trans lineStart 1 0))
               (cons 11 (trans lineEnd 1 0))
             )
           )

           ;; 2. Draw Start Bubble (if Start or Both)
           (if (or (= pos "Start") (= pos "Both"))
             (progn
               (setq centerPt (polar pt1 (+ ang pi) (+ ext rad)))
               (entmake
                 (list
                   '(0 . "CIRCLE")
                   '(100 . "AcDbEntity")
                   (cons 8 "R-ANNO-SYMB")
                   '(100 . "AcDbCircle")
                   (cons 10 (trans centerPt 1 0))
                   (cons 40 rad)
                 )
               )
               (entmake
                 (list
                   '(0 . "MTEXT")
                   '(100 . "AcDbEntity")
                   (cons 8 "R-ANNO-SYMB")
                   '(100 . "AcDbMText")
                   (cons 10 (trans centerPt 1 0))
                   (cons 40 th)
                   '(71 . 5)  ;; Middle Center
                   (cons 1 (strcat "{\\fArial|b1;" lbl "}"))
                 )
               )
             )
           )

           ;; 3. Draw End Bubble (if End or Both)
           (if (or (= pos "End") (= pos "Both"))
             (progn
               (setq centerPt (polar pt2 ang (+ ext rad)))
               (entmake
                 (list
                   '(0 . "CIRCLE")
                   '(100 . "AcDbEntity")
                   (cons 8 "R-ANNO-SYMB")
                   '(100 . "AcDbCircle")
                   (cons 10 (trans centerPt 1 0))
                   (cons 40 rad)
                 )
               )
               (entmake
                 (list
                   '(0 . "MTEXT")
                   '(100 . "AcDbEntity")
                   (cons 8 "R-ANNO-SYMB")
                   '(100 . "AcDbMText")
                   (cons 10 (trans centerPt 1 0))
                   (cons 40 th)
                   '(71 . 5)  ;; Middle Center
                   (cons 1 (strcat "{\\fArial|b1;" lbl "}"))
                 )
               )
             )
           )

           (CadSetup:UndoEnd)

           ;; Increment tag for next line
           (setq *GL-SINGLE-TAG* (CadSetup:IncrementGridTag lbl))
           (princ (strcat "\n[GL-Single] Placed line '" lbl "'. Next default tag: '" *GL-SINGLE-TAG* "'."))
         )
         (setq mainLoop nil)
       )
      )
      (t
       (setq mainLoop nil)
      )
    )
  )

  (setvar "CLAYER" oldLayer)
  (setvar "CMDECHO" oldEcho)
  (princ (strcat "\n[GL-Single] Completed. Restored layer: " oldLayer))
  (princ)
)

;; Helper: Draw Full Grid System with Bubbles and Dimensions
(defun CadSetup:DrawGridSystem ( xSpacings xStartTag ySpacings yStartTag
                                 bubblePos bubbleRad gridExt textHt
                                 addBayDims addTotalDims dimOffset /
                                 *error* oldLayer oldEcho insPt rotAngle cosA sinA
                                 cumX curX cumY curY xTot yTot
                                 _toWorld i curTag xVal yVal
                                 pStart pEnd pCircle
                                 hasTopBubble hasBottomBubble hasLeftBubble hasRightBubble
                                 yBayDim yTotDim xBayDim xTotDim p1 p2 pDim )

  (defun *error* (msg)
    (if oldEcho (setvar "CMDECHO" oldEcho))
    (if oldLayer (setvar "CLAYER" oldLayer))
    (CadSetup:UndoEnd)
    (if (and msg (not (wcmatch (strcase msg t) "*break*,*cancel*,*exit*")))
      (princ (strcat "\n[GL-Grid Error]: " msg))
    )
    (princ)
  )

  (setq oldEcho  (getvar "CMDECHO")
        oldLayer (getvar "CLAYER"))
  (setvar "CMDECHO" 0)

  ;; 1. Prompt for Insertion Point and Rotation
  (setq insPt (getpoint "\n[GL] Specify Grid System Origin Point (Intersection 1-A): "))
  (if (null insPt)
    (progn
      (princ "\n[GL] Grid insertion cancelled.")
      (setvar "CLAYER" oldLayer)
      (setvar "CMDECHO" oldEcho)
      (exit)
    )
  )

  (setq rotAngle (getangle insPt "\n[GL] Specify Grid Rotation Angle <0.0>: "))
  (if (null rotAngle) (setq rotAngle 0.0))

  ;; Ensure Layers
  (if (and (boundp 'CadSetup:EnsureLayer) CadSetup:EnsureLayer)
    (progn
      (CadSetup:EnsureLayer "03-GRID-LINE" 8  "100,100,100" "CONTINUOUS" 18 T "NONE" 1.0 0.0 0 nil "Structural Grid Lines")
      (CadSetup:EnsureLayer "R-ANNO-SYMB"  2  "170,115,0"   "CONTINUOUS" 25 T "NONE" 1.0 0.0 0 nil "Grid Bubbles & Markers")
      (CadSetup:EnsureLayer "R-ANNO-DIMS"  20 "180,75,0"    "CONTINUOUS" 18 T "NONE" 1.0 0.0 0 nil "Grid Dimensions")
    )
  )

  (CadSetup:UndoStart)

  ;; 2. Coordinate Transformation Math
  (setq cosA (cos rotAngle)
        sinA (sin rotAngle))

  (defun _toWorld (lx ly)
    (list
      (+ (car insPt)  (- (* lx cosA) (* ly sinA)))
      (+ (cadr insPt) (+ (* lx sinA) (* ly cosA)))
      (if (caddr insPt) (caddr insPt) 0.0)
    )
  )

  ;; 3. Compute Cumulative Coordinate Lists
  ;; X coordinates (vertical lines at each x)
  (setq cumX (list 0.0)
        curX 0.0)
  (foreach sp xSpacings
    (setq curX (+ curX sp)
          cumX (cons curX cumX))
  )
  (setq cumX (reverse cumX))
  (setq xTot (last cumX))

  ;; Y coordinates (horizontal lines at each y)
  (setq cumY (list 0.0)
        curY 0.0)
  (foreach sp ySpacings
    (setq curY (+ curY sp)
          cumY (cons curY cumY))
  )
  (setq cumY (reverse cumY))
  (setq yTot (last cumY))

  ;; Determine bubble flags:
  ;; 0 = Both Ends, 1 = Top & Left Only, 2 = Bottom & Right Only, 3 = None
  (setq hasTopBubble    (or (= bubblePos 0) (= bubblePos 1))
        hasBottomBubble (or (= bubblePos 0) (= bubblePos 2))
        hasLeftBubble   (or (= bubblePos 0) (= bubblePos 1))
        hasRightBubble  (or (= bubblePos 0) (= bubblePos 2)))

  ;; 4. Draw Vertical Grid Lines (X-Axis)
  (setq curTag xStartTag
        i 0)
  (while (< i (length cumX))
    (setq xVal (nth i cumX))

    ;; Grid Line (03-GRID-LINE)
    (setq pStart (_toWorld xVal (- 0.0 gridExt))
          pEnd   (_toWorld xVal (+ yTot gridExt)))
    (entmake
      (list
        '(0 . "LINE")
        '(100 . "AcDbEntity")
        (cons 8 "03-GRID-LINE")
        '(100 . "AcDbLine")
        (cons 10 (trans pStart 1 0))
        (cons 11 (trans pEnd 1 0))
      )
    )

    ;; Top Bubble
    (if hasTopBubble
      (progn
        (setq pCircle (_toWorld xVal (+ yTot gridExt bubbleRad)))
        (entmake
          (list
            '(0 . "CIRCLE")
            '(100 . "AcDbEntity")
            (cons 8 "R-ANNO-SYMB")
            '(100 . "AcDbCircle")
            (cons 10 (trans pCircle 1 0))
            (cons 40 bubbleRad)
          )
        )
        (entmake
          (list
            '(0 . "MTEXT")
            '(100 . "AcDbEntity")
            (cons 8 "R-ANNO-SYMB")
            '(100 . "AcDbMText")
            (cons 10 (trans pCircle 1 0))
            (cons 40 textHt)
            '(71 . 5) ;; Middle Center
            (cons 1 (strcat "{\\fArial|b1;" curTag "}"))
          )
        )
      )
    )

    ;; Bottom Bubble
    (if hasBottomBubble
      (progn
        (setq pCircle (_toWorld xVal (- 0.0 gridExt bubbleRad)))
        (entmake
          (list
            '(0 . "CIRCLE")
            '(100 . "AcDbEntity")
            (cons 8 "R-ANNO-SYMB")
            '(100 . "AcDbCircle")
            (cons 10 (trans pCircle 1 0))
            (cons 40 bubbleRad)
          )
        )
        (entmake
          (list
            '(0 . "MTEXT")
            '(100 . "AcDbEntity")
            (cons 8 "R-ANNO-SYMB")
            '(100 . "AcDbMText")
            (cons 10 (trans pCircle 1 0))
            (cons 40 textHt)
            '(71 . 5) ;; Middle Center
            (cons 1 (strcat "{\\fArial|b1;" curTag "}"))
          )
        )
      )
    )

    (setq curTag (CadSetup:IncrementGridTag curTag)
          i (1+ i))
  )

  ;; 5. Draw Horizontal Grid Lines (Y-Axis)
  (setq curTag yStartTag
        i 0)
  (while (< i (length cumY))
    (setq yVal (nth i cumY))

    ;; Grid Line (03-GRID-LINE)
    (setq pStart (_toWorld (- 0.0 gridExt) yVal)
          pEnd   (_toWorld (+ xTot gridExt) yVal))
    (entmake
      (list
        '(0 . "LINE")
        '(100 . "AcDbEntity")
        (cons 8 "03-GRID-LINE")
        '(100 . "AcDbLine")
        (cons 10 (trans pStart 1 0))
        (cons 11 (trans pEnd 1 0))
      )
    )

    ;; Left Bubble
    (if hasLeftBubble
      (progn
        (setq pCircle (_toWorld (- 0.0 gridExt bubbleRad) yVal))
        (entmake
          (list
            '(0 . "CIRCLE")
            '(100 . "AcDbEntity")
            (cons 8 "R-ANNO-SYMB")
            '(100 . "AcDbCircle")
            (cons 10 (trans pCircle 1 0))
            (cons 40 bubbleRad)
          )
        )
        (entmake
          (list
            '(0 . "MTEXT")
            '(100 . "AcDbEntity")
            (cons 8 "R-ANNO-SYMB")
            '(100 . "AcDbMText")
            (cons 10 (trans pCircle 1 0))
            (cons 40 textHt)
            '(71 . 5) ;; Middle Center
            (cons 1 (strcat "{\\fArial|b1;" curTag "}"))
          )
        )
      )
    )

    ;; Right Bubble
    (if hasRightBubble
      (progn
        (setq pCircle (_toWorld (+ xTot gridExt bubbleRad) yVal))
        (entmake
          (list
            '(0 . "CIRCLE")
            '(100 . "AcDbEntity")
            (cons 8 "R-ANNO-SYMB")
            '(100 . "AcDbCircle")
            (cons 10 (trans pCircle 1 0))
            (cons 40 bubbleRad)
          )
        )
        (entmake
          (list
            '(0 . "MTEXT")
            '(100 . "AcDbEntity")
            (cons 8 "R-ANNO-SYMB")
            '(100 . "AcDbMText")
            (cons 10 (trans pCircle 1 0))
            (cons 40 textHt)
            '(71 . 5) ;; Middle Center
            (cons 1 (strcat "{\\fArial|b1;" curTag "}"))
          )
        )
      )
    )

    (setq curTag (CadSetup:IncrementGridTag curTag)
          i (1+ i))
  )

  ;; 6. Generate Automated Dimensions (R-ANNO-DIMS)
  (if (or (= addBayDims 1) (= addTotalDims 1))
    (progn
      (setvar "CLAYER" "R-ANNO-DIMS")

      ;; X-Axis Dimension Placements (Above Top)
      (setq yBayDim (+ yTot gridExt (if hasTopBubble (* 2.0 bubbleRad) 0.0) dimOffset))
      (setq yTotDim (+ yBayDim (if (= addBayDims 1) dimOffset 0.0)))

      ;; Bay Dimensions along X
      (if (= addBayDims 1)
        (progn
          (setq i 0)
          (while (< i (1- (length cumX)))
            (setq p1   (_toWorld (nth i cumX) yTot)
                  p2   (_toWorld (nth (1+ i) cumX) yTot)
                  pDim (_toWorld (* 0.5 (+ (nth i cumX) (nth (1+ i) cumX))) yBayDim))
            (command "._dimaligned" "_non" p1 "_non" p2 "_non" pDim)
            (setq i (1+ i))
          )
        )
      )

      ;; Total / Overall Dimension along X
      (if (= addTotalDims 1)
        (progn
          (setq p1   (_toWorld 0.0 yTot)
                p2   (_toWorld xTot yTot)
                pDim (_toWorld (* 0.5 xTot) yTotDim))
          (command "._dimaligned" "_non" p1 "_non" p2 "_non" pDim)
        )
      )

      ;; Y-Axis Dimension Placements (To Left)
      (setq xBayDim (- 0.0 (+ gridExt (if hasLeftBubble (* 2.0 bubbleRad) 0.0) dimOffset)))
      (setq xTotDim (- xBayDim (if (= addBayDims 1) dimOffset 0.0)))

      ;; Bay Dimensions along Y
      (if (= addBayDims 1)
        (progn
          (setq i 0)
          (while (< i (1- (length cumY)))
            (setq p1   (_toWorld 0.0 (nth i cumY))
                  p2   (_toWorld 0.0 (nth (1+ i) cumY))
                  pDim (_toWorld xBayDim (* 0.5 (+ (nth i cumY) (nth (1+ i) cumY)))))
            (command "._dimaligned" "_non" p1 "_non" p2 "_non" pDim)
            (setq i (1+ i))
          )
        )
      )

      ;; Total / Overall Dimension along Y
      (if (= addTotalDims 1)
        (progn
          (setq p1   (_toWorld 0.0 0.0)
                p2   (_toWorld 0.0 yTot)
                pDim (_toWorld xTotDim (* 0.5 yTot)))
          (command "._dimaligned" "_non" p1 "_non" p2 "_non" pDim)
        )
      )
    )
  )

  (CadSetup:UndoEnd)

  (setvar "CLAYER" oldLayer)
  (setvar "CMDECHO" oldEcho)
  (princ (strcat "\n[GL] Grid System placed successfully (" 
                 (itoa (length cumX)) "x" (itoa (length cumY)) " lines). Restored layer: " oldLayer))
  (princ)
)

;; Dialog Controller: c:GRID-GENERATOR-DIALOG
(defun c:GRID-GENERATOR-DIALOG ( / *error* dclPath dclId act
                                   strX strXTag strY strYTag
                                   popBubble bubbleRad gridExt textHt
                                   bayDims totalDims dimOff
                                   xList yList )

  (defun *error* (msg)
    (if (and dclId (>= dclId 0))
      (vl-catch-all-apply 'unload_dialog (list dclId))
    )
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*exit*,*quit*")))
      (princ (strcat "\n[GL Error]: " msg))
    )
    (princ)
  )

  ;; Robust DCL search
  (setq dclPath nil)
  (cond
    ((and (boundp 'CadSetup:GetDir) CadSetup:GetDir (setq dclPath (strcat (CadSetup:GetDir) "\\UI\\GRID-GENERATOR.dcl")) (findfile dclPath))
     dclPath)
    ((setq dclPath (findfile "GRID-GENERATOR.dcl"))
     dclPath)
    ((setq dclPath (findfile "UI\\GRID-GENERATOR.dcl"))
     dclPath)
    ((setq dclPath (findfile "d:\\Cad-Setup\\Cad-Setup-v4\\UI\\GRID-GENERATOR.dcl"))
     dclPath)
  )

  (if (or (null dclPath) (not (findfile dclPath)))
    (progn
      (princ "\n[GL Error]: GRID-GENERATOR.dcl not found in UI/ directory.")
      (exit)
    )
  )

  (setq dclId (load_dialog dclPath))
  (if (or (null dclId) (< dclId 0))
    (progn
      (princ (strcat "\n[GL Error]: Failed to load dialog file " dclPath))
      (exit)
    )
  )

  (if (not (new_dialog "grid_system_dialog" dclId))
    (progn
      (unload_dialog dclId)
      (princ "\n[GL Error]: Dialog definition 'grid_system_dialog' not found in DCL.")
      (exit)
    )
  )

  ;; Initialize Pop-up list for bubble positions
  (start_list "pop_bubble_pos")
  (mapcar 'add_list '("0. Both Ends (Top/Bottom & Left/Right)"
                      "1. Top & Left Only"
                      "2. Bottom & Right Only"
                      "3. None (Lines Only)"))
  (end_list)

  ;; Fixed Sensible Defaults
  (set_tile "eb_x_spacings" "4*6000")
  (set_tile "eb_x_tag"      "1")
  (set_tile "eb_y_spacings" "3*5000")
  (set_tile "eb_y_tag"      "A")
  (set_tile "pop_bubble_pos"
    (cond
      ((= *GL-BUBBLE-POS* "Both")  "0")
      ((= *GL-BUBBLE-POS* "Start") "1")
      ((= *GL-BUBBLE-POS* "End")   "2")
      ((= *GL-BUBBLE-POS* "None")  "3")
      (t "0")
    )
  )
  (set_tile "eb_bubble_rad"  (rtos *GL-BUBBLE-RAD* 2 1))
  (set_tile "eb_grid_ext"    (rtos *GL-GRID-EXT* 2 1))
  (set_tile "eb_text_height" (rtos *GL-TEXT-HT* 2 1))
  (set_tile "tog_bay_dims"   "1")
  (set_tile "tog_total_dims" "1")
  (set_tile "eb_dim_offset"  "1200")

  ;; Button Handlers
  (action_tile "btn_single" "(done_dialog 2)")
  (action_tile "cancel"     "(done_dialog 0)")
  (action_tile "accept"
    "(progn
       (setq strX       (get_tile \"eb_x_spacings\")
             strXTag    (get_tile \"eb_x_tag\")
             strY       (get_tile \"eb_y_spacings\")
             strYTag    (get_tile \"eb_y_tag\")
             popBubble  (atoi (get_tile \"pop_bubble_pos\"))
             bubbleRad  (distof (get_tile \"eb_bubble_rad\"))
             gridExt    (distof (get_tile \"eb_grid_ext\"))
             textHt     (distof (get_tile \"eb_text_height\"))
             bayDims    (atoi (get_tile \"tog_bay_dims\"))
             totalDims  (atoi (get_tile \"tog_total_dims\"))
             dimOff     (distof (get_tile \"eb_dim_offset\")))
       (done_dialog 1)
     )"
  )

  (setq act (start_dialog))
  (unload_dialog dclId)

  ;; Dispatch user action
  (cond
    ((= act 1)
     ;; Validate and process inputs
     (setq xList (CadSetup:ParseGridSpacings strX))
     (setq yList (CadSetup:ParseGridSpacings strY))

     (if (or (null xList) (null yList))
       (princ "\n[GL Error]: Invalid bay spacing syntax. Example: 4*6000 or 6000,5000,7500")
       (progn
         (if (and bubbleRad (> bubbleRad 0.0)) (setq *GL-BUBBLE-RAD* bubbleRad))
         (if (and gridExt   (> gridExt 0.0))   (setq *GL-GRID-EXT* gridExt))
         (if (and textHt    (> textHt 0.0))    (setq *GL-TEXT-HT* textHt))
         (setq *GL-BUBBLE-POS*
           (cond
             ((= popBubble 0) "Both")
             ((= popBubble 1) "Start")
             ((= popBubble 2) "End")
             ((= popBubble 3) "None")
             (t "Both")
           )
         )

         (CadSetup:DrawGridSystem xList strXTag yList strYTag
                                  popBubble *GL-BUBBLE-RAD* *GL-GRID-EXT* *GL-TEXT-HT*
                                  bayDims totalDims dimOff)
       )
     )
    )
    ((= act 2)
     ;; Jump directly into Single Line Mode
     (CadSetup:DrawSingleGridLine)
    )
    (t
     (princ "\n[GL] Cancelled.")
    )
  )
  (princ)
)

;; Main Command Entry Point: GL / 3
(defun c:GL ( / opt )
  (initget "Dialog Single Line GL 3")
  (setq opt (getkword "\nGrid System [Dialog/Single Line] <Dialog>: "))
  (cond
    ((or (null opt) (= opt "Dialog") (= opt "D"))
     (c:GRID-GENERATOR-DIALOG)
    )
    ((or (= opt "Single") (= opt "Line") (= opt "GL") (= opt "3"))
     (CadSetup:DrawSingleGridLine)
    )
    (t
     (c:GRID-GENERATOR-DIALOG)
    )
  )
  (princ)
)

(defun c:3 () (c:GL))

;; Direct command shortcuts for Single Grid Line placement
(defun c:GLS () (CadSetup:DrawSingleGridLine) (princ))
(defun c:GL1 () (CadSetup:DrawSingleGridLine) (princ))


;;; --------------------------------------------------------------------------
;;; 4. MATERIAL LAYER SELECTOR & CONTINUOUS RECTANGLE DRAWING (4 / ML)
;;; --------------------------------------------------------------------------

;; Snapshot master layers data on first load for 'Reset to DB Defaults' feature
(if (and (boundp '*CadSetup-Layers-Data*) (null (boundp '*CadSetup-Master-Layers-Backup*)))
  (setq *CadSetup-Master-Layers-Backup* *CadSetup-Layers-Data*)
)

;; CadSetup:GetDrawingMaterialLayers - Returns a sorted list of all active drawing layers matching "R-MAT-*"
(defun CadSetup:GetDrawingMaterialLayers ( / layEntry layList )
  (setq layList nil)
  (setq layEntry (tblnext "LAYER" t))
  (while layEntry
    (if (wcmatch (strcase (cdr (assoc 2 layEntry))) "R-MAT-*")
      (setq layList (cons (cdr (assoc 2 layEntry)) layList))
    )
    (setq layEntry (tblnext "LAYER" nil))
  )
  (if layList
    (acad_strlsort layList)
    nil
  )
)

;; CadSetup:EnsureMaterialLayersLoaded - Creates standard material layers from Db_Layers.lsp if missing
(defun CadSetup:EnsureMaterialLayersLoaded ( / allData count row lName lCol lPlotCol lType lWt lPlot lHatch lHScale lHRot lTrans lLocked lDesc )
  (setq allData (if (boundp '*CadSetup-Layers-Data*) *CadSetup-Layers-Data* nil)
        count 0)
  (if allData
    (foreach row allData
      (setq lName (if (nth 0 row) (vl-princ-to-string (nth 0 row)) ""))
      (if (wcmatch (strcase lName) "R-MAT-*")
        (progn
          (setq lCol     (if (numberp (nth 1 row)) (nth 1 row) 7)
                lPlotCol (if (nth 2 row) (vl-princ-to-string (nth 2 row)) "")
                lType    (if (nth 3 row) (vl-princ-to-string (nth 3 row)) "CONTINUOUS")
                lWt      (if (numberp (nth 4 row)) (nth 4 row) 25)
                lPlot    (nth 5 row)
                lHatch   (if (nth 6 row) (vl-princ-to-string (nth 6 row)) "NONE")
                lHScale  (if (numberp (nth 7 row)) (nth 7 row) 1.0)
                lHRot    (if (numberp (nth 8 row)) (nth 8 row) 0.0)
                lTrans   (if (numberp (nth 9 row)) (nth 9 row) 0)
                lLocked  (nth 10 row)
                lDesc    (if (nth 11 row) (vl-princ-to-string (nth 11 row)) ""))
          (if (CadSetup:EnsureLayer lName lCol lPlotCol lType lWt lPlot 
                                    lHatch lHScale lHRot lTrans lLocked lDesc)
            (setq count (1+ count))
          )
        )
      )
    )
  )
  count
)

;; CadSetup:GetLoadedLinetypes - Returns sorted list of all linetypes currently loaded in the drawing
(defun CadSetup:GetLoadedLinetypes ( / ltEntry ltList )
  (setq ltList nil)
  (setq ltEntry (tblnext "LTYPE" t))
  (while ltEntry
    (setq ltList (cons (cdr (assoc 2 ltEntry)) ltList))
    (setq ltEntry (tblnext "LTYPE" nil))
  )
  (if ltList (acad_strlsort ltList) '("Continuous"))
)

;; CadSetup:WrapText3Lines - Breaks a string into up to 3 lines at word boundaries (~50 chars)
(defun CadSetup:WrapText3Lines (str maxLen / words curLine lineList pos w)
  (if (or (null str) (= str ""))
    '("-" " " " ")
    (progn
      (setq words nil)
      (while (and str (/= str ""))
        (setq pos (vl-string-search " " str))
        (if pos
          (progn
            (setq w (substr str 1 pos))
            (if (> (strlen w) 0) (setq words (cons w words)))
            (setq str (substr str (+ pos 2)))
          )
          (progn
            (if (> (strlen str) 0) (setq words (cons str words)))
            (setq str nil)
          )
        )
      )
      (setq words (reverse words))
      (setq lineList nil
            curLine  "")
      (foreach w words
        (if (= curLine "")
          (setq curLine w)
          (if (<= (+ (strlen curLine) 1 (strlen w)) maxLen)
            (setq curLine (strcat curLine " " w))
            (progn
              (setq lineList (cons curLine lineList))
              (setq curLine w)
            )
          )
        )
      )
      (if (/= curLine "") (setq lineList (cons curLine lineList)))
      (setq lineList (reverse lineList))
      (list
        (if (>= (length lineList) 1) (nth 0 lineList) "-")
        (if (>= (length lineList) 2) (nth 1 lineList) " ")
        (if (>= (length lineList) 3) (nth 2 lineList) " ")
      )
    )
  )
)

;; CadSetup:UpdateLayerDataInMemory - Modifies hatch/transparency specs in *CadSetup-Layers-Data*
(defun CadSetup:UpdateLayerDataInMemory (lName newPat newScl newRot newTrans / oldRow newRow)
  (if (boundp '*CadSetup-Layers-Data*)
    (progn
      (setq oldRow (assoc (strcase lName) *CadSetup-Layers-Data*))
      (if oldRow
        (progn
          (setq newRow (list
            (nth 0 oldRow)
            (nth 1 oldRow)
            (nth 2 oldRow)
            (nth 3 oldRow)
            (nth 4 oldRow)
            (nth 5 oldRow)
            (if newPat newPat (nth 6 oldRow))
            (if (numberp newScl) newScl (nth 7 oldRow))
            (if (numberp newRot) newRot (nth 8 oldRow))
            (if (numberp newTrans) newTrans (nth 9 oldRow))
            (nth 10 oldRow)
            (nth 11 oldRow)
          ))
          (setq *CadSetup-Layers-Data*
            (mapcar
              (function (lambda (r) (if (= (strcase (car r)) (strcase lName)) newRow r)))
              *CadSetup-Layers-Data*
            )
          )
        )
      )
    )
  )
)

;; CadSetup:DrawContinuousRectangles - Continuously draws rectangles on specified layer until Enter/Esc
(defun CadSetup:DrawContinuousRectangles (layName / *error* oldEcho pt1)
  (defun *error* (msg)
    (if oldEcho (setvar "CMDECHO" oldEcho))
    (if (and msg (not (wcmatch (strcase msg t) "*break*,*cancel*,*exit*")))
      (princ (strcat "\n[ML] Error: " msg))
    )
    (princ)
  )

  (setq oldEcho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (CadSetup:SetCurrentLayerSafe layName)
  (setq *WF-LAYER-ML* layName)
  (princ (strcat "\n[ML] Current layer set to: " layName))
  (princ "\n[ML] Continuous Rectangle Mode: Pick two corners per rectangle (Press Enter or Esc when finished)...")

  (while (setq pt1 (getpoint "\nSpecify first corner point or [Enter to finish]: "))
    (setvar "CMDECHO" 1)
    (command "._rectang" pt1 pause)
    (setvar "CMDECHO" 0)
  )

  (setvar "CMDECHO" oldEcho)
  (princ (strcat "\n[ML] Finished drawing rectangles. Active layer remains: " layName))
  (princ)
)

;; CadSetup:MaterialSelectorDialog - Modal DCL controller for selecting and editing material layer properties
(defun CadSetup:MaterialSelectorDialog (matLayers / *error* dclPath dclId act
                                                     acadApp acadDoc layersColl
                                                     selIdx selLayer layObj curCol
                                                     curTrans curHPat curHScl curHRot
                                                     pendingTransMap ltList lwLabels lwValues
                                                     updateDetails newCol selLtypeIdx selLwIdx
                                                     rawVal rawScl defRow defCol defLt defLw
                                                     defPlot defPat defScl defRot defTrans
                                                     oldEcho item)
  (defun *error* (msg)
    (if (and dclId (>= dclId 0))
      (vl-catch-all-apply 'unload_dialog (list dclId))
    )
    (if (and msg (not (wcmatch (strcase msg t) "*break*,*cancel*,*exit*")))
      (princ (strcat "\n[ML Dialog Error]: " msg))
    )
    (princ)
  )

  (setq acadApp         (vlax-get-acad-object)
        acadDoc         (vla-get-activedocument acadApp)
        layersColl      (vla-get-layers acadDoc)
        pendingTransMap nil)

  ;; Lineweight definition mappings
  (setq lwLabels '("Default (-3)" "0.00 mm (0)" "0.05 mm (5)" "0.09 mm (9)" 
                   "0.13 mm (13)" "0.15 mm (15)" "0.18 mm (18)" "0.20 mm (20)" 
                   "0.25 mm (25)" "0.30 mm (30)" "0.35 mm (35)" "0.40 mm (40)" 
                   "0.50 mm (50)" "0.60 mm (60)" "0.70 mm (70)" "0.80 mm (80)" 
                   "0.90 mm (90)" "1.00 mm (100)" "1.20 mm (120)"))
  (setq lwValues '(-3 0 5 9 13 15 18 20 25 30 35 40 50 60 70 80 90 100 120))
  (setq ltList (CadSetup:GetLoadedLinetypes))

  ;; Locate MATERIAL-SELECTOR.dcl
  (setq dclPath nil)
  (cond
    ((and (boundp 'CadSetup:GetDir) CadSetup:GetDir 
          (setq dclPath (strcat (CadSetup:GetDir) "\\UI\\MATERIAL-SELECTOR.dcl")) 
          (findfile dclPath))
     dclPath)
    ((setq dclPath (findfile "MATERIAL-SELECTOR.dcl"))
     dclPath)
    ((setq dclPath (findfile "UI\\MATERIAL-SELECTOR.dcl"))
     dclPath)
    ((setq dclPath (findfile "d:\\Cad-Setup\\Cad-Setup-v4\\UI\\MATERIAL-SELECTOR.dcl"))
     dclPath)
  )

  (if (or (null dclPath) (not (findfile dclPath)))
    (progn
      (princ "\n[ML Error]: MATERIAL-SELECTOR.dcl not found in UI/ directory.")
      (exit)
    )
  )

  (setq dclId (load_dialog dclPath))
  (if (or (null dclId) (< dclId 0))
    (progn
      (princ (strcat "\n[ML Error]: Failed to load dialog file " dclPath))
      (exit)
    )
  )

  (if (not (new_dialog "material_selector_dialog" dclId))
    (progn
      (unload_dialog dclId)
      (princ "\n[ML Error]: Dialog definition 'material_selector_dialog' not found in DCL.")
      (exit)
    )
  )

  ;; Populate materials listbox
  (start_list "lst_materials")
  (mapcar 'add_list matLayers)
  (end_list)

  ;; Populate Linetypes dropdown
  (start_list "pop_mat_ltype")
  (mapcar 'add_list ltList)
  (end_list)

  ;; Populate Lineweights dropdown
  (start_list "pop_mat_lweight")
  (mapcar 'add_list lwLabels)
  (end_list)

  ;; Selection state
  (setq selIdx 0
        selLayer (nth 0 matLayers))
  (set_tile "lst_materials" "0")

  ;; Details Updater Subroutine
  (defun updateDetails (layName / obj cVal ltp lwt plt dbRow hPat hScl hRot hTrn desc swW swH posLtp posLwt descLines pendT)
    (setq obj (vl-catch-all-apply 'vla-item (list layersColl layName)))
    (if (and (not (vl-catch-all-error-p obj)) (= (type obj) 'VLA-OBJECT))
      (progn
        (setq layObj obj)
        (setq cVal (abs (vla-get-color layObj)))
        (setq curCol cVal)
        (setq ltp (vla-get-linetype layObj))
        (setq lwt (vla-get-lineweight layObj))
        (setq plt (= (vla-get-plottable layObj) :vlax-true))

        ;; Render Color Swatch
        (setq swW (dimx_tile "img_color_swatch")
              swH (dimy_tile "img_color_swatch"))
        (start_image "img_color_swatch")
        (fill_image 0 0 swW swH cVal)
        (end_image)

        ;; Update textual tiles
        (set_tile "txt_mat_name" (strcat "Material: " layName))
        (set_tile "txt_mat_color" (strcat "Color: " (itoa cVal) " (ACI)"))

        ;; Match and set Linetype popup
        (setq posLtp (vl-position (strcase ltp) (mapcar 'strcase ltList)))
        (if posLtp (set_tile "pop_mat_ltype" (itoa posLtp)))

        ;; Match and set Lineweight popup
        (setq posLwt (vl-position lwt lwValues))
        (if (null posLwt) (setq posLwt 0))
        (set_tile "pop_mat_lweight" (itoa posLwt))

        ;; Set Plottable toggle
        (set_tile "tog_mat_plot" (if plt "1" "0"))

        ;; Query Db_Layers database for rich architectural specs
        (setq dbRow (if (boundp 'CadSetup:GetLayerData) (CadSetup:GetLayerData layName) nil))
        (if (null dbRow)
          (setq dbRow (assoc (strcase layName) (if (boundp '*CadSetup-Layers-Data*) *CadSetup-Layers-Data* nil)))
        )
        (if dbRow
          (progn
            (setq hPat (if (nth 6 dbRow) (vl-princ-to-string (nth 6 dbRow)) "NONE")
                  hScl (if (numberp (nth 7 dbRow)) (nth 7 dbRow) 1.0)
                  hRot (if (numberp (nth 8 dbRow)) (nth 8 dbRow) 0.0)
                  hTrn (if (numberp (nth 9 dbRow)) (nth 9 dbRow) 0)
                  desc (if (nth 11 dbRow) (vl-princ-to-string (nth 11 dbRow)) "-"))
          )
          (progn
            (setq hPat "NONE"
                  hScl 1.0
                  hRot 0.0
                  hTrn 0
                  desc "Custom Material Layer")
          )
        )

        ;; Check if transparency was edited in pending map
        (setq pendT (assoc layName pendingTransMap))
        (if pendT (setq hTrn (cdr pendT)))
        (setq curTrans hTrn
              curHPat  hPat
              curHScl  hScl
              curHRot  hRot)

        ;; Populate editable Hatch and Transparency tiles
        (set_tile "eb_mat_trans" (itoa curTrans))
        (set_tile "eb_hatch_pat" curHPat)
        (set_tile "eb_hatch_scl" (rtos curHScl 2 2))
        (set_tile "eb_hatch_rot" (rtos curHRot 2 1))

        ;; 3-Line Word-Wrapped Description (No truncation)
        (setq descLines (CadSetup:WrapText3Lines desc 50))
        (set_tile "txt_mat_desc1" (nth 0 descLines))
        (set_tile "txt_mat_desc2" (nth 1 descLines))
        (set_tile "txt_mat_desc3" (nth 2 descLines))
      )
    )
  )

  ;; Initial display
  (updateDetails selLayer)

  ;; Callbacks
  (action_tile "lst_materials"
    "(setq selIdx (atoi $value)
           selLayer (nth selIdx matLayers))
     (updateDetails selLayer)
     (if (= $reason 4) (done_dialog 1))"
  )

  (action_tile "btn_edit_color"
    "(setq newCol (acad_colordlg curCol nil))
     (if (and newCol layObj)
       (progn
         (vla-put-color layObj newCol)
         (updateDetails selLayer)
       )
     )"
  )

  (action_tile "pop_mat_ltype"
    "(setq selLtypeIdx (atoi $value))
     (if (and layObj selLtypeIdx (< selLtypeIdx (length ltList)))
       (vl-catch-all-apply 'vla-put-linetype (list layObj (nth selLtypeIdx ltList)))
     )"
  )

  (action_tile "pop_mat_lweight"
    "(setq selLwIdx (atoi $value))
     (if (and layObj selLwIdx (< selLwIdx (length lwValues)))
       (vl-catch-all-apply 'vla-put-lineweight (list layObj (nth selLwIdx lwValues)))
     )"
  )

  (action_tile "tog_mat_plot"
    "(if layObj
       (vla-put-plottable layObj (if (= $value \"1\") :vlax-true :vlax-false))
     )"
  )

  (action_tile "eb_mat_trans"
    "(setq rawVal (atoi $value))
     (if (< rawVal 0) (setq rawVal 0))
     (if (> rawVal 90) (setq rawVal 90))
     (set_tile \"eb_mat_trans\" (itoa rawVal))
     (setq curTrans rawVal)
     (setq pendingTransMap (cons (cons selLayer curTrans) 
                                 (vl-remove-if (function (lambda (x) (= (car x) selLayer))) pendingTransMap)))
     (CadSetup:UpdateLayerDataInMemory selLayer nil nil nil curTrans)"
  )

  (action_tile "eb_hatch_pat"
    "(setq curHPat (strcase (vl-string-trim \" \" $value)))
     (CadSetup:UpdateLayerDataInMemory selLayer curHPat nil nil nil)"
  )

  (action_tile "eb_hatch_scl"
    "(setq rawScl (atof $value))
     (if (> rawScl 0.0)
       (progn
         (setq curHScl rawScl)
         (CadSetup:UpdateLayerDataInMemory selLayer nil curHScl nil nil)
       )
       (set_tile \"eb_hatch_scl\" (rtos curHScl 2 2))
     )"
  )

  (action_tile "eb_hatch_rot"
    "(setq curHRot (atof $value))
     (CadSetup:UpdateLayerDataInMemory selLayer nil nil curHRot nil)"
  )

  (action_tile "btn_reset_defaults"
    "(if (boundp '*CadSetup-Master-Layers-Backup*)
       (setq defRow (assoc (strcase selLayer) *CadSetup-Master-Layers-Backup*))
       (setq defRow nil)
     )
     (if defRow
       (progn
         (setq defCol   (if (numberp (nth 1 defRow)) (nth 1 defRow) 7)
               defLt    (if (nth 3 defRow) (vl-princ-to-string (nth 3 defRow)) \"CONTINUOUS\")
               defLw    (if (numberp (nth 4 defRow)) (nth 4 defRow) 25)
               defPlot  (nth 5 defRow)
               defPat   (if (nth 6 defRow) (vl-princ-to-string (nth 6 defRow)) \"NONE\")
               defScl   (if (numberp (nth 7 defRow)) (nth 7 defRow) 1.0)
               defRot   (if (numberp (nth 8 defRow)) (nth 8 defRow) 0.0)
               defTrans (if (numberp (nth 9 defRow)) (nth 9 defRow) 0))

         (if layObj
           (progn
             (vla-put-color layObj defCol)
             (vl-catch-all-apply 'vla-put-linetype (list layObj defLt))
             (vl-catch-all-apply 'vla-put-lineweight (list layObj defLw))
             (vla-put-plottable layObj (if defPlot :vlax-true :vlax-false))
           )
         )
         (setq curTrans defTrans)
         (setq pendingTransMap (cons (cons selLayer curTrans) 
                                     (vl-remove-if (function (lambda (x) (= (car x) selLayer))) pendingTransMap)))
         (CadSetup:UpdateLayerDataInMemory selLayer defPat defScl defRot defTrans)
         (updateDetails selLayer)
         (princ (strcat \"\\n[ML] Reset \" selLayer \" to standard database defaults.\"))
       )
       (princ (strcat \"\\n[ML] No default definition found for \" selLayer))
     )"
  )

  (action_tile "btn_draw" "(done_dialog 1)")
  (action_tile "btn_current" "(done_dialog 2)")
  (action_tile "cancel" "(done_dialog 0)")

  (setq act (start_dialog))
  (unload_dialog dclId)

  ;; Apply any modified layer transparencies natively outside DCL
  (if pendingTransMap
    (progn
      (setq oldEcho (getvar "CMDECHO"))
      (setvar "CMDECHO" 0)
      (foreach item pendingTransMap
        (vl-catch-all-apply
          (function (lambda ()
            (command "._-LAYER" "_TR" (itoa (cdr item)) (car item) "")
          ))
        )
      )
      (setvar "CMDECHO" oldEcho)
    )
  )

  ;; Return action result: (list actionType layerName)
  (cond
    ((= act 1) (list :draw selLayer))
    ((= act 2) (list :current selLayer))
    (t nil)
  )
)

;; c:ML - Material Line / Rectangle Workflow Entry Point
(defun c:ML ( / matLayers ans promptStr kwStr kwMap suffix kw opt res chosenLayer )
  ;; 1. Check for existing R-MAT-* layers in active drawing
  (setq matLayers (CadSetup:GetDrawingMaterialLayers))

  ;; 2. If none exist, offer to load standard materials from Db_Layers
  (if (null matLayers)
    (progn
      (initget "Yes No")
      (setq ans (getkword "\n[ML] No 'R-MAT-*' material layers found. Load standard materials from database? [Yes/No] <Yes>: "))
      (if (or (null ans) (= ans "Yes") (= ans "Y"))
        (progn
          (princ "\n[ML] Loading standard material layers from database...")
          (CadSetup:EnsureMaterialLayersLoaded)
          (setq matLayers (CadSetup:GetDrawingMaterialLayers))
        )
      )
    )
  )

  (if (null matLayers)
    (progn
      (princ "\n[ML] Cancelled. No material layers available.")
      (princ)
    )
    (progn
      ;; 3. Build Command-Line Keywords and Prompt
      ;; Format: [Dialog/SUFFIX1/SUFFIX2/...] <Dialog>:
      (setq kwMap '(("DIALOG" . "DIALOG") ("D" . "DIALOG")))
      (setq promptStr "\nSelect Material [Dialog")
      (setq kwStr "Dialog D")

      (foreach lay matLayers
        ;; Extract suffix after "R-MAT-"
        (setq suffix (if (> (strlen lay) 6) (substr lay 7) lay))
        (setq kw (strcase (vl-string-translate " " "_" suffix)))
        ;; Store in keyword-to-layer lookup map
        (setq kwMap (cons (cons kw lay) kwMap))
        (setq promptStr (strcat promptStr "/" suffix))
        (setq kwStr (strcat kwStr " " kw))
      )
      (setq promptStr (strcat promptStr "] <Dialog>: "))

      ;; 4. Prompt User
      (initget kwStr)
      (setq opt (getkword promptStr))

      ;; Default is Dialog if Enter pressed
      (if (or (null opt) (= (strcase opt) "DIALOG") (= (strcase opt) "D"))
        (progn
          (setq res (CadSetup:MaterialSelectorDialog matLayers))
          (if res
            (cond
              ((= (car res) :draw)
               (CadSetup:DrawContinuousRectangles (cadr res))
              )
              ((= (car res) :current)
               (CadSetup:SetCurrentLayerSafe (cadr res))
               (setq *WF-LAYER-ML* (cadr res))
               (princ (strcat "\n[ML] Current layer set to: " (cadr res)))
              )
            )
            (princ "\n[ML] Cancelled.")
          )
        )
        ;; Direct Keyword Selected from Command Line
        (progn
          (setq chosenLayer (cdr (assoc (strcase (vl-string-translate " " "_" opt)) kwMap)))
          (if chosenLayer
            (CadSetup:DrawContinuousRectangles chosenLayer)
            (princ (strcat "\n[ML] Unrecognized material: " opt))
          )
        )
      )
    )
  )
  (princ)
)

(defun c:4 () (c:ML))


(if *CadSetup-Debug*
  (princ "\n[01_Workflow-Keys.lsp] Workflow keys (1=HL, 2=VP, 3=GL, 4=ML) loaded.")
)
(princ)
