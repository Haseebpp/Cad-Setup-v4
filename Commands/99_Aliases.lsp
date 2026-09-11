;;; ==========================================================================
;;; 99_Aliases.lsp - Final Stage Consolidated Drafting Aliases & Overrides
;;; ==========================================================================
;;; Category : Final-Stage Ergonomic Shortcuts & Command Remappings
;;; Author   : Haseeb
;;; ==========================================================================

(vl-load-com)

;;; ==========================================================================
;;; HIGH-FREQUENCY ERGONOMIC OPTIMIZED DRAFTING ALIASES & MACROS
;;; ==========================================================================

;;; --- DRAW & GEOMETRY ------------------------------------------------------
(defun c:P   () (initcommandversion) (command "_.PLINE")     (princ)) ; P   -> Draw 2D Polyline
(defun c:L   () (initcommandversion) (command "_.LINE")      (princ)) ; L   -> Draw Line segments
(defun c:R   () (initcommandversion) (command "_.RECTANG")   (princ)) ; R   -> Draw Rectangle
(defun c:CI  () (initcommandversion) (command "_.CIRCLE")    (princ)) ; CI  -> Draw Circle
(defun c:A   () (initcommandversion) (command "_.ARC")       (princ)) ; A   -> Draw 3-Point Arc
(defun c:H   () (initcommandversion) (command "_.HATCH")     (princ)) ; H   -> Open Hatch / Fill boundaries
(defun c:W   () (initcommandversion) (command "_.WIPEOUT")   (princ)) ; W   -> Create polygonal mask/wipeout
(defun c:BO  () (initcommandversion) (command "_.BOUNDARY")  (princ)) ; BO  -> Create region/polyline boundary

;;; --- MEASURE & INQUIRE ----------------------------------------------------
(defun c:D   () (initcommandversion) (command "_.DIST")      (princ)) ; D   -> Measure linear distance & angle
(defun c:T   () (command "_.MEASUREGEOM" "_QUICK") (princ)) ; T   -> Dynamic real-time quick measure
(defun c:AA  () (initcommandversion) (command "_.AREA")      (princ)) ; AA  -> Calculate area and perimeter

;;; --- TRANSFORM & ARRANGE --------------------------------------------------
(defun c:V   () (initcommandversion) (command "_.MOVE")       (princ)) ; V   -> Move objects
(defun c:MP  () (command "_.MOVE" "_P" "")(princ)); VP  -> Move previously selected set
(defun c:C   () (initcommandversion) (command "_.COPY")       (princ)) ; C   -> Copy objects
(defun c:CP  () (command "_.COPY" "_P" "")(princ)); CP  -> Copy previously selected set
(defun c:RR  () (initcommandversion) (command "_.ROTATE")     (princ)) ; RR  -> Rotate objects around a basepoint
(defun c:SC  () (initcommandversion) (command "_.SCALE")      (princ)) ; SC  -> Scale objects uniformly
(defun c:MI  () (initcommandversion) (command "_.MIRROR")     (princ)) ; MI  -> Mirror objects (retain source)
(defun c:AL  () (initcommandversion) (command "_.ALIGN")      (princ)) ; AL  -> Align 2D/3D objects with source/target points
(defun c:AR  () (initcommandversion) (command "_.ARRAY")      (princ)) ; AR  -> Rectangular, Path, or Polar Array
(defun c:VO  () (initcommandversion) (command "_.PASTEORIG")  (princ)) ; VO  -> Paste clipboard to original coordinates
(defun c:VB  () (initcommandversion) (command "_.PASTEBLOCK") (princ)) ; VB  -> Paste clipboard as an anonymous block

;; Transform Multi-Action Macros (with null-selection guards)
(defun c:ROR (/ ss) (if (setq ss (ssget)) (command "_.ROTATE" ss "" pause "_R")) (princ)) ; ROR -> Rotate by Reference angle
(defun c:SCR (/ ss) (if (setq ss (ssget)) (command "_.SCALE"  ss "" pause "_R")) (princ)) ; SCR -> Scale by Reference length
(defun c:BF  (/ ss) (if (setq ss (ssget)) (command "_.DRAWORDER" ss "" "_F"))    (princ)) ; BF  -> Draw Order: Bring to absolute Front
(defun c:BB  (/ ss) (if (setq ss (ssget)) (command "_.DRAWORDER" ss "" "_B"))    (princ)) ; BB  -> Draw Order: Send to absolute Back
(defun c:ME  (/ ss p1 p2)                                                                 ; ME  -> Mirror and erase source objects
  (if (and (setq ss (ssget)) 
           (setq p1 (getpoint "\nSpecify 1st point of mirror axis: ")) 
           (setq p2 (getpoint p1 "\nSpecify 2nd point of mirror axis: ")))
    (command "_.MIRROR" ss "" p1 p2 "_Y")) (princ))

;;; --- MODIFY & CLEANUP -----------------------------------------------------
(defun c:S   () (initcommandversion) (command "_.STRETCH")   (princ)) ; S   -> Stretch crossing-window selection
(defun c:O   () (initcommandversion) (command "_.OFFSET")    (princ)) ; O   -> Offset curves by parallel distance
(defun c:TR  () (initcommandversion) (command "_.TRIM")      (princ)) ; TR  -> Trim geometry to cutting edges
(defun c:EX  () (initcommandversion) (command "_.EXTEND")    (princ)) ; EX  -> Extend geometry to boundary edges
(defun c:F   () (initcommandversion) (command "_.FILLET")    (princ)) ; F   -> Round/fillet corners
(defun c:CH  () (initcommandversion) (command "_.CHAMFER")   (princ)) ; CH  -> Bevel/chamfer edges
(defun c:J   () (initcommandversion) (command "_.JOIN")      (princ)) ; J   -> Join collinear/coincident entities
(defun c:X   () (initcommandversion) (command "_.EXPLODE")   (princ)) ; X   -> Break compound object into primitives
(defun c:E   () (initcommandversion) (command "_.ERASE")     (princ)) ; E   -> Delete selected objects
(defun c:OK  () (initcommandversion) (command "_.OVERKILL")  (princ)) ; OK  -> Clean duplicate/overlapping lines
(defun c:BR  () (initcommandversion) (command "_.BREAK")     (princ)) ; BR  -> Break entity between 2 pick points

;; Modern & Legacy Safe Break-At-Point
(defun c:BR1 ()                                  ; BR1 -> Split curve at a single pick point
  (if (vl-cmdf "_.BREAKATPOINT") 
    (command "_.BREAKATPOINT") 
    (progn (initcommandversion) (command "_.BREAKATPOINT"))) 
  (princ))

;; Force legacy non-associative array (creates plain individual objects)
(defun c:AR1 ()
  (initcommandversion 1)
  (command "_.ARRAY")
  (princ)
)

;; Force modern interactive ribbon associative array
(defun c:AR2 ()
  (initcommandversion 2)
  (command "_.ARRAY")
  (princ)
)
  
;; Instant Zero-Radius / Zero-Distance Corners
(defun c:FF  () (setvar "FILLETRAD" 0.0) (command "_.FILLET")  (princ)) ; FF  -> Fillet with Radius 0 (Clean sharp join)
(defun c:CC  () (setvar "CHAMFERA" 0.0) (setvar "CHAMFERB" 0.0) (command "_.CHAMFER") (princ)) ; CC  -> Chamfer with 0x0 Distances
(setq c:F0 c:FF  c:CH0 c:CC)

;; Robust Polyline Processing
(defun c:JJ  (/ ss) (if (setq ss (ssget '((0 . "LINE,ARC,*POLYLINE")))) (command "_.pedit" "_M" ss "" "_Y" "_J" 0.0 "")) (princ)) ; JJ  -> Batch convert & join lines/arcs to 2D Polyline
(defun c:PC  (/ ss) (if (setq ss (ssget '((0 . "*POLYLINE"))))          (command "_.pedit" "_M" ss "" "_C" ""))          (princ)) ; PC  -> Batch force Close polyline contours
(defun c:PO  (/ ss) (if (setq ss (ssget '((0 . "*POLYLINE"))))          (command "_.pedit" "_M" ss "" "_O" ""))          (princ)) ; PO  -> Batch force Open polyline contours

;;; --- SELECTION & ISOLATION ------------------------------------------------
(defun c:SS  () (initcommandversion) (command "_.SELECTSIMILAR")    (princ)) ; SS  -> Select all matching objects by type/layer
(defun c:QSL () (initcommandversion) (command "_.QSELECT")          (princ)) ; QSL -> Quick Select filter dialog (renamed from QS to prevent overwrite)
(defun c:IS  () (initcommandversion) (command "_.ISOLATEOBJECTS")   (princ)) ; IS  -> Isolate selected entities (hide all others)
(defun c:HO  () (initcommandversion) (command "_.HIDEOBJECTS")      (princ)) ; HO  -> Hide selected entities
(defun c:UN  () (initcommandversion) (command "_.UNISOLATEOBJECTS") (princ)) ; UN  -> Restore all hidden/isolated entities

;;; --- LAYER TOOLS ----------------------------------------------------------
(defun c:Q   () (initcommandversion) (command "_.MATCHPROP")   (princ)) ; Q   -> Painter tool: Match source properties
(defun c:LI  () (initcommandversion) (command "_.LAYISO")      (princ)) ; LI  -> Isolate layers of selected entities
(defun c:LU  () (initcommandversion) (command "_.LAYUNISO")    (princ)) ; LU  -> Restore layers hidden by LAYISO
(defun c:LO  () (initcommandversion) (command "_.LAYOFF")      (princ)) ; LO  -> Turn off layer of picked object
(defun c:LON () (initcommandversion) (command "_.LAYON")       (princ)) ; LON -> Turn ON all layers in drawing
(defun c:LF  () (initcommandversion) (command "_.LAYFRZ")      (princ)) ; LF  -> Freeze layer of picked object
(defun c:LTH () (initcommandversion) (command "_.LAYTHW")      (princ)) ; LTH -> Thaw ALL frozen layers
(defun c:LLK () (initcommandversion) (command "_.LAYLCK")      (princ)) ; LLK -> Lock layer of picked object
(defun c:LUK () (initcommandversion) (command "_.LAYULK")      (princ)) ; LUK -> Unlock layer of picked object
(defun c:LM  () (initcommandversion) (command "_.LAYMCH")      (princ)) ; LM  -> Match layer of selected object to target
(defun c:LC  () (initcommandversion) (command "_.LAYMCUR")     (princ)) ; LC  -> Set picked object's layer to Current
(defun c:LCC () (initcommandversion) (command "_.LAYCUR")      (princ)) ; LCC -> Move selected entities to current working layer
(defun c:LW  () (initcommandversion) (command "_.LAYWALK")     (princ)) ; LW  -> Interactive Layer Walk diagnostic dialog
(defun c:LMR () (initcommandversion) (command "_.LAYMRG")      (princ)) ; LMR -> Merge all items on layer into target layer
(defun c:LD  () (initcommandversion) (command "_.LAYDEL")      (princ)) ; LD  -> Force-purge layer and all nested entities

;;; --- BLOCKS & EXTERNAL REFERENCES -----------------------------------------
(defun c:`   () (initcommandversion) (command "_.REFEDIT")     (princ)) ; `   -> In-place Block / XREF editor
(defun c:RC  () (initcommandversion) (command "_.REFCLOSE")    (princ)) ; RC  -> Save & Close in-place block reference
(defun c:B   () (initcommandversion) (command "_.BLOCK")       (princ)) ; B   -> Open Block Definition creation dialog
(defun c:I   () (initcommandversion) (command "_.INSERT")      (princ)) ; I   -> Launch Block Insertion palette/dialog

;;; --- ZOOM & VIEW SHORTCUTS ------------------------------------------------
(defun c:ZE  () (command "_.ZOOM" "_E")   (princ)) ; ZE  -> Zoom Extents (fits all geometry to screen)
(defun c:ZW  () (command "_.ZOOM" "_W")   (princ)) ; ZW  -> Zoom to user-defined rectangle Window
(defun c:ZP  () (command "_.ZOOM" "_P")   (princ)) ; ZP  -> Zoom to Previous viewport state
(defun c:ZS  () (command "_.ZOOM" "_O")   (princ)) ; ZS  -> Zoom to selected Object bounds
(defun c:RE  () (initcommandversion) (command "_.REGEN")       (princ)) ; RE  -> Regenerate active viewport display cache
(defun c:REA () (initcommandversion) (command "_.REGENALL")    (princ)) ; REA -> Regenerate all viewports and model tabs

;;; --- DIMENSIONING & ANNOTATION --------------------------------------------
(defun c:DA  () (initcommandversion) (command "_.DIMALIGNED")  (princ)) ; DA  -> Aligned dimension (follows curve angle)
(defun c:DD  () (initcommandversion) (command "_.DIMLINEAR")   (princ)) ; DD  -> Orthogonal Linear dimension (Horizontal/Vertical)
(defun c:DR  () (initcommandversion) (command "_.DIMRADIUS")   (princ)) ; DR  -> Radial dimension for arcs and circles
(defun c:DDI () (initcommandversion) (command "_.DIMDIAMETER") (princ)) ; DDI -> Diameter dimension for circles
(defun c:DAN () (initcommandversion) (command "_.DIMANGULAR")  (princ)) ; DAN -> Angular dimension between two vectors
(defun c:DC  () (initcommandversion) (command "_.DIMCONTINUE") (princ)) ; DC  -> Chain-continue from previous dimension extension line
(defun c:DE  () (initcommandversion) (command "_.DIMEDIT")     (princ)) ; DE  -> Edit dimension text / orientation
(defun c:TE  () (initcommandversion) (command "_.TEXTEDIT")    (princ)) ; TE  -> Edit MText, DText, or Dimension string

;;; --- PRODUCTIVITY & FILE UTILITIES ----------------------------------------
(defun c:QS  () (initcommandversion) (command "_.QSAVE")       (princ)) ; QS  -> Quick Save current drawing without prompt
(defun c:CL  () (initcommandversion) (command "_.CLOSE")       (princ)) ; CL  -> Close current active drawing window

(if *CadSetup-Debug*
  (princ "\n[99_Aliases.lsp] Consolidated personal aliases loaded successfully.")
)
(princ)
