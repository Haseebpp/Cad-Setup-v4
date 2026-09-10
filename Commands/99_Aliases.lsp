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
(defun c:P   () (command "_.PLINE")     (princ)) ; P   -> Draw 2D Polyline
(defun c:L   () (command "_.LINE")      (princ)) ; L   -> Draw Line segments
(defun c:R   () (command "_.RECTANG")   (princ)) ; R   -> Draw Rectangle
(defun c:CI  () (command "_.CIRCLE")    (princ)) ; CI  -> Draw Circle
(defun c:A   () (command "_.ARC")       (princ)) ; A   -> Draw 3-Point Arc
(defun c:H   () (command "_.HATCH")     (princ)) ; H   -> Open Hatch / Fill boundaries
(defun c:W   () (command "_.WIPEOUT")   (princ)) ; W   -> Create polygonal mask/wipeout
(defun c:BO  () (command "_.BOUNDARY")  (princ)) ; BO  -> Create region/polyline boundary

;;; --- MEASURE & INQUIRE ----------------------------------------------------
(defun c:D   () (command "_.DIST")      (princ)) ; D   -> Measure linear distance & angle
(defun c:T   () (command "_.MEASUREGEOM" "_QUICK") (princ)) ; T   -> Dynamic real-time quick measure
(defun c:AA  () (command "_.AREA")      (princ)) ; AA  -> Calculate area and perimeter

;;; --- TRANSFORM & ARRANGE --------------------------------------------------
(defun c:V   () (command "_.MOVE")       (princ)) ; V   -> Move objects
(defun c:MP  () (command "_.MOVE" "_P" "")(princ)); VP  -> Move previously selected set
(defun c:C   () (command "_.COPY")       (princ)) ; C   -> Copy objects
(defun c:CP  () (command "_.COPY" "_P" "")(princ)); CP  -> Copy previously selected set
(defun c:RR  () (command "_.ROTATE")     (princ)) ; RR  -> Rotate objects around a basepoint
(defun c:SC  () (command "_.SCALE")      (princ)) ; SC  -> Scale objects uniformly
(defun c:MI  () (command "_.MIRROR")     (princ)) ; MI  -> Mirror objects (retain source)
(defun c:AL  () (command "_.ALIGN")      (princ)) ; AL  -> Align 2D/3D objects with source/target points
(defun c:AR  () (command "_.ARRAY")      (princ)) ; AR  -> Rectangular, Path, or Polar Array
(defun c:VO  () (command "_.PASTEORIG")  (princ)) ; VO  -> Paste clipboard to original coordinates
(defun c:VB  () (command "_.PASTEBLOCK") (princ)) ; VB  -> Paste clipboard as an anonymous block

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
(defun c:S   () (command "_.STRETCH")   (princ)) ; S   -> Stretch crossing-window selection
(defun c:O   () (command "_.OFFSET")    (princ)) ; O   -> Offset curves by parallel distance
(defun c:TR  () (command "_.TRIM")      (princ)) ; TR  -> Trim geometry to cutting edges
(defun c:EX  () (command "_.EXTEND")    (princ)) ; EX  -> Extend geometry to boundary edges
(defun c:F   () (command "_.FILLET")    (princ)) ; F   -> Round/fillet corners
(defun c:CH  () (command "_.CHAMFER")   (princ)) ; CH  -> Bevel/chamfer edges
(defun c:J   () (command "_.JOIN")      (princ)) ; J   -> Join collinear/coincident entities
(defun c:X   () (command "_.EXPLODE")   (princ)) ; X   -> Break compound object into primitives
(defun c:E   () (command "_.ERASE")     (princ)) ; E   -> Delete selected objects
(defun c:OK  () (command "_.OVERKILL")  (princ)) ; OK  -> Clean duplicate/overlapping lines
(defun c:BR  () (command "_.BREAK")     (princ)) ; BR  -> Break entity between 2 pick points

;; Modern & Legacy Safe Break-At-Point
(defun c:BR1 ()                                  ; BR1 -> Split curve at a single pick point
  (if (vl-cmdf "_.BREAKATPOINT") 
    (command "_.BREAKATPOINT") 
    (progn (initcommandversion) (command "_.BREAKATPOINT"))) 
  (princ))

;; Instant Zero-Radius / Zero-Distance Corners
(defun c:FF  () (setvar "FILLETRAD" 0.0) (command "_.FILLET")  (princ)) ; FF  -> Fillet with Radius 0 (Clean sharp join)
(defun c:CC  () (setvar "CHAMFERA" 0.0) (setvar "CHAMFERB" 0.0) (command "_.CHAMFER") (princ)) ; CC  -> Chamfer with 0x0 Distances
(setq c:F0 c:FF  c:CH0 c:CC)

;; Robust Polyline Processing
(defun c:JJ  (/ ss) (if (setq ss (ssget '((0 . "LINE,ARC,*POLYLINE")))) (command "_.pedit" "_M" ss "" "_Y" "_J" 0.0 "")) (princ)) ; JJ  -> Batch convert & join lines/arcs to 2D Polyline
(defun c:PC  (/ ss) (if (setq ss (ssget '((0 . "*POLYLINE"))))          (command "_.pedit" "_M" ss "" "_C" ""))          (princ)) ; PC  -> Batch force Close polyline contours
(defun c:PO  (/ ss) (if (setq ss (ssget '((0 . "*POLYLINE"))))          (command "_.pedit" "_M" ss "" "_O" ""))          (princ)) ; PO  -> Batch force Open polyline contours

;;; --- SELECTION & ISOLATION ------------------------------------------------
(defun c:SS  () (command "_.SELECTSIMILAR")    (princ)) ; SS  -> Select all matching objects by type/layer
(defun c:QSL () (command "_.QSELECT")          (princ)) ; QSL -> Quick Select filter dialog (renamed from QS to prevent overwrite)
(defun c:IS  () (command "_.ISOLATEOBJECTS")   (princ)) ; IS  -> Isolate selected entities (hide all others)
(defun c:HO  () (command "_.HIDEOBJECTS")      (princ)) ; HO  -> Hide selected entities
(defun c:UN  () (command "_.UNISOLATEOBJECTS") (princ)) ; UN  -> Restore all hidden/isolated entities

;;; --- LAYER TOOLS ----------------------------------------------------------
(defun c:Q   () (command "_.MATCHPROP")   (princ)) ; Q   -> Painter tool: Match source properties
(defun c:LI  () (command "_.LAYISO")      (princ)) ; LI  -> Isolate layers of selected entities
(defun c:LU  () (command "_.LAYUNISO")    (princ)) ; LU  -> Restore layers hidden by LAYISO
(defun c:LO  () (command "_.LAYOFF")      (princ)) ; LO  -> Turn off layer of picked object
(defun c:LON () (command "_.LAYON")       (princ)) ; LON -> Turn ON all layers in drawing
(defun c:LF  () (command "_.LAYFRZ")      (princ)) ; LF  -> Freeze layer of picked object
(defun c:LTH () (command "_.LAYTHW")      (princ)) ; LTH -> Thaw ALL frozen layers
(defun c:LLK () (command "_.LAYLCK")      (princ)) ; LLK -> Lock layer of picked object
(defun c:LUK () (command "_.LAYULK")      (princ)) ; LUK -> Unlock layer of picked object
(defun c:LM  () (command "_.LAYMCH")      (princ)) ; LM  -> Match layer of selected object to target
(defun c:LC  () (command "_.LAYMCUR")     (princ)) ; LC  -> Set picked object's layer to Current
(defun c:LCC () (command "_.LAYCUR")      (princ)) ; LCC -> Move selected entities to current working layer
(defun c:LW  () (command "_.LAYWALK")     (princ)) ; LW  -> Interactive Layer Walk diagnostic dialog
(defun c:LMR () (command "_.LAYMRG")      (princ)) ; LMR -> Merge all items on layer into target layer
(defun c:LD  () (command "_.LAYDEL")      (princ)) ; LD  -> Force-purge layer and all nested entities

;;; --- BLOCKS & EXTERNAL REFERENCES -----------------------------------------
(defun c:`   () (command "_.REFEDIT")     (princ)) ; `   -> In-place Block / XREF editor
(defun c:RC  () (command "_.REFCLOSE")    (princ)) ; RC  -> Save & Close in-place block reference
(defun c:B   () (command "_.BLOCK")       (princ)) ; B   -> Open Block Definition creation dialog
(defun c:I   () (command "_.INSERT")      (princ)) ; I   -> Launch Block Insertion palette/dialog

;;; --- ZOOM & VIEW SHORTCUTS ------------------------------------------------
(defun c:ZE  () (command "_.ZOOM" "_E")   (princ)) ; ZE  -> Zoom Extents (fits all geometry to screen)
(defun c:ZW  () (command "_.ZOOM" "_W")   (princ)) ; ZW  -> Zoom to user-defined rectangle Window
(defun c:ZP  () (command "_.ZOOM" "_P")   (princ)) ; ZP  -> Zoom to Previous viewport state
(defun c:ZS  () (command "_.ZOOM" "_O")   (princ)) ; ZS  -> Zoom to selected Object bounds
(defun c:RE  () (command "_.REGEN")       (princ)) ; RE  -> Regenerate active viewport display cache
(defun c:REA () (command "_.REGENALL")    (princ)) ; REA -> Regenerate all viewports and model tabs

;;; --- DIMENSIONING & ANNOTATION --------------------------------------------
(defun c:DA  () (command "_.DIMALIGNED")  (princ)) ; DA  -> Aligned dimension (follows curve angle)
(defun c:DD  () (command "_.DIMLINEAR")   (princ)) ; DD  -> Orthogonal Linear dimension (Horizontal/Vertical)
(defun c:DR  () (command "_.DIMRADIUS")   (princ)) ; DR  -> Radial dimension for arcs and circles
(defun c:DDI () (command "_.DIMDIAMETER") (princ)) ; DDI -> Diameter dimension for circles
(defun c:DAN () (command "_.DIMANGULAR")  (princ)) ; DAN -> Angular dimension between two vectors
(defun c:DC  () (command "_.DIMCONTINUE") (princ)) ; DC  -> Chain-continue from previous dimension extension line
(defun c:DE  () (command "_.DIMEDIT")     (princ)) ; DE  -> Edit dimension text / orientation
(defun c:TE  () (command "_.TEXTEDIT")    (princ)) ; TE  -> Edit MText, DText, or Dimension string

;;; --- PRODUCTIVITY & FILE UTILITIES ----------------------------------------
(defun c:QS  () (command "_.QSAVE")       (princ)) ; QS  -> Quick Save current drawing without prompt
(defun c:CL  () (command "_.CLOSE")       (princ)) ; CL  -> Close current active drawing window

(princ "\n[99_Aliases.lsp] Consolidated personal aliases loaded successfully.")
(princ)
