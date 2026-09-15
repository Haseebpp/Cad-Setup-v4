;;; ==========================================================================
;;; 99_Aliases.lsp - Final Stage Consolidated Drafting Aliases & Overrides
;;; ==========================================================================
;;; Category : Final-Stage Ergonomic Shortcuts & Command Remappings
;;; Author   : Haseeb
;;; ==========================================================================
;;; ARCHITECTURAL NOTE & DESIGN PRINCIPLES:
;;; 1. PRECEDENCE OVER ACAD.PGP:
;;;    Commands defined here (c:CMD) take deterministic precedence over acad.pgp
;;;    across all CAD workstations without requiring administrative file edits
;;;    or AutoCAD environment restarts (REINIT).
;;; 2. ERGONOMIC LEFT-HAND CLUSTERING:
;;;    High-frequency modification, query, and transform commands are optimized
;;;    for the left hand (e.g., V=Move, C=Copy, S=Stretch, Q=Matchprop, FF/CC=Zero-corners),
;;;    minimizing travel distance and maximizing drafting velocity.
;;; 3. TARGETED MODERN COMMAND VERSIONING:
;;;    (initcommandversion) is reserved strictly for commands that feature dual-mode
;;;    behaviors in modern AutoCAD (e.g., HATCH, ARRAY, INSERT, BREAKATPOINT) to
;;;    guarantee modern ribbon, contextual tab, and palette interfaces. Standard
;;;    drawing primitives remain lean and fast.
;;; 4. DEFENSIVE GUARDS & ATOMIC UNDO:
;;;    Multi-step macros are protected with null-selection guards (ssget checks)
;;;    and wrapped in re-entrant (CadSetup:UndoStart) / (CadSetup:UndoEnd) transactions
;;;    so that multi-action commands undo cleanly in a single Ctrl+Z keystroke.
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
(defun c:H   () (initcommandversion) (command "_.HATCH") (princ)) ; H   -> Open Hatch Ribbon / Fill boundaries
(defun c:W   () (initcommandversion) (command "_.WIPEOUT")   (princ)) ; W   -> Create polygonal mask/wipeout
(defun c:BO  () (initcommandversion) (command "_.BOUNDARY")  (princ)) ; BO  -> Create region/polyline boundary

;;; --- MEASURE & INQUIRE ----------------------------------------------------
(defun c:D   () (initcommandversion) (command "_.DIST")      (princ)) ; D   -> Measure linear distance & angle
(defun c:T   () (initcommandversion) (command "_.MEASUREGEOM" "_QUICK") (princ)) ; T   -> Dynamic real-time quick measure
(defun c:AA  () (initcommandversion) (command "_.AREA")      (princ)) ; AA  -> Calculate area and perimeter

;;; --- TRANSFORM & ARRANGE --------------------------------------------------
(defun c:V   () (initcommandversion) (command "_.MOVE")       (princ)) ; V   -> Move objects
(defun c:MP  () (initcommandversion) (command "_.MOVE" "_P" "")(princ)); VP  -> Move previously selected set
(defun c:C   () (initcommandversion) (command "_.COPY")       (princ)) ; C   -> Copy objects
(defun c:CP  () (initcommandversion) (command "_.COPY" "_P" "")(princ)); CP  -> Copy previously selected set
(defun c:RR  () (initcommandversion) (command "_.ROTATE")     (princ)) ; RR  -> Rotate objects around a basepoint
(defun c:SC  () (initcommandversion) (command "_.SCALE")      (princ)) ; SC  -> Scale objects uniformly
;; (defun c:MI  () (initcommandversion) (command "_.MIRROR")     (princ)) ; MI  -> Mirror objects (retain source)
(defun c:AL  () (initcommandversion) (command "_.ALIGN")      (princ)) ; AL  -> Align 2D/3D objects with source/target points
(defun c:AR  () (initcommandversion) (command "_.ARRAY") (princ)) ; AR  -> Modern Associative Array Ribbon
(defun c:VO  () (initcommandversion) (command "_.PASTEORIG")  (princ)) ; VO  -> Paste clipboard to original coordinates
(defun c:VB  () (initcommandversion) (command "_.PASTEBLOCK") (princ)) ; VB  -> Paste clipboard as an anonymous block

;; Transform Multi-Action Macros (with null-selection guards and atomic undo)
(defun c:ROR (/ *error* ss) 
  (defun *error* (msg)
    (if (boundp 'CadSetup:UndoReset) (CadSetup:UndoReset))
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[ROR] Error: " msg))
    )
    (princ)
  )
  (if (setq ss (ssget)) 
    (progn 
      (if (boundp 'CadSetup:UndoStart) (CadSetup:UndoStart))
      (command "_.ROTATE" ss "" pause "_R") 
      (while (> (getvar 'cmdactive) 0)
        (command pause)
      )
      (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))
    )
  ) 
  (princ)
) ; ROR -> Rotate by Reference angle

(defun c:SCR (/ *error* ss) 
  (defun *error* (msg)
    (if (boundp 'CadSetup:UndoReset) (CadSetup:UndoReset))
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[SCR] Error: " msg))
    )
    (princ)
  )
  (if (setq ss (ssget)) 
    (progn 
      (if (boundp 'CadSetup:UndoStart) (CadSetup:UndoStart))
      (command "_.SCALE" ss "" pause "_R") 
      (while (> (getvar 'cmdactive) 0)
        (command pause)
      )
      (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))
    )
  ) 
  (princ)
) ; SCR -> Scale by Reference length

(defun c:BF (/ *error* ss) 
  (defun *error* (msg)
    (if (boundp 'CadSetup:UndoReset) (CadSetup:UndoReset))
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[BF] Error: " msg))
    )
    (princ)
  )
  (if (setq ss (ssget)) 
    (progn 
      (if (boundp 'CadSetup:UndoStart) (CadSetup:UndoStart))
      (command "_.DRAWORDER" ss "" "_F") 
      (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))
    )
  ) 
  (princ)
) ; BF  -> Draw Order: Bring to absolute Front

(defun c:BB (/ *error* ss) 
  (defun *error* (msg)
    (if (boundp 'CadSetup:UndoReset) (CadSetup:UndoReset))
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[BB] Error: " msg))
    )
    (princ)
  )
  (if (setq ss (ssget)) 
    (progn 
      (if (boundp 'CadSetup:UndoStart) (CadSetup:UndoStart))
      (command "_.DRAWORDER" ss "" "_B") 
      (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))
    )
  ) 
  (princ)
) ; BB  -> Draw Order: Send to absolute Back

(defun c:ME (/ *error* ss p1 p2)                                                                 ; ME  -> Mirror and erase source objects
  (defun *error* (msg)
    (if (boundp 'CadSetup:UndoReset) (CadSetup:UndoReset))
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[ME] Error: " msg))
    )
    (princ)
  )
  (if (and (setq ss (ssget)) 
           (setq p1 (getpoint "\nSpecify 1st point of mirror axis: ")) 
           (setq p2 (getpoint p1 "\nSpecify 2nd point of mirror axis: ")))
    (progn
      (if (boundp 'CadSetup:UndoStart) (CadSetup:UndoStart))
      (command "_.MIRROR" ss "" p1 p2 "_Y")
      (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))
    )
  ) 
  (princ)
)

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
(defun c:BR1 (/ *error*)                                  ; BR1 -> Split curve at a single pick point
  (defun *error* (msg)
    (if (boundp 'CadSetup:UndoReset) (CadSetup:UndoReset))
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[BR1] Error: " msg))
    )
    (princ)
  )
  (if (boundp 'CadSetup:UndoStart) (CadSetup:UndoStart))
  (if (vl-cmdf "_.BREAKATPOINT") 
    (command "_.BREAKATPOINT") 
    (progn (initcommandversion) (command "_.BREAKATPOINT"))
  )
  (while (> (getvar 'cmdactive) 0)
    (command pause)
  )
  (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))
  (princ)
)

;; Explicit Array Versions
(defun c:AR1 () (initcommandversion 1) (command "_.ARRAY") (princ)) ; AR1 -> Force legacy non-associative array
(defun c:AR2 () (initcommandversion 2) (command "_.ARRAY") (princ)) ; AR2 -> Force modern associative ribbon array

;; Instant Zero-Radius / Zero-Distance Corners (Fast persistent resets)
(defun c:FF ( / *error* )                                                               ; FF  -> Fillet with Radius 0 (Clean sharp join)
  (defun *error* (msg)
    (if (boundp 'CadSetup:UndoReset) (CadSetup:UndoReset))
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[FF] Error: " msg))
    )
    (princ)
  )
  (if (boundp 'CadSetup:UndoStart) (CadSetup:UndoStart))
  (setvar "FILLETRAD" 0.0)
  (command "_.FILLET")
  (while (> (getvar 'cmdactive) 0)
    (command pause)
  )
  (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))
  (princ)
)

(defun c:CC ( / *error* )                                                               ; CC  -> Chamfer with 0x0 Distances
  (defun *error* (msg)
    (if (boundp 'CadSetup:UndoReset) (CadSetup:UndoReset))
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[CC] Error: " msg))
    )
    (princ)
  )
  (if (boundp 'CadSetup:UndoStart) (CadSetup:UndoStart))
  (setvar "CHAMFERA" 0.0)
  (setvar "CHAMFERB" 0.0)
  (command "_.CHAMFER")
  (while (> (getvar 'cmdactive) 0)
    (command pause)
  )
  (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))
  (princ)
)
(setq c:F0 c:FF  c:CH0 c:CC)

;; Robust Polyline Processing (with PEDITACCEPT handling & atomic undo)
(defun c:JJ (/ *error* ss)                                                              ; JJ  -> Batch convert & join lines/arcs to 2D Polyline
  (defun *error* (msg)
    (if (boundp 'CadSetup:UndoReset) (CadSetup:UndoReset))
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[JJ] Error: " msg))
    )
    (princ)
  )
  (if (setq ss (ssget '((0 . "LINE,ARC,*POLYLINE"))))
    (progn
      (if (boundp 'CadSetup:UndoStart) (CadSetup:UndoStart))
      (if (= (getvar "PEDITACCEPT") 1)
        (command "_.pedit" "_M" ss "" "_J" 0.0 "")
        (command "_.pedit" "_M" ss "" "_Y" "_J" 0.0 ""))
      (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))
    )
  )
  (princ)
)

(defun c:PC (/ *error* ss)                                                              ; PC  -> Batch force Close polyline contours
  (defun *error* (msg)
    (if (boundp 'CadSetup:UndoReset) (CadSetup:UndoReset))
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[PC] Error: " msg))
    )
    (princ)
  )
  (if (setq ss (ssget '((0 . "*POLYLINE"))))
    (progn
      (if (boundp 'CadSetup:UndoStart) (CadSetup:UndoStart))
      (command "_.pedit" "_M" ss "" "_C" "")
      (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))
    )
  )
  (princ)
)

(defun c:PO (/ *error* ss)                                                              ; PO  -> Batch force Open polyline contours
  (defun *error* (msg)
    (if (boundp 'CadSetup:UndoReset) (CadSetup:UndoReset))
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[PO] Error: " msg))
    )
    (princ)
  )
  (if (setq ss (ssget '((0 . "*POLYLINE"))))
    (progn
      (if (boundp 'CadSetup:UndoStart) (CadSetup:UndoStart))
      (command "_.pedit" "_M" ss "" "_O" "")
      (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))
    )
  )
  (princ)
)

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
(defun c:RE  () (initcommandversion) (command "_.REFEDIT")     (princ)) ; RE  -> In-place Block / XREF editor
(defun c:RC  () (initcommandversion) (command "_.REFCLOSE")    (princ)) ; RC  -> Save & Close in-place block reference
(defun c:B   () (initcommandversion) (initdia) (command "_.BLOCK") (princ)) ; B   -> Open Block Definition creation dialog
(defun c:I   () (initcommandversion) (command "_.INSERT") (princ)) ; I   -> Launch Modern Block Insertion palette

;;; --- ZOOM & VIEW SHORTCUTS ------------------------------------------------
(defun c:ZE  () (initcommandversion) (command "_.ZOOM" "_E")   (princ)) ; ZE  -> Zoom Extents (fits all geometry to screen)
(defun c:ZW  () (initcommandversion) (command "_.ZOOM" "_W")   (princ)) ; ZW  -> Zoom to user-defined rectangle Window
(defun c:ZP  () (initcommandversion) (command "_.ZOOM" "_P")   (princ)) ; ZP  -> Zoom to Previous viewport state
(defun c:ZS  () (initcommandversion) (command "_.ZOOM" "_O")   (princ)) ; ZS  -> Zoom to selected Object bounds
(defun c:REG  () (initcommandversion) (command "_.REGEN")       (princ)) ; RE  -> Regenerate active viewport display cache
(defun c:REA () (initcommandversion) (command "_.REGENALL")    (princ)) ; REA -> Regenerate all viewports and model tabs
(defun c:VR  () (c:VPCLIPRECTANGLE))                                   ; VR  -> Copy Viewport & Clip with Rectangle (VPCLIPRECTANGLE)

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

;;; --- LAYER & VISIBILITY TOGGLES --------------------------------------------
(defun c:/   () (c:TOGGLE-HELP-LINE)) ; /   -> Toggle 01-HELP-LINE layer ON/OFF

;;; ==========================================================================
;;; FUTURE DRAFTING ALIASES TEMPLATE (USER-EXTENSIBLE)
;;; ==========================================================================
;;; Use the patterns below to add high-value shortcuts as your workflow evolves:
;;;
;;; Pattern 1: Standard Single-Command Shortcut
;;; (defun c:XC () (command "_.XCLIP")      (princ)) ; XC -> Clip XREF or Block boundaries
;;; (defun c:XR () (command "_.XREF")       (princ)) ; XR -> Launch External References Palette
;;; (defun c:PR () (command "_.PROPERTIES") (princ)) ; PR -> Toggle Properties Inspector Palette
;;;
;;; Pattern 2: Dialog-Invoking Shortcut (requires initdia to prevent CLI prompting)
;;; (defun c:PU () (initdia) (command "_.PURGE")  (princ)) ; PU -> Launch Purge dialog box
;;; (defun c:FD () (initdia) (command "_.FIND")   (princ)) ; FD -> Launch Find & Replace Text dialog
;;; (defun c:VP () (initdia) (command "_.VPORTS") (princ)) ; VP -> Launch Viewports configuration dialog
;;;
;;; Pattern 3: Environment/Sysvar Toggles
;;; (defun c:TI () (setvar "TILEMODE" (if (= (getvar "TILEMODE") 1) 0 1)) (princ)) ; TI -> Toggle Model/Paper tab
;;;
;;; Pattern 4: Multi-Action Macro with Atomic Undo
;;; (defun c:MYMACRO (/ ss)
;;;   (if (setq ss (ssget))
;;;     (progn
;;;       (CadSetup:UndoStart)
;;;       ;; ... your commands here ...
;;;       (CadSetup:UndoEnd)))
;;;   (princ))

(if *CadSetup-Debug*
  (princ "\n[99_Aliases.lsp] Consolidated personal aliases loaded successfully.")
)
(princ)
