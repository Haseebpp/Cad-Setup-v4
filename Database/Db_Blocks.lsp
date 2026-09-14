;;; ==========================================================================
;;; Db_Blocks.lsp - Master Standard Joinery Block Dictionary & Geometry Data
;;; Layer: Database (Level 2 - Pure Data)
;;; ==========================================================================

;; ==============================================================================
;; Master Data Definition
;; Format: (
;;   BlockName            ; String: Unique AutoCAD block name
;;   Category             ; String: Category ("Connectors", "Hardware", "Carcass & Panels")
;;   Description          ; String: Component human-readable description
;;   TargetLayer          ; String: Recommended production layer
;;   Specs                ; String: Dimensions / technical specs
;;   BoundingBox          ; List: ((minX minY) (maxX maxY))
;;   Geometry             ; List of 2D primitive definitions for entmake and vector_image:
;;                        ;   (:line (x1 y1) (x2 y2))
;;                        ;   (:circle (cx cy) radius)
;;                        ;   (:rect (x1 y1) (x2 y2))
;;                        ;   (:poly ((x1 y1) (x2 y2) ...) isClosed)
;; )
;; ==============================================================================

(setq *CadSetup-Standard-Blocks-Data* '(

  ;; ----------------------------------------------------------------------------
  ;; 1. CONNECTORS & FASTENERS (R-HARD-FITTINGS)
  ;; ----------------------------------------------------------------------------
  (
    "MINIFIX_CAM_15"
    "Connectors"
    "Minifix 15mm Eccentric Cam & Rim (Drill D=15mm, Depth=12mm)"
    "R-HARD-FITTINGS"
    "D=15mm, Depth=12mm, Center datum=8mm"
    ((-7.5 -7.5) (7.5 7.5))
    (
      (:circle (0.0 0.0) 7.5)       ; Outer 15mm bore rim
      (:circle (0.0 0.0) 5.5)       ; Inner cam core
      (:line (-3.5 0.0) (3.5 0.0))  ; Pozidriv slot horizontal
      (:line (0.0 -3.5) (0.0 3.5))  ; Pozidriv slot vertical
      (:line (-2.0 -2.0) (2.0 2.0)) ; Cross diagonal 1
      (:line (-2.0 2.0) (2.0 -2.0)) ; Cross diagonal 2
    )
  )

  (
    "MINIFIX_PIN_34"
    "Connectors"
    "Minifix Connecting Bolt / Pin (L=34mm, Drill D=5mm/8mm)"
    "R-HARD-FITTINGS"
    "L=34mm, Thread M6x11mm, Shaft D=5mm"
    ((0.0 -3.5) (34.0 3.5))
    (
      (:rect (0.0 -3.0) (11.0 3.0))   ; Euro screw thread collar
      (:rect (11.0 -4.0) (13.0 4.0))  ; Stop ring / shoulder flange
      (:rect (13.0 -2.5) (27.0 2.5))  ; Smooth guide pin shaft
      (:circle (31.0 0.0) 3.0)        ; Spherical head engagement bulb
      (:line (27.0 -2.5) (30.0 -3.0)) ; Neck taper bottom
      (:line (27.0 2.5) (30.0 3.0))   ; Neck taper top
    )
  )

  (
    "DOWEL_8x30"
    "Connectors"
    "Wooden Fluted Alignment Dowel Pin (D=8mm x L=30mm)"
    "R-HARD-FITTINGS"
    "D=8mm, L=30mm with 2mm chamfered tips"
    ((-15.0 -4.0) (15.0 4.0))
    (
      (:poly ((-13.0 4.0) (13.0 4.0) (15.0 2.0) (15.0 -2.0) (13.0 -4.0) (-13.0 -4.0) (-15.0 -2.0) (-15.0 2.0)) T)
      (:line (-13.0 4.0) (-13.0 -4.0)) ; Left chamfer demarcation
      (:line (13.0 4.0) (13.0 -4.0))   ; Right chamfer demarcation
      (:line (-10.0 1.5) (10.0 1.5))   ; Flute indicator line top
      (:line (-10.0 -1.5) (10.0 -1.5)) ; Flute indicator line bottom
    )
  )

  (
    "SHELF_PIN_5"
    "Connectors"
    "Shelf Support Stud with Stop Collar (Pin D=5mm, Shelf Support=8mm)"
    "R-HARD-FITTINGS"
    "Pin D=5mm x 8mm, Flange D=8mm, Support Pad=10mm"
    ((-8.0 -3.5) (10.0 3.5))
    (
      (:rect (-8.0 -2.5) (0.0 2.5))   ; Carcass bore pin
      (:rect (0.0 -3.5) (2.0 3.5))    ; Middle stop flange collar
      (:poly ((2.0 2.5) (10.0 2.5) (10.0 -1.0) (4.0 -2.5) (2.0 -2.5)) T) ; Support shelf spoon tongue
      (:circle (6.0 0.5) 1.2)         ; Rubber pad / suction cup indicator
    )
  )

  (
    "CORNER_BRACKET_25"
    "Connectors"
    "Steel Angle Cleat / Corner Bracket 25x25x2mm with Screw Holes"
    "R-HARD-FITTINGS"
    "25mm x 25mm x 15mm wide, 2mm gauge steel"
    ((0.0 0.0) (25.0 25.0))
    (
      (:poly ((0.0 0.0) (25.0 0.0) (25.0 2.0) (2.0 2.0) (2.0 25.0) (0.0 25.0)) T) ; L-profile
      (:circle (14.0 1.0) 2.0)        ; Horizontal screw hole
      (:circle (1.0 14.0) 2.0)        ; Vertical screw hole
      (:line (2.0 2.0) (7.0 7.0))     ; Reinforcing gusset rib
    )
  )

  ;; ----------------------------------------------------------------------------
  ;; 2. HARDWARE & HINGES (R-HARD-HANDLES / R-HARD-RUNNERS / R-HARD-FITTINGS)
  ;; ----------------------------------------------------------------------------
  (
    "HINGE_CUP_35"
    "Hardware"
    "Concealed Euro Hinge Cup 35mm (Bore D=35mm, Screw CTC=45mm)"
    "R-HARD-FITTINGS"
    "Bore D=35mm x 11.5mm, Flange 62mm wide, Hole CTC=45mm"
    ((-31.0 -17.5) (31.0 17.5))
    (
      (:circle (0.0 0.0) 17.5)        ; 35mm cup body
      (:poly ((-31.0 -10.0) (-22.0 -17.5) (22.0 -17.5) (31.0 -10.0) (31.0 10.0) (22.0 17.5) (-22.0 17.5) (-31.0 10.0)) T) ; Mounting ear flange
      (:circle (-22.5 0.0) 2.5)       ; Left mounting screw hole (CTC 45mm)
      (:circle (22.5 0.0) 2.5)        ; Right mounting screw hole (CTC 45mm)
      (:rect (-10.0 -16.0) (10.0 0.0)); Hinge arm anchor block
    )
  )

  (
    "HINGE_PLATE_WING"
    "Hardware"
    "Cruciform Wing Mounting Plate (System 32mm Hole Spacing)"
    "R-HARD-FITTINGS"
    "52mm x 37mm, Hole spacing=32mm, Height=0mm/2mm/4mm"
    ((-26.0 -18.5) (26.0 18.5))
    (
      (:poly ((-26.0 -6.0) (-18.0 -18.5) (18.0 -18.5) (26.0 -6.0) (26.0 6.0) (18.0 18.5) (-18.0 18.5) (-26.0 6.0)) T) ; Wing perimeter
      (:circle (0.0 -16.0) 2.5)       ; Bottom System 32 hole
      (:circle (0.0 16.0) 2.5)        ; Top System 32 hole
      (:rect (-6.0 -8.0) (6.0 8.0))   ; Clip-on hinge snap chassis
      (:line (-6.0 0.0) (6.0 0.0))    ; Depth adjustment datum mark
    )
  )

  (
    "RUNNER_SLIDE_45"
    "Hardware"
    "Full Extension Ball-Bearing Drawer Slide 45mm Height Profile"
    "R-HARD-RUNNERS"
    "H=45mm x W=12.7mm, 3-Fold telescoping section"
    ((0.0 -22.5) (12.7 22.5))
    (
      (:rect (0.0 -22.5) (2.5 22.5))  ; Outer cabinet member base
      (:rect (2.5 -21.0) (4.0 21.0))  ; Outer channel lip
      (:circle (5.5 -12.0) 1.5)       ; Ball bearing 1
      (:circle (5.5 12.0) 1.5)        ; Ball bearing 2
      (:rect (7.0 -18.0) (9.0 18.0))  ; Intermediate sliding rail
      (:circle (10.5 -7.0) 1.5)       ; Inner ball bearing 3
      (:circle (10.5 7.0) 1.5)        ; Inner ball bearing 4
      (:rect (11.5 -16.0) (12.7 16.0)); Drawer box mounting member
    )
  )

  (
    "HANDLE_BAR_128"
    "Hardware"
    "Tubular Bar Handle D=12mm (CTC=128mm, Overall L=188mm)"
    "R-HARD-HANDLES"
    "D=12mm, CTC=128mm, Overall L=188mm, Projection=32mm"
    ((-94.0 0.0) (94.0 32.0))
    (
      (:rect (-94.0 20.0) (94.0 32.0)); Main tubular bar rod (D=12mm)
      (:rect (-68.0 0.0) (-60.0 20.0)); Left standoff leg (CTC 128mm)
      (:rect (60.0 0.0) (68.0 20.0))  ; Right standoff leg (CTC 128mm)
      (:line (-64.0 -3.0) (-64.0 0.0)); M4 mounting bolt center datum
      (:line (64.0 -3.0) (64.0 0.0))  ; M4 mounting bolt center datum
    )
  )

  (
    "HANDLE_EDGE_PULL"
    "Hardware"
    "Recessed J-Profile Aluminum Edge Pull Handle (L=50mm)"
    "R-HARD-HANDLES"
    "L=50mm, Back lip=18mm, J-grip pocket=16mm depth"
    ((0.0 -18.0) (22.0 4.0))
    (
      (:poly ((0.0 -18.0) (2.0 -18.0) (2.0 -2.0) (18.0 -2.0) (22.0 2.0) (22.0 4.0) (14.0 4.0) (14.0 0.0) (0.0 0.0)) T)
      (:line (2.0 -18.0) (2.0 0.0))   ; Door back panel alignment line
      (:circle (1.0 -10.0) 1.2)       ; Rear fixing countersunk screw
    )
  )

  ;; ----------------------------------------------------------------------------
  ;; 3. CARCASS, PANELS & ARCHITECTURAL JOINERY (R-LINE-VISB / R-MAT-PANEL-BOARD)
  ;; ----------------------------------------------------------------------------
  (
    "PANEL_18MM_CROSS"
    "Carcass & Panels"
    "18mm Substrate Panel Cross-Section with 1mm Edgeband (H=100mm)"
    "R-LINE-VISB"
    "Thick=18mm, Height=100mm, 1mm ABS Edgeband indicator"
    ((0.0 0.0) (18.0 100.0))
    (
      (:rect (0.0 0.0) (18.0 100.0))  ; 18mm board substrate
      (:line (1.0 0.0) (1.0 100.0))   ; Front 1mm ABS edgeband line
      (:line (17.0 0.0) (17.0 100.0)) ; Rear 1mm backing tape line
      (:line (0.0 0.0) (18.0 18.0))   ; Section slicing hatching guide
      (:line (0.0 50.0) (18.0 68.0))  ; Section slicing hatching guide
    )
  )

  (
    "PLINTH_LEG_100"
    "Carcass & Panels"
    "Adjustable Plinth Leveler Foot (H=100mm, Base D=50mm)"
    "R-HARD-FITTINGS"
    "Height=100mm (+/-15mm adjustment), Mounting base 60x60mm"
    ((-30.0 0.0) (30.0 100.0))
    (
      (:rect (-25.0 0.0) (25.0 12.0)) ; Bottom glide floor pad (D=50mm)
      (:rect (-9.0 12.0) (9.0 35.0))  ; Threaded adjuster spindle M10
      (:rect (-14.0 35.0) (14.0 88.0)); Main tubular leg cylinder
      (:poly ((-30.0 88.0) (30.0 88.0) (24.0 100.0) (-24.0 100.0)) T) ; Cabinet bottom mounting boss
      (:circle (-18.0 94.0) 2.0)      ; Left mounting screw hole
      (:circle (18.0 94.0) 2.0)       ; Right mounting screw hole
    )
  )

  (
    "HANGING_RAIL_32"
    "Carcass & Panels"
    "Wall Hanging Steel Rail / French Cleat Bracket Profile (H=32mm)"
    "R-LINE-VISB"
    "Profile H=32mm x W=8mm, Continuous interlocking cleat"
    ((0.0 0.0) (8.0 32.0))
    (
      (:poly ((0.0 0.0) (2.0 0.0) (2.0 18.0) (8.0 26.0) (8.0 32.0) (4.0 32.0) (0.0 24.0)) T) ; Interlocking hook profile
      (:circle (1.0 10.0) 2.5)        ; Wall anchor screw slot
    )
  )

))

;; ===========================================================================
;; DATABASE ACCESSORS
;; ===========================================================================

;; CadSetup:GetAllStandardBlocks - Returns complete list of standard joinery block definitions
(defun CadSetup:GetAllStandardBlocks ()
  *CadSetup-Standard-Blocks-Data*
)

;; CadSetup:GetBlockData - Looks up a specific standard block by name
(defun CadSetup:GetBlockData (blkName / match)
  (if (and blkName (= (type blkName) 'STR))
    (assoc (strcase blkName)
           (mapcar '(lambda (x) (cons (strcase (car x)) (cdr x))) *CadSetup-Standard-Blocks-Data*))
    nil
  )
)

;; CadSetup:GetBlockCategories - Returns unique sorted list of categories
(defun CadSetup:GetBlockCategories ( / cats )
  (setq cats nil)
  (foreach item *CadSetup-Standard-Blocks-Data*
    (if (not (vl-position (cadr item) cats))
      (setq cats (cons (cadr item) cats))
    )
  )
  (if cats
    (cons "All Standards" (acad_strlsort cats))
    '("All Standards")
  )
)

(if *CadSetup-Debug*
  (princ "\n[Database/Db_Blocks.lsp] Master Standard Joinery Block Dictionary loaded.")
)
(princ)
