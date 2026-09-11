;;; ==========================================================================
;;; CAD-SETTINGS-CTRL.lsp - Controller & Event Engine for CAD-SETTINGS Dialog
;;; Part of Cad-Setup-v3 Horizontal Layered Architecture
;;; Layer: UI (Event Handlers, Live Preview & Preset Controller)
;;; Commands: CAD-SETTINGS, CADSETTINGS
;;; ==========================================================================

(vl-load-com)

(defun c:CAD-SETTINGS (/ 
                      ;; Core Locals & Dialog Handle
                      *error* dcl-path dcl-id act is-applied count
                      ;; Original System Variables Cache
                      orig-lun orig-lupr orig-aun orig-aupr orig-ins orig-meas orig-angd orig-angb
                      orig-cur orig-pb orig-grp orig-ap orig-padd orig-pfirst orig-high
                      orig-dy orig-sp orig-drag orig-snst orig-gr orig-sn orig-orth orig-lw orig-mirr orig-osm
                      ;; Internal Functions & Lookup Data
                      *insunits-map* update-cursor-preview sync-slider-to-eb sync-eb-to-slider
                      apply-preset-data populate-dialog-controls apply-all-settings show-help-dialog)

  ;; -------------------------------------------------------------------------
  ;; 1. ERROR HANDLER & SAFE UNLOAD
  ;; -------------------------------------------------------------------------
  (defun *error* (msg)
    (if (and dcl-id (>= dcl-id 0))
      (progn
        (vl-catch-all-apply 'unload_dialog (list dcl-id))
        (setq dcl-id nil)
      )
    )
    (setvar "CMDECHO" 1)
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*exit*,*quit*")))
      (princ (strcat "\n[CAD-SETTINGS Error]: " msg))
    )
    (princ)
  )

  ;; Lookup Table for INSUNITS (Drawing Insertion Scale)
  (setq *insunits-map*
    '(
      (0  "0. Unitless (Unspecified)")
      (1  "1. Inches")
      (2  "2. Feet")
      (3  "3. Miles")
      (4  "4. Millimeters")
      (5  "5. Centimeters")
      (6  "6. Meters")
      (7  "7. Kilometers")
      (8  "8. Microinches")
      (9  "9. Mils")
      (10 "10. Yards")
      (14 "14. Decimeters")
    )
  )

  ;; -------------------------------------------------------------------------
  ;; 2. LIVE VECTOR PREVIEW CANVAS RENDERER
  ;; -------------------------------------------------------------------------
  (defun update-cursor-preview (cur-val pb-val grp-val ap-val grid-state / 
                                w h cx cy half-pb half-grp half-ap span gx gy sx sy)

    (setq cur-val (CadSetup:Clamp (if (numberp cur-val) cur-val (atoi (vl-princ-to-string cur-val))) 1 100)
          pb-val  (CadSetup:Clamp (if (numberp pb-val)  pb-val  (atoi (vl-princ-to-string pb-val)))  1 50)
          grp-val (CadSetup:Clamp (if (numberp grp-val) grp-val (atoi (vl-princ-to-string grp-val))) 1 50)
          ap-val  (CadSetup:Clamp (if (numberp ap-val)  ap-val  (atoi (vl-princ-to-string ap-val)))  1 50))

    (setq w (dimx_tile "img_preview")
          h (dimy_tile "img_preview"))

    (if (and w h (> w 0) (> h 0))
      (progn
        (setq cx (/ w 2)
              cy (/ h 2))

        (start_image "img_preview")
        ;; Dark Charcoal Background (ACI 250)
        (fill_image 0 0 w h 250)

        ;; 1. Draw Drafting Grid Dots (Color 8)
        (if (= grid-state "1")
          (progn
            (setq gx 12)
            (while (< gx w)
              (setq gy 10)
              (while (< gy h)
                (vector_image gx gy gx gy 8)
                (setq gy (+ gy 14))
              )
              (setq gx (+ gx 14))
            )
          )
        )

        ;; 2. Draw Crosshair (White / Color 7)
        (if (>= cur-val 100)
          (progn
            ;; Full Viewport Crosshair
            (vector_image 0 cy w cy 7)
            (vector_image cx 0 cx h 7)
          )
          (progn
            ;; Scaled Crosshair Span
            (setq span (max 4 (fix (* (/ (float cur-val) 100.0) cx))))
            (vector_image (- cx span) cy (+ cx span) cy 7)
            (vector_image cx (- cy span) cx (+ cy span) 7)
          )
        )

        ;; 3. Draw Grip Nodes on simulated line segments (Cyan / Color 4)
        (setq half-grp (max 2 (fix (/ (float grp-val) 2.0))))
        (foreach gx-pos (list (- cx 38) (+ cx 38))
          (if (and (> (- gx-pos half-grp) 3) (< (+ gx-pos half-grp) (- w 3)))
            (progn
              (vector_image (- gx-pos half-grp) (- cy half-grp) (+ gx-pos half-grp) (- cy half-grp) 4)
              (vector_image (+ gx-pos half-grp) (- cy half-grp) (+ gx-pos half-grp) (+ cy half-grp) 4)
              (vector_image (+ gx-pos half-grp) (+ cy half-grp) (- gx-pos half-grp) (+ cy half-grp) 4)
              (vector_image (- gx-pos half-grp) (+ cy half-grp) (- gx-pos half-grp) (- cy half-grp) 4)
              (vector_image (- gx-pos half-grp) (- cy half-grp) (+ gx-pos half-grp) (+ cy half-grp) 4)
              (vector_image (- gx-pos half-grp) (+ cy half-grp) (+ gx-pos half-grp) (- cy half-grp) 4)
            )
          )
        )

        ;; 4. Draw Osnap Target Aperture Box (Green / Color 3)
        (setq half-ap (max 3 (fix (/ (float ap-val) 2.0))))
        (vector_image (- cx half-ap) (- cy half-ap) (+ cx half-ap) (- cy half-ap) 3)
        (vector_image (+ cx half-ap) (- cy half-ap) (+ cx half-ap) (+ cy half-ap) 3)
        (vector_image (+ cx half-ap) (+ cy half-ap) (- cx half-ap) (+ cy half-ap) 3)
        (vector_image (- cx half-ap) (+ cy half-ap) (- cx half-ap) (- cy half-ap) 3)

        ;; 5. Draw Pickbox (Red / Color 1)
        (setq half-pb (max 2 (fix (/ (float pb-val) 2.0))))
        (vector_image (- cx half-pb) (- cy half-pb) (+ cx half-pb) (- cy half-pb) 1)
        (vector_image (+ cx half-pb) (- cy half-pb) (+ cx half-pb) (+ cy half-pb) 1)
        (vector_image (+ cx half-pb) (+ cy half-pb) (- cx half-pb) (+ cy half-pb) 1)
        (vector_image (- cx half-pb) (+ cy half-pb) (- cx half-pb) (- cy half-pb) 1)

        ;; 6. Draw Simulated Snap Marker (Yellow / Color 2) at top-right
        (setq sx (+ cx 22) sy (- cy 22))
        (vector_image (- sx 4) (- sy 4) (+ sx 4) (- sy 4) 2)
        (vector_image (+ sx 4) (- sy 4) (+ sx 4) (+ sy 4) 2)
        (vector_image (+ sx 4) (+ sy 4) (- sx 4) (+ sy 4) 2)
        (vector_image (- sx 4) (+ sy 4) (- sx 4) (- sy 4) 2)
        (vector_image (- sx 4) (- sy 4) (+ sx 4) (+ sy 4) 2)

        ;; 7. Coordinate axes hint at bottom-left corner
        (vector_image 8 (- h 8) 22 (- h 8) 1) ; X Axis (Red)
        (vector_image 8 (- h 8) 8 (- h 22) 3) ; Y Axis (Green)

        (end_image)
      )
    )
  )

  ;; -------------------------------------------------------------------------
  ;; 3. SLIDER & EDIT BOX BIDIRECTIONAL SYNC
  ;; -------------------------------------------------------------------------
  (defun sync-slider-to-eb (slider-key eb-key min-val max-val / val)
    (setq val (CadSetup:Clamp (atoi (get_tile slider-key)) min-val max-val))
    (set_tile eb-key (itoa val))
    (update-cursor-preview 
      (get_tile "eb_cur") 
      (get_tile "eb_pb") 
      (get_tile "eb_grp") 
      (get_tile "eb_ap") 
      (get_tile "tog_grid")
    )
  )

  (defun sync-eb-to-slider (eb-key slider-key min-val max-val / val-str val)
    (setq val-str (get_tile eb-key))
    (setq val (CadSetup:Clamp (atoi val-str) min-val max-val))
    (set_tile eb-key (itoa val))
    (set_tile slider-key (itoa val))
    (update-cursor-preview 
      (get_tile "eb_cur") 
      (get_tile "eb_pb") 
      (get_tile "eb_grp") 
      (get_tile "eb_ap") 
      (get_tile "tog_grid")
    )
  )

  ;; -------------------------------------------------------------------------
  ;; 4. PRESET APPLICATION ENGINE (Using Db_Presets.lsp)
  ;; -------------------------------------------------------------------------
  (defun apply-preset-data (preset-name / pRec tiles status)
    (cond
      ((= preset-name "RESET_ORIG")
       (populate-dialog-controls)
       (set_tile "txt_status" "Restored Initial Drawing Settings"))
      (t
       (setq pRec (CadSetup:GetPresetData preset-name))
       (if pRec
         (progn
           (setq tiles  (cdr (assoc :tiles (cdr pRec)))
                 status (cdr (assoc :status (cdr pRec))))
           (foreach pair tiles
             (set_tile (car pair) (cdr pair))
           )
           (if status (set_tile "txt_status" status))
         )
       )))
    (update-cursor-preview 
      (get_tile "eb_cur") 
      (get_tile "eb_pb") 
      (get_tile "eb_grp") 
      (get_tile "eb_ap") 
      (get_tile "tog_grid")
    )
  )

  ;; -------------------------------------------------------------------------
  ;; 5. POPULATE INITIAL DIALOG CONTROLS
  ;; -------------------------------------------------------------------------
  (defun populate-dialog-controls (/ ins-idx i)
    ;; Units & Precision
    (set_tile "pop_lunits" (itoa (CadSetup:Clamp (1- orig-lun) 0 4)))
    (set_tile "pop_luprec" (itoa (CadSetup:Clamp orig-lupr 0 8)))
    (set_tile "pop_aunits" (itoa (CadSetup:Clamp orig-aun 0 4)))
    (set_tile "pop_auprec" (itoa (CadSetup:Clamp orig-aupr 0 8)))

    ;; Insertion Units
    (setq ins-idx 0 i 0)
    (foreach item *insunits-map*
      (if (= (car item) orig-ins)
        (setq ins-idx i)
      )
      (setq i (1+ i))
    )
    (set_tile "pop_insunits" (itoa ins-idx))
    (set_tile "pop_meas"     (itoa (CadSetup:Clamp orig-meas 0 1)))
    (set_tile "pop_angd"     (itoa (CadSetup:Clamp orig-angd 0 1)))
    (set_tile "pop_angb"     (itoa (CadSetup:Clamp (fix orig-angb) 0 3)))

    ;; Cursor & Sizing
    (set_tile "eb_cur"  (itoa (CadSetup:Clamp orig-cur 1 100)))
    (set_tile "sld_cur" (itoa (CadSetup:Clamp orig-cur 1 100)))
    (set_tile "eb_pb"   (itoa (CadSetup:Clamp orig-pb 1 50)))
    (set_tile "sld_pb"  (itoa (CadSetup:Clamp orig-pb 1 50)))
    (set_tile "eb_grp"  (itoa (CadSetup:Clamp orig-grp 1 50)))
    (set_tile "sld_grp" (itoa (CadSetup:Clamp orig-grp 1 50)))
    (set_tile "eb_ap"   (itoa (CadSetup:Clamp orig-ap 1 50)))
    (set_tile "sld_ap"  (itoa (CadSetup:Clamp orig-ap 1 50)))

    ;; Drafting Aids
    (set_tile "pop_dyn"    (itoa (CadSetup:Clamp orig-dy 0 3)))
    (set_tile "pop_sp"     (itoa (CadSetup:Clamp orig-sp 0 3)))
    (set_tile "pop_drag"   (itoa (CadSetup:Clamp orig-drag 0 2)))
    (set_tile "pop_snst"   (itoa (CadSetup:Clamp orig-snst 0 1)))
    (set_tile "pop_selopt" (if (and (= orig-padd 2) (= orig-pfirst 1)) "1" "0"))

    ;; OSMODE mapping
    (cond
      ((= orig-osm 0)    (set_tile "pop_osm" "0"))
      ((= orig-osm 439)  (set_tile "pop_osm" "2"))
      ((= orig-osm 2735) (set_tile "pop_osm" "3"))
      (t                 (set_tile "pop_osm" "1"))
    )

    ;; Toggles
    (set_tile "tog_grid"  (if (/= orig-gr 0) "1" "0"))
    (set_tile "tog_snap"  (if (/= orig-sn 0) "1" "0"))
    (set_tile "tog_ortho" (if (/= orig-orth 0) "1" "0"))
    (set_tile "tog_lw"    (if (/= orig-lw 0) "1" "0"))
    (set_tile "tog_mirr"  (if (/= orig-mirr 0) "1" "0"))

    (set_tile "txt_status" "Ready. Adjust controls or select a preset.")
  )

  ;; -------------------------------------------------------------------------
  ;; 6. APPLY SETTINGS TO ACTIVE DRAWING ENVIRONMENT
  ;; -------------------------------------------------------------------------
  (defun apply-all-settings (/ lunit luprec aunit auprec insidx insval
                               meas angd angb cur pb grp ap dyn sp drag
                               snst selopt osm gr sn orth lw mirr modified-count)
    (setq modified-count 0)

    ;; Linear Units & Precision
    (setq lunit  (1+ (atoi (get_tile "pop_lunits")))
          luprec (atoi (get_tile "pop_luprec")))
    (if (CadSetup:SafeSetVar "LUNITS" lunit)   (setq modified-count (1+ modified-count)))
    (if (CadSetup:SafeSetVar "LUPREC" luprec) (setq modified-count (1+ modified-count)))

    ;; Angular Units & Precision
    (setq aunit  (atoi (get_tile "pop_aunits"))
          auprec (atoi (get_tile "pop_auprec")))
    (if (CadSetup:SafeSetVar "AUNITS" aunit)   (setq modified-count (1+ modified-count)))
    (if (CadSetup:SafeSetVar "AUPREC" auprec) (setq modified-count (1+ modified-count)))

    ;; Insertion Units
    (setq insidx (atoi (get_tile "pop_insunits")))
    (if (and (>= insidx 0) (< insidx (length *insunits-map*)))
      (progn
        (setq insval (car (nth insidx *insunits-map*)))
        (if (CadSetup:SafeSetVar "INSUNITS" insval) (setq modified-count (1+ modified-count)))
      )
    )

    ;; Measurement, Angle Direction & Base
    (setq meas (atoi (get_tile "pop_meas"))
          angd (atoi (get_tile "pop_angd"))
          angb (* (float (atoi (get_tile "pop_angb"))) 90.0))
    (if (CadSetup:SafeSetVar "MEASUREMENT" meas) (setq modified-count (1+ modified-count)))
    (if (CadSetup:SafeSetVar "ANGDIR" angd)      (setq modified-count (1+ modified-count)))
    (if (CadSetup:SafeSetVar "ANGBASE" angb)     (setq modified-count (1+ modified-count)))

    ;; Cursor & Sizing
    (setq cur (CadSetup:Clamp (atoi (get_tile "eb_cur")) 1 100)
          pb  (CadSetup:Clamp (atoi (get_tile "eb_pb"))  1 50)
          grp (CadSetup:Clamp (atoi (get_tile "eb_grp")) 1 50)
          ap  (CadSetup:Clamp (atoi (get_tile "eb_ap"))  1 50))
    (if (CadSetup:SafeSetVar "CURSORSIZE" cur) (setq modified-count (1+ modified-count)))
    (if (CadSetup:SafeSetVar "PICKBOX" pb)     (setq modified-count (1+ modified-count)))
    (if (CadSetup:SafeSetVar "GRIPSIZE" grp)   (setq modified-count (1+ modified-count)))
    (if (CadSetup:SafeSetVar "APERTURE" ap)    (setq modified-count (1+ modified-count)))

    ;; Dynamic Input, Preview & Drag
    (setq dyn  (atoi (get_tile "pop_dyn"))
          sp   (atoi (get_tile "pop_sp"))
          drag (atoi (get_tile "pop_drag"))
          snst (atoi (get_tile "pop_snst")))
    (if (CadSetup:SafeSetVar "DYNMODE" dyn)          (setq modified-count (1+ modified-count)))
    (if (CadSetup:SafeSetVar "SELECTIONPREVIEW" sp) (setq modified-count (1+ modified-count)))
    (if (CadSetup:SafeSetVar "DRAGMODE" drag)        (setq modified-count (1+ modified-count)))
    (if (CadSetup:SafeSetVar "SNAPSTYLE" snst)       (setq modified-count (1+ modified-count)))

    ;; Selection Options
    (setq selopt (atoi (get_tile "pop_selopt")))
    (if (= selopt 1)
      (progn
        (CadSetup:SafeSetVar "PICKADD" 2)
        (CadSetup:SafeSetVar "PICKFIRST" 1)
        (CadSetup:SafeSetVar "HIGHLIGHT" 1)
      )
      (progn
        (CadSetup:SafeSetVar "PICKADD" 0)
        (CadSetup:SafeSetVar "PICKFIRST" 1)
        (CadSetup:SafeSetVar "HIGHLIGHT" 1)
      )
    )

    ;; OSMODE Snaps
    (setq osm (atoi (get_tile "pop_osm")))
    (cond
      ((= osm 0) (CadSetup:SafeSetVar "OSMODE" 0))
      ((= osm 1) (CadSetup:SafeSetVar "OSMODE" 2215))
      ((= osm 2) (CadSetup:SafeSetVar "OSMODE" 439))
      ((= osm 3) (CadSetup:SafeSetVar "OSMODE" 2735))
    )

    ;; Toggles
    (setq gr   (atoi (get_tile "tog_grid"))
          sn   (atoi (get_tile "tog_snap"))
          orth (atoi (get_tile "tog_ortho"))
          lw   (atoi (get_tile "tog_lw"))
          mirr (atoi (get_tile "tog_mirr")))
    (CadSetup:SafeSetVar "GRIDMODE" gr)
    (CadSetup:SafeSetVar "SNAPMODE" sn)
    (CadSetup:SafeSetVar "ORTHOMODE" orth)
    (CadSetup:SafeSetVar "LWDISPLAY" lw)
    (CadSetup:SafeSetVar "MIRRTEXT" mirr)

    modified-count
  )

  ;; -------------------------------------------------------------------------
  ;; 7. HELP & SHORTCUTS DIALOG
  ;; -------------------------------------------------------------------------
  (defun show-help-dialog (/ h-id)
    (if (and dcl-path (findfile dcl-path))
      (progn
        (setq h-id (load_dialog dcl-path))
        (if (and h-id (>= h-id 0))
          (progn
            (if (new_dialog "cad_help_diag" h-id)
              (start_dialog)
            )
            (unload_dialog h-id)
          )
        )
      )
    )
  )

  ;; -------------------------------------------------------------------------
  ;; 8. READ CURRENT AUTOCAD ENVIRONMENT
  ;; -------------------------------------------------------------------------
  (setvar "CMDECHO" 0)

  (setq orig-lun   (CadSetup:SafeGetVar "LUNITS" 2)
        orig-lupr  (CadSetup:SafeGetVar "LUPREC" 4)
        orig-aun   (CadSetup:SafeGetVar "AUNITS" 0)
        orig-aupr  (CadSetup:SafeGetVar "AUPREC" 0)
        orig-ins   (CadSetup:SafeGetVar "INSUNITS" 4)
        orig-meas  (CadSetup:SafeGetVar "MEASUREMENT" 1)
        orig-angd  (CadSetup:SafeGetVar "ANGDIR" 0)
        orig-angb  (CadSetup:SafeGetVar "ANGBASE" 0.0)
        orig-cur   (CadSetup:SafeGetVar "CURSORSIZE" 5)
        orig-pb    (CadSetup:SafeGetVar "PICKBOX" 3)
        orig-grp   (CadSetup:SafeGetVar "GRIPSIZE" 5)
        orig-ap    (CadSetup:SafeGetVar "APERTURE" 10)
        orig-padd  (CadSetup:SafeGetVar "PICKADD" 2)
        orig-pfirst (CadSetup:SafeGetVar "PICKFIRST" 1)
        orig-high  (CadSetup:SafeGetVar "HIGHLIGHT" 1)
        orig-dy    (abs (CadSetup:SafeGetVar "DYNMODE" 3))
        orig-sp    (CadSetup:SafeGetVar "SELECTIONPREVIEW" 3)
        orig-drag  (CadSetup:SafeGetVar "DRAGMODE" 2)
        orig-snst  (CadSetup:SafeGetVar "SNAPSTYLE" 0)
        orig-gr    (CadSetup:SafeGetVar "GRIDMODE" 0)
        orig-sn    (CadSetup:SafeGetVar "SNAPMODE" 0)
        orig-orth  (CadSetup:SafeGetVar "ORTHOMODE" 0)
        orig-lw    (CadSetup:SafeGetVar "LWDISPLAY" 0)
        orig-mirr  (CadSetup:SafeGetVar "MIRRTEXT" 0)
        orig-osm   (CadSetup:SafeGetVar "OSMODE" 47))

  ;; -------------------------------------------------------------------------
  ;; 9. LOCATE AND LOAD STATIC DCL
  ;; -------------------------------------------------------------------------
  (setq dcl-path (strcat (CadSetup:GetDir) "\\UI\\CAD-SETTINGS.dcl"))
  (if (not (findfile dcl-path))
    (setq dcl-path (findfile "CAD-SETTINGS.dcl"))
  )

  (if (null dcl-path)
    (progn
      (princ "\n[CAD-SETTINGS Error]: CAD-SETTINGS.dcl not found in UI/ directory.")
      (exit)
    )
  )

  (setq dcl-id (load_dialog dcl-path))
  (if (or (null dcl-id) (< dcl-id 0))
    (progn
      (princ "\n[CAD-SETTINGS Error]: Unable to load dialog from CAD-SETTINGS.dcl.")
      (exit)
    )
  )

  (if (not (new_dialog "cad_main_dialog" dcl-id))
    (progn
      (unload_dialog dcl-id)
      (princ "\n[CAD-SETTINGS Error]: Unable to initialize 'cad_main_dialog'.")
      (exit)
    )
  )

  ;; -------------------------------------------------------------------------
  ;; 10. POPULATE POPUP CONTROLS
  ;; -------------------------------------------------------------------------
  ;; Linear Units
  (start_list "pop_lunits")
  (mapcar 'add_list '("1. Scientific" "2. Decimal" "3. Engineering" "4. Architectural" "5. Fractional"))
  (end_list)

  ;; Linear Precision
  (start_list "pop_luprec")
  (mapcar 'add_list '("0" "0.0" "0.00" "0.000" "0.0000" "0.00000" "0.000000" "0.0000000" "0.00000000"))
  (end_list)

  ;; Angular Units
  (start_list "pop_aunits")
  (mapcar 'add_list '("0. Decimal Degrees" "1. Deg/Min/Sec" "2. Grads" "3. Radians" "4. Surveyor's Units"))
  (end_list)

  ;; Angular Precision
  (start_list "pop_auprec")
  (mapcar 'add_list '("0" "0.0" "0.00" "0.000" "0.0000" "0.00000" "0.000000" "0.0000000" "0.00000000"))
  (end_list)

  ;; Insertion Scale
  (start_list "pop_insunits")
  (mapcar 'add_list (mapcar 'cadr *insunits-map*))
  (end_list)

  ;; Measurement Standard
  (start_list "pop_meas")
  (mapcar 'add_list '("0. Imperial (Inches / ANSI)" "1. Metric (Millimeters / ISO)"))
  (end_list)

  ;; Angle Direction
  (start_list "pop_angd")
  (mapcar 'add_list '("0. Counter-Clockwise (Standard)" "1. Clockwise"))
  (end_list)

  ;; Angle Base
  (start_list "pop_angb")
  (mapcar 'add_list '("0. East (0°)" "1. North (90°)" "2. West (180°)" "3. South (270°)"))
  (end_list)

  ;; Dynamic Input
  (start_list "pop_dyn")
  (mapcar 'add_list '("0. Off" "1. Pointer Input Only" "2. Dimensional Input Only" "3. Full (Pointer & Dimensional)"))
  (end_list)

  ;; Selection Preview
  (start_list "pop_sp")
  (mapcar 'add_list '("0. Off" "1. Active When No Commands" "2. Active During Commands" "3. Always Active (Full)"))
  (end_list)

  ;; Drag Dynamics
  (start_list "pop_drag")
  (mapcar 'add_list '("0. Off (No Dragging)" "1. On Demand (DRAG command)" "2. Auto Continuous Dragging"))
  (end_list)

  ;; Snap Grid Style
  (start_list "pop_snst")
  (mapcar 'add_list '("0. Standard 2D Rectangular Grid" "1. Isometric Snap Grid (SNAPSTYLE=1)"))
  (end_list)

  ;; Selection Mode
  (start_list "pop_selopt")
  (mapcar 'add_list '("0. Standard / Replace (PICKADD=0)" "1. Additive / Shift Not Required (PICKADD=2)"))
  (end_list)

  ;; Object Snaps
  (start_list "pop_osm")
  (mapcar 'add_list '(
    "0. All Snaps Off (OSMODE=0)"
    "1. Standard 2D Suite (End/Mid/Cen/Int/Perp/Ext - 2215)"
    "2. Mechanical Suite (End/Mid/Cen/Quad/Int/Perp/Tan - 439)"
    "3. Civil / Survey Suite (End/Mid/Cen/Node/Int/Perp/Near/Ext - 2735)"
  ))
  (end_list)

  ;; Populate initial values
  (populate-dialog-controls)

  ;; Render preview canvas
  (update-cursor-preview 
    (get_tile "eb_cur") 
    (get_tile "eb_pb") 
    (get_tile "eb_grp") 
    (get_tile "eb_ap") 
    (get_tile "tog_grid")
  )

  ;; -------------------------------------------------------------------------
  ;; 11. BIND TILE EVENT ACTIONS
  ;; -------------------------------------------------------------------------
  ;; Slider & Edit Box Sync Callbacks
  (action_tile "sld_cur" "(sync-slider-to-eb \"sld_cur\" \"eb_cur\" 1 100)")
  (action_tile "eb_cur"  "(sync-eb-to-slider \"eb_cur\" \"sld_cur\" 1 100)")
  (action_tile "sld_pb"  "(sync-slider-to-eb \"sld_pb\" \"eb_pb\" 1 50)")
  (action_tile "eb_pb"   "(sync-eb-to-slider \"eb_pb\" \"sld_pb\" 1 50)")
  (action_tile "sld_grp" "(sync-slider-to-eb \"sld_grp\" \"eb_grp\" 1 50)")
  (action_tile "eb_grp"  "(sync-eb-to-slider \"eb_grp\" \"sld_grp\" 1 50)")
  (action_tile "sld_ap"  "(sync-slider-to-eb \"sld_ap\" \"eb_ap\" 1 50)")
  (action_tile "eb_ap"   "(sync-eb-to-slider \"eb_ap\" \"sld_ap\" 1 50)")

  ;; Grid Toggle Canvas Update
  (action_tile "tog_grid" 
    "(update-cursor-preview (get_tile \"eb_cur\") (get_tile \"eb_pb\") (get_tile \"eb_grp\") (get_tile \"eb_ap\") $value)"
  )

  ;; Preset Button Actions
  (action_tile "btn_arch_mm"   "(apply-preset-data \"ARCH_MM\")")
  (action_tile "btn_arch_m"    "(apply-preset-data \"ARCH_M\")")
  (action_tile "btn_arch"      "(apply-preset-data \"ARCH\")")
  (action_tile "btn_struct"    "(apply-preset-data \"STRUCT_MM\")")
  (action_tile "btn_mech"      "(apply-preset-data \"MECH_MM\")")
  (action_tile "btn_civil"     "(apply-preset-data \"CIVIL_M\")")
  (action_tile "btn_elec"      "(apply-preset-data \"ELEC_MEP\")")
  (action_tile "btn_iso"       "(apply-preset-data \"ISO_25D\")")
  (action_tile "btn_cross"     "(apply-preset-data \"FULL_CROSS\")")
  (action_tile "btn_plot"      "(apply-preset-data \"PRESENT_PLOT\")")
  (action_tile "btn_default"   "(apply-preset-data \"DEFAULT\")")
  (action_tile "btn_reset_orig""(apply-preset-data \"RESET_ORIG\")")

  ;; Dialog Action Buttons
  (action_tile "btn_help"  "(show-help-dialog)")
  (action_tile "btn_apply" "(progn (setq count (apply-all-settings)) (set_tile \"txt_status\" (strcat \"Applied \" (itoa count) \" settings successfully.\")) (setq is-applied T))")
  (action_tile "accept"    "(progn (setq count (apply-all-settings)) (done_dialog 1))")
  (action_tile "cancel"    "(done_dialog 0)")

  ;; -------------------------------------------------------------------------
  ;; 12. RUN DIALOG & FINALIZE
  ;; -------------------------------------------------------------------------
  (setq act (start_dialog))
  (unload_dialog dcl-id)
  (setq dcl-id nil)

  (if (= act 1)
    (princ (strcat "\n[CAD-SETTINGS]: Configuration applied (" (itoa count) " variables updated).\n"))
    (if is-applied
      (princ "\n[CAD-SETTINGS]: Exited (Applied changes retained).\n")
      (princ "\n[CAD-SETTINGS]: Cancelled. No changes saved.\n")
    )
  )
  (princ)
)

(defun c:CADSETTINGS () (c:CAD-SETTINGS))

(princ "\n[UI/CAD-SETTINGS-CTRL.lsp] Settings Dialog Controller loaded. Type CAD-SETTINGS to launch.")
(princ)
