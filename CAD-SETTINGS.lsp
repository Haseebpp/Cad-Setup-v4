;;; ==========================================================================
;;; Program Name : CAD-SETTINGS.lsp
;;; Description  : Modern AutoCAD Core System Setup Manager
;;; Commands      : CAD-SETTINGS, CADSETTINGS
;;; Version      : 2.0.0 (Enhanced UI, Presets, Sliders & Error-Proof Engine)
;;; ==========================================================================

(vl-load-com)

(defun c:CAD-SETTINGS (/ 
                      ;; Core Locals & File Pointers
                      *error* dcl-file dcl-fp dcl-id act is-applied count
                      ;; Original System Variables Cache
                      orig-lun orig-lupr orig-aun orig-aupr orig-ins orig-meas orig-angd orig-angb
                      orig-cur orig-pb orig-grp orig-ap orig-padd orig-pfirst orig-high
                      orig-dy orig-sp orig-drag orig-snst orig-gr orig-sn orig-orth orig-lw orig-mirr orig-osm
                      ;; Active State Variables
                      cur-lun cur-lupr cur-aun cur-aupr cur-ins cur-meas cur-angd cur-angb
                      cur-cur cur-pb cur-grp cur-ap cur-padd cur-pfirst cur-high
                      cur-dy cur-sp cur-drag cur-snst cur-gr cur-sn cur-orth cur-lw cur-mirr cur-osm
                      ;; Internal Helper Functions & Data
                      safe-getvar safe-setvar clamp update-cursor-preview
                      sync-slider-to-eb sync-eb-to-slider apply-preset
                      apply-all-settings populate-dialog-controls
                      show-help-dialog *insunits-map*)

  ;; -------------------------------------------------------------------------
  ;; 1. ERROR HANDLER & CLEANUP
  ;; -------------------------------------------------------------------------
  (defun *error* (msg)
    (if dcl-fp
      (progn
        (vl-catch-all-apply 'close (list dcl-fp))
        (setq dcl-fp nil)
      )
    )
    (if (and dcl-id (>= dcl-id 0))
      (progn
        (vl-catch-all-apply 'unload_dialog (list dcl-id))
        (setq dcl-id nil)
      )
    )
    (if (and dcl-file (findfile dcl-file))
      (vl-catch-all-apply 'vl-file-delete (list dcl-file))
    )
    (setvar "CMDECHO" 1)
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*exit*,*quit*")))
      (princ (strcat "\n[Settings Configurator Error]: " msg))
    )
    (princ)
  )

  ;; -------------------------------------------------------------------------
  ;; 2. SAFE VARIABLE ACCESS & MATH HELPERS
  ;; -------------------------------------------------------------------------
  (defun safe-getvar (vname default / v)
    (if (vl-catch-all-error-p (setq v (vl-catch-all-apply 'getvar (list vname))))
      default
      (if (null v) default v)
    )
  )

  (defun safe-setvar (vname val / res)
    (if (and vname val)
      (progn
        (setq res (vl-catch-all-apply 'setvar (list vname val)))
        (not (vl-catch-all-error-p res))
      )
      nil
    )
  )

  (defun clamp (val min-val max-val)
    (max min-val (min max-val val))
  )

  ;; Lookup Table for INSUNITS (Drawing Insertion Scale)
  ;; Format: (Index Code Label Units-Desc)
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
  ;; 3. LIVE VECTOR PREVIEW CANVAS RENDERER
  ;; -------------------------------------------------------------------------
  (defun update-cursor-preview (cur-val pb-val grp-val ap-val grid-state / 
                                w h cx cy half-pb half-grp half-ap span gx gy sx sy)
    (setq cur-val (clamp (if (numberp cur-val) cur-val (atoi (vl-princ-to-string cur-val))) 1 100)
          pb-val  (clamp (if (numberp pb-val)  pb-val  (atoi (vl-princ-to-string pb-val)))  1 50)
          grp-val (clamp (if (numberp grp-val) grp-val (atoi (vl-princ-to-string grp-val))) 1 50)
          ap-val  (clamp (if (numberp ap-val)  ap-val  (atoi (vl-princ-to-string ap-val)))  1 50))

    (setq w (dimx_tile "img_preview")
          h (dimy_tile "img_preview"))

    (if (and w h (> w 0) (> h 0))
      (progn
        (setq cx (/ w 2)
              cy (/ h 2))

        (start_image "img_preview")
        ;; Dark Charcoal Modern Background (ACI 250)
        (fill_image 0 0 w h 250)

        ;; 1. Draw Subtle Drafting Grid Dots (Color 8)
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
              ;; Filled box simulation
              (vector_image (- gx-pos half-grp) (- cy half-grp) (+ gx-pos half-grp) (- cy half-grp) 4)
              (vector_image (+ gx-pos half-grp) (- cy half-grp) (+ gx-pos half-grp) (+ cy half-grp) 4)
              (vector_image (+ gx-pos half-grp) (+ cy half-grp) (- gx-pos half-grp) (+ cy half-grp) 4)
              (vector_image (- gx-pos half-grp) (+ cy half-grp) (- gx-pos half-grp) (- cy half-grp) 4)
              ;; Solid inner cross for grip node
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
  ;; 4. SLIDER & EDIT BOX BIDIRECTIONAL SYNC
  ;; -------------------------------------------------------------------------
  (defun sync-slider-to-eb (slider-key eb-key min-val max-val / val)
    (setq val (clamp (atoi (get_tile slider-key)) min-val max-val))
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
    (setq val (clamp (atoi val-str) min-val max-val))
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
  ;; 5. PRESET CONFIGURATION ENGINE (12 Comprehensive Industry Profiles)
  ;; -------------------------------------------------------------------------
  (defun apply-preset (preset-name)
    (cond
      ;; 1. Architectural (Metric Millimeters / ISO)
      ((= preset-name "ARCH_MM")
        (set_tile "pop_lunits" "1")   ; 2. Decimal
        (set_tile "pop_luprec" "0")   ; 0 mm (Standard ISO building floorplan precision)
        (set_tile "pop_aunits" "0")   ; Decimal Degrees
        (set_tile "pop_auprec" "2")   ; 0.00
        (set_tile "pop_insunits" "4") ; 4. Millimeters
        (set_tile "pop_meas" "1")     ; 1. Metric (ISO Hatch & Linetypes)
        (set_tile "pop_angd" "0")     ; Counter-Clockwise
        (set_tile "pop_angb" "0")     ; 0° East
        (set_tile "eb_cur" "100")     (set_tile "sld_cur" "100") ; 100% Crosshair
        (set_tile "eb_pb" "5")        (set_tile "sld_pb" "5")
        (set_tile "eb_grp" "6")       (set_tile "sld_grp" "6")
        (set_tile "eb_ap" "10")       (set_tile "sld_ap" "10")
        (set_tile "pop_dyn" "3")
        (set_tile "pop_sp" "3")
        (set_tile "pop_drag" "2")
        (set_tile "pop_snst" "0")
        (set_tile "pop_selopt" "1")
        (set_tile "pop_osm" "1")      ; Standard 2D Suite (End/Mid/Cen/Int/Perp/Ext - 2215)
        (set_tile "tog_grid" "1")
        (set_tile "tog_snap" "0")
        (set_tile "tog_ortho" "1")
        (set_tile "tog_lw" "1")
        (set_tile "tog_mirr" "0")
        (set_tile "txt_status" "Applied Preset: Architectural Metric ISO (Millimeters, LUPREC=0, INSUNITS=mm, Ortho ON)")
      )

      ;; 2. Architectural Masterplan & Site Planning (Metric Meters)
      ((= preset-name "ARCH_M")
        (set_tile "pop_lunits" "1")   ; 2. Decimal
        (set_tile "pop_luprec" "3")   ; 0.000 m (Millimeter terrain accuracy)
        (set_tile "pop_aunits" "0")   ; Decimal Degrees
        (set_tile "pop_auprec" "2")   ; 0.00
        (set_tile "pop_insunits" "6") ; 6. Meters
        (set_tile "pop_meas" "1")     ; 1. Metric
        (set_tile "pop_angd" "0")     ; Counter-Clockwise
        (set_tile "pop_angb" "0")     ; 0° East
        (set_tile "eb_cur" "100")     (set_tile "sld_cur" "100") ; 100% Crosshair
        (set_tile "eb_pb" "6")        (set_tile "sld_pb" "6")
        (set_tile "eb_grp" "7")       (set_tile "sld_grp" "7")
        (set_tile "eb_ap" "12")       (set_tile "sld_ap" "12")
        (set_tile "pop_dyn" "3")
        (set_tile "pop_sp" "3")
        (set_tile "pop_drag" "2")
        (set_tile "pop_snst" "0")
        (set_tile "pop_selopt" "1")
        (set_tile "pop_osm" "1")      ; Standard 2D Suite (2215)
        (set_tile "tog_grid" "1")
        (set_tile "tog_snap" "0")
        (set_tile "tog_ortho" "1")
        (set_tile "tog_lw" "1")
        (set_tile "tog_mirr" "0")
        (set_tile "txt_status" "Applied Preset: Architectural Masterplan (Meters, LUPREC=3, INSUNITS=Meters, Ortho ON)")
      )

      ;; 3. Architectural (Imperial Feet & Fractional Inches)
      ((= preset-name "ARCH")
        (set_tile "pop_lunits" "3")   ; 4. Architectural (Feet & Fractional Inches)
        (set_tile "pop_luprec" "4")   ; 1/16" precision (Construction Standard)
        (set_tile "pop_aunits" "0")   ; Decimal Degrees
        (set_tile "pop_auprec" "2")   ; 0.00
        (set_tile "pop_insunits" "1") ; 1. Inches (Essential: 1 Drawing Unit = 1 Inch for Blocks/Xrefs)
        (set_tile "pop_meas" "0")     ; 0. Imperial (ANSI Hatch & Linetypes)
        (set_tile "pop_angd" "0")     ; Counter-Clockwise (Standard CAD)
        (set_tile "pop_angb" "0")     ; 0° East
        (set_tile "eb_cur" "100")     (set_tile "sld_cur" "100") ; 100% Crosshair for wall/grid alignment
        (set_tile "eb_pb" "5")        (set_tile "sld_pb" "5")
        (set_tile "eb_grp" "6")       (set_tile "sld_grp" "6")
        (set_tile "eb_ap" "10")       (set_tile "sld_ap" "10")
        (set_tile "pop_dyn" "3")      ; Full Dynamic Input (Pointer & Dimensional)
        (set_tile "pop_sp" "3")       ; Selection Highlight Always Active
        (set_tile "pop_drag" "2")     ; Auto Dynamic Dragging
        (set_tile "pop_snst" "0")     ; 2D Rectangular Grid
        (set_tile "pop_selopt" "1")   ; Additive Selection (PICKADD=2, PICKFIRST=1)
        (set_tile "pop_osm" "1")      ; Standard 2D Suite (End/Mid/Cen/Int/Perp/Ext - 2215)
        (set_tile "tog_grid" "1")     ; Grid Display ON
        (set_tile "tog_snap" "0")     ; Snap OFF
        (set_tile "tog_ortho" "1")    ; Ortho ON
        (set_tile "tog_lw" "1")       ; Lineweight Display ON
        (set_tile "tog_mirr" "0")     ; MIRRTEXT=0 (Readable Text)
        (set_tile "txt_status" "Applied Preset: Architectural Imperial (Feet & 1/16\" Inches, INSUNITS=Inches, Ortho ON)")
      )

      ;; 4. Structural Engineering (Metric Millimeters)
      ((= preset-name "STRUCT_MM")
        (set_tile "pop_lunits" "1")   ; 2. Decimal
        (set_tile "pop_luprec" "1")   ; 0.0 mm
        (set_tile "pop_aunits" "0")   ; Decimal Degrees
        (set_tile "pop_auprec" "2")   ; 0.00
        (set_tile "pop_insunits" "4") ; 4. Millimeters
        (set_tile "pop_meas" "1")     ; 1. Metric
        (set_tile "pop_angd" "0")     ; Counter-Clockwise
        (set_tile "pop_angb" "0")     ; 0° East
        (set_tile "eb_cur" "100")     (set_tile "sld_cur" "100") ; 100% Crosshair
        (set_tile "eb_pb" "6")        (set_tile "sld_pb" "6")
        (set_tile "eb_grp" "7")       (set_tile "sld_grp" "7")
        (set_tile "eb_ap" "10")       (set_tile "sld_ap" "10")
        (set_tile "pop_dyn" "3")
        (set_tile "pop_sp" "3")
        (set_tile "pop_drag" "2")
        (set_tile "pop_snst" "0")
        (set_tile "pop_selopt" "1")
        (set_tile "pop_osm" "1")      ; Standard 2D Suite (2215)
        (set_tile "tog_grid" "1")
        (set_tile "tog_snap" "0")
        (set_tile "tog_ortho" "1")
        (set_tile "tog_lw" "1")
        (set_tile "tog_mirr" "0")
        (set_tile "txt_status" "Applied Preset: Structural Engineering (Metric mm, Precision 0.0, Lineweights ON)")
      )

      ;; 5. Mechanical / Manufacturing (Metric Millimeters)
      ((= preset-name "MECH_MM")
        (set_tile "pop_lunits" "1")   ; 2. Decimal
        (set_tile "pop_luprec" "2")   ; 0.00 mm (Machining precision)
        (set_tile "pop_aunits" "0")   ; Decimal Degrees
        (set_tile "pop_auprec" "2")   ; 0.00
        (set_tile "pop_insunits" "4") ; 4. Millimeters
        (set_tile "pop_meas" "1")     ; 1. Metric
        (set_tile "pop_angd" "0")     ; Counter-Clockwise
        (set_tile "pop_angb" "0")     ; 0° East
        (set_tile "eb_cur" "8")       (set_tile "sld_cur" "8") ; Focused crosshair for intricate components
        (set_tile "eb_pb" "4")        (set_tile "sld_pb" "4")
        (set_tile "eb_grp" "5")       (set_tile "sld_grp" "5")
        (set_tile "eb_ap" "8")        (set_tile "sld_ap" "8")
        (set_tile "pop_dyn" "3")
        (set_tile "pop_sp" "3")
        (set_tile "pop_drag" "2")
        (set_tile "pop_snst" "0")
        (set_tile "pop_selopt" "1")
        (set_tile "pop_osm" "2")      ; Mechanical Suite (End/Mid/Cen/Quad/Int/Perp/Tan - 439)
        (set_tile "tog_grid" "1")
        (set_tile "tog_snap" "0")
        (set_tile "tog_ortho" "0")    ; Polar drafting freedom
        (set_tile "tog_lw" "1")
        (set_tile "tog_mirr" "0")
        (set_tile "txt_status" "Applied Preset: Mechanical / Manufacturing (Metric mm, Precision 0.00, Tangent/Quad Snaps ON)")
      )

      ;; 6. Civil Engineering & Surveying (Metric Meters)
      ((= preset-name "CIVIL_M")
        (set_tile "pop_lunits" "1")   ; 2. Decimal
        (set_tile "pop_luprec" "3")   ; 0.000 m (Millimeter terrain accuracy)
        (set_tile "pop_aunits" "1")   ; 1. Deg/Min/Sec (Surveyor Angle Convention)
        (set_tile "pop_auprec" "2")   ; 0d00'00" (Seconds precision)
        (set_tile "pop_insunits" "6") ; 6. Meters
        (set_tile "pop_meas" "1")     ; 1. Metric
        (set_tile "pop_angd" "1")     ; 1. Clockwise (True Surveyor Azimuth & Bearings)
        (set_tile "pop_angb" "1")     ; 90° North (Surveyor 0° baseline)
        (set_tile "eb_cur" "100")     (set_tile "sld_cur" "100") ; 100% Crosshair across terrain contours
        (set_tile "eb_pb" "5")        (set_tile "sld_pb" "5")
        (set_tile "eb_grp" "6")       (set_tile "sld_grp" "6")
        (set_tile "eb_ap" "10")       (set_tile "sld_ap" "10")
        (set_tile "pop_dyn" "3")
        (set_tile "pop_sp" "3")
        (set_tile "pop_drag" "2")
        (set_tile "pop_snst" "0")
        (set_tile "pop_selopt" "1")
        (set_tile "pop_osm" "3")      ; Civil / Survey Suite (End/Mid/Cen/Node/Int/Perp/Near/Ext - 2735)
        (set_tile "tog_grid" "1")
        (set_tile "tog_snap" "0")
        (set_tile "tog_ortho" "0")
        (set_tile "tog_lw" "1")
        (set_tile "tog_mirr" "0")
        (set_tile "txt_status" "Applied Preset: Civil / Surveying (Meters 0.000, DMS Angles, North Azimuth, Node Snaps)")
      )

      ;; 7. Electrical & MEP Schematics
      ((= preset-name "ELEC_MEP")
        (set_tile "pop_lunits" "1")   ; 2. Decimal
        (set_tile "pop_luprec" "0")   ; 0 mm
        (set_tile "pop_aunits" "0")   ; Decimal Degrees
        (set_tile "pop_auprec" "0")   ; 0
        (set_tile "pop_insunits" "4") ; 4. Millimeters
        (set_tile "pop_meas" "1")     ; 1. Metric
        (set_tile "pop_angd" "0")     ; Counter-Clockwise
        (set_tile "pop_angb" "0")     ; 0° East
        (set_tile "eb_cur" "50")      (set_tile "sld_cur" "50")
        (set_tile "eb_pb" "5")        (set_tile "sld_pb" "5")
        (set_tile "eb_grp" "6")       (set_tile "sld_grp" "6")
        (set_tile "eb_ap" "10")       (set_tile "sld_ap" "10")
        (set_tile "pop_dyn" "3")
        (set_tile "pop_sp" "3")
        (set_tile "pop_drag" "2")
        (set_tile "pop_snst" "0")
        (set_tile "pop_selopt" "1")
        (set_tile "pop_osm" "1")      ; Standard 2D Suite (2215)
        (set_tile "tog_grid" "1")     ; Grid ON
        (set_tile "tog_snap" "1")     ; Snap ON (Grid alignment for schematics)
        (set_tile "tog_ortho" "1")    ; Ortho ON
        (set_tile "tog_lw" "1")
        (set_tile "tog_mirr" "0")     ; Readable Text
        (set_tile "txt_status" "Applied Preset: Electrical & MEP Schematics (Snap/Grid Aligned, Ortho ON, MIRRTEXT=0)")
      )

      ;; 8. Isometric 2.5D Drafting (Piping, HVAC, Diagrams)
      ((= preset-name "ISO_25D")
        (set_tile "pop_lunits" "1")   ; 2. Decimal
        (set_tile "pop_luprec" "1")   ; 0.0
        (set_tile "pop_aunits" "0")   ; Decimal Degrees
        (set_tile "pop_auprec" "2")   ; 0.00
        (set_tile "pop_insunits" "4") ; 4. Millimeters
        (set_tile "pop_meas" "1")     ; 1. Metric
        (set_tile "pop_angd" "0")     ; Counter-Clockwise
        (set_tile "pop_angb" "0")     ; 0° East
        (set_tile "eb_cur" "100")     (set_tile "sld_cur" "100") ; 100% Crosshair for isometric isoplane axis orientation
        (set_tile "eb_pb" "5")        (set_tile "sld_pb" "5")
        (set_tile "eb_grp" "6")       (set_tile "sld_grp" "6")
        (set_tile "eb_ap" "10")       (set_tile "sld_ap" "10")
        (set_tile "pop_dyn" "3")
        (set_tile "pop_sp" "3")
        (set_tile "pop_drag" "2")
        (set_tile "pop_snst" "1")     ; 1. Isometric Snap Grid (SNAPSTYLE=1)
        (set_tile "pop_selopt" "1")
        (set_tile "pop_osm" "1")      ; Standard 2D Suite (2215)
        (set_tile "tog_grid" "1")     ; Grid ON
        (set_tile "tog_snap" "1")     ; Isometric Snap ON
        (set_tile "tog_ortho" "1")    ; Ortho ON (Constrains to 30°/90°/150° isoplanes)
        (set_tile "tog_lw" "1")
        (set_tile "tog_mirr" "0")
        (set_tile "txt_status" "Applied Preset: Isometric 2.5D Drafting (SNAPSTYLE=Isometric, Ortho ON, Grid ON, 100% Crosshair)")
      )

      ;; 9. Full Screen Crosshair & Pro Drafter Mode
      ((= preset-name "FULL_CROSS")
        (set_tile "eb_cur" "100")     (set_tile "sld_cur" "100")
        (set_tile "eb_pb" "7")        (set_tile "sld_pb" "7")
        (set_tile "eb_grp" "8")       (set_tile "sld_grp" "8")
        (set_tile "eb_ap" "14")       (set_tile "sld_ap" "14")
        (set_tile "pop_dyn" "3")
        (set_tile "pop_sp" "3")
        (set_tile "pop_drag" "2")
        (set_tile "pop_selopt" "1")
        (set_tile "pop_osm" "1")
        (set_tile "tog_lw" "1")
        (set_tile "tog_grid" "1")
        (set_tile "txt_status" "Applied Preset: 100% Pro Drafter & 4K High Visibility Ergonomics")
      )

      ;; 10. Clean Presentation & Plot Preparation
      ((= preset-name "PRESENT_PLOT")
        (set_tile "eb_cur" "5")       (set_tile "sld_cur" "5")
        (set_tile "eb_pb" "3")        (set_tile "sld_pb" "3")
        (set_tile "eb_grp" "4")       (set_tile "sld_grp" "4")
        (set_tile "eb_ap" "8")        (set_tile "sld_ap" "8")
        (set_tile "pop_dyn" "0")      ; Dynamic Input Off for clean presentation
        (set_tile "pop_sp" "0")       ; Selection Preview Off
        (set_tile "pop_drag" "2")
        (set_tile "pop_selopt" "1")
        (set_tile "pop_osm" "0")      ; Snaps Off
        (set_tile "tog_grid" "0")     ; Grid Off
        (set_tile "tog_snap" "0")     ; Snap Off
        (set_tile "tog_ortho" "0")
        (set_tile "tog_lw" "1")       ; Lineweights ON to preview line hierarchy
        (set_tile "tog_mirr" "0")
        (set_tile "txt_status" "Applied Preset: Presentation & Plot Preparation (Lineweights ON, Grid OFF, Snaps OFF)")
      )

      ;; 11. AutoCAD Factory Defaults
      ((= preset-name "DEFAULT")
        (set_tile "pop_lunits" "1")   ; 2. Decimal
        (set_tile "pop_luprec" "4")   ; 0.0000
        (set_tile "pop_aunits" "0")   ; Decimal Degrees
        (set_tile "pop_auprec" "0")   ; 0
        (set_tile "pop_insunits" "4") ; 4. Millimeters
        (set_tile "pop_meas" "1")     ; 1. Metric
        (set_tile "pop_angd" "0")     ; Counter-Clockwise
        (set_tile "pop_angb" "0")     ; 0° East
        (set_tile "eb_cur" "5")       (set_tile "sld_cur" "5")
        (set_tile "eb_pb" "3")        (set_tile "sld_pb" "3")
        (set_tile "eb_grp" "5")       (set_tile "sld_grp" "5")
        (set_tile "eb_ap" "10")       (set_tile "sld_ap" "10")
        (set_tile "pop_dyn" "3")
        (set_tile "pop_sp" "3")
        (set_tile "pop_drag" "2")
        (set_tile "pop_snst" "0")     ; 2D Rectangular
        (set_tile "pop_selopt" "1")   ; PICKADD=2
        (set_tile "pop_osm" "1")      ; Standard 2D Suite
        (set_tile "tog_grid" "0")
        (set_tile "tog_snap" "0")
        (set_tile "tog_ortho" "0")
        (set_tile "tog_lw" "0")
        (set_tile "tog_mirr" "0")
        (set_tile "txt_status" "Reset to Standard Factory Default CAD Settings")
      )

      ;; 12. Reset to Initial Drawing Session Values
      ((= preset-name "RESET_ORIG")
        (populate-dialog-controls)
        (set_tile "txt_status" "Restored Initial Drawing Settings")
      )
    )
    (update-cursor-preview 
      (get_tile "eb_cur") 
      (get_tile "eb_pb") 
      (get_tile "eb_grp") 
      (get_tile "eb_ap") 
      (get_tile "tog_grid")
    )
  )

  ;; -------------------------------------------------------------------------
  ;; 6. POPULATE INITIAL DIALOG CONTROLS
  ;; -------------------------------------------------------------------------
  (defun populate-dialog-controls (/ ins-idx i)
    ;; Units & Precision
    (set_tile "pop_lunits" (itoa (clamp (1- orig-lun) 0 4)))
    (set_tile "pop_luprec" (itoa (clamp orig-lupr 0 8)))
    (set_tile "pop_aunits" (itoa (clamp orig-aun 0 4)))
    (set_tile "pop_auprec" (itoa (clamp orig-aupr 0 8)))

    ;; Insertion Units
    (setq ins-idx 0 i 0)
    (foreach item *insunits-map*
      (if (= (car item) orig-ins)
        (setq ins-idx i)
      )
      (setq i (1+ i))
    )
    (set_tile "pop_insunits" (itoa ins-idx))

    (set_tile "pop_meas" (if (= orig-meas 1) "1" "0"))
    (set_tile "pop_angd" (if (= orig-angd 1) "1" "0"))
    (cond
      ((= (fix orig-angb) 90)  (set_tile "pop_angb" "1"))
      ((= (fix orig-angb) 180) (set_tile "pop_angb" "2"))
      ((= (fix orig-angb) 270) (set_tile "pop_angb" "3"))
      (t                       (set_tile "pop_angb" "0"))
    )

    ;; Cursor & Sizing Controls
    (set_tile "eb_cur"  (itoa (clamp orig-cur 1 100)))
    (set_tile "sld_cur" (itoa (clamp orig-cur 1 100)))
    (set_tile "eb_pb"   (itoa (clamp orig-pb 1 50)))
    (set_tile "sld_pb"  (itoa (clamp orig-pb 1 50)))
    (set_tile "eb_grp"  (itoa (clamp orig-grp 1 50)))
    (set_tile "sld_grp" (itoa (clamp orig-grp 1 50)))
    (set_tile "eb_ap"   (itoa (clamp orig-ap 1 50)))
    (set_tile "sld_ap"  (itoa (clamp orig-ap 1 50)))

    ;; Drafting Aids
    (set_tile "pop_dyn"  (itoa (clamp orig-dy 0 3)))
    (set_tile "pop_sp"   (itoa (clamp orig-sp 0 3)))
    (set_tile "pop_drag" (itoa (clamp orig-drag 0 2)))
    (set_tile "pop_snst" (itoa (clamp orig-snst 0 1)))

    ;; Selection Behavior
    (cond
      ((and (= orig-padd 2) (= orig-pfirst 1)) (set_tile "pop_selopt" "1")) ; Standard Additive
      ((= orig-padd 0)                         (set_tile "pop_selopt" "0")) ; Single Replace
      (t                                       (set_tile "pop_selopt" "1"))
    )

    ;; Osnap Mode Mapping
    (cond
      ((= orig-osm 0)     (set_tile "pop_osm" "0")) ; Snaps Off
      ((= orig-osm 2215)  (set_tile "pop_osm" "1")) ; Standard 2D Drafting Suite
      ((= orig-osm 439)   (set_tile "pop_osm" "2")) ; Mechanical Suite
      ((= orig-osm 2735)  (set_tile "pop_osm" "3")) ; Civil / Survey Suite
      ((= orig-osm 8191)  (set_tile "pop_osm" "4")) ; Full All-Snaps Suite
      ((= orig-osm 16384) (set_tile "pop_osm" "5")) ; Suppressed Snap Tracking
      ((= orig-osm 47)    (set_tile "pop_osm" "1")) ; Legacy Essential
      ((= orig-osm 4263)  (set_tile "pop_osm" "4")) ; Legacy Full
      (t                  (set_tile "pop_osm" "1"))
    )

    ;; Toggles
    (set_tile "tog_grid"  (if (= orig-gr 1) "1" "0"))
    (set_tile "tog_snap"  (if (= orig-sn 1) "1" "0"))
    (set_tile "tog_ortho" (if (= orig-orth 1) "1" "0"))
    (set_tile "tog_lw"    (if (= orig-lw 1) "1" "0"))
    (set_tile "tog_mirr"  (if (= orig-mirr 1) "1" "0"))

    (set_tile "txt_status" "Ready. Adjust settings, pick a preset, or click Apply/OK.")
  )

  ;; -------------------------------------------------------------------------
  ;; 7. APPLY SETTINGS TO ACTIVE DRAWING ENVIRONMENT
  ;; -------------------------------------------------------------------------
  (defun apply-all-settings (/ ins-entry target-osm changed-count angb-idx)
    (setq changed-count 0)

    ;; Read current UI values safely
    (setq cur-lun   (1+ (clamp (atoi (get_tile "pop_lunits")) 0 4))
          cur-lupr  (clamp (atoi (get_tile "pop_luprec")) 0 8)
          cur-aun   (clamp (atoi (get_tile "pop_aunits")) 0 4)
          cur-aupr  (clamp (atoi (get_tile "pop_auprec")) 0 8)
          ins-entry (nth (clamp (atoi (get_tile "pop_insunits")) 0 (1- (length *insunits-map*))) *insunits-map*)
          cur-ins   (if ins-entry (car ins-entry) 4)
          cur-meas  (atoi (get_tile "pop_meas"))
          cur-angd  (atoi (get_tile "pop_angd"))
          angb-idx  (atoi (get_tile "pop_angb"))
          cur-angb  (cond ((= angb-idx 1) 90.0) ((= angb-idx 2) 180.0) ((= angb-idx 3) 270.0) (t 0.0))
          cur-cur   (clamp (atoi (get_tile "eb_cur")) 1 100)
          cur-pb    (clamp (atoi (get_tile "eb_pb")) 1 50)
          cur-grp   (clamp (atoi (get_tile "eb_grp")) 1 50)
          cur-ap    (clamp (atoi (get_tile "eb_ap")) 1 50)
          cur-dy    (clamp (atoi (get_tile "pop_dyn")) 0 3)
          cur-sp    (clamp (atoi (get_tile "pop_sp")) 0 3)
          cur-drag  (clamp (atoi (get_tile "pop_drag")) 0 2)
          cur-snst  (clamp (atoi (get_tile "pop_snst")) 0 1)
          cur-gr    (atoi (get_tile "tog_grid"))
          cur-sn    (atoi (get_tile "tog_snap"))
          cur-orth  (atoi (get_tile "tog_ortho"))
          cur-lw    (atoi (get_tile "tog_lw"))
          cur-mirr  (atoi (get_tile "tog_mirr")))

    ;; Selection Options
    (cond
      ((= (get_tile "pop_selopt") "0") (setq cur-padd 0 cur-pfirst 1))
      ((= (get_tile "pop_selopt") "1") (setq cur-padd 2 cur-pfirst 1))
      ((= (get_tile "pop_selopt") "2") (setq cur-padd 1 cur-pfirst 1))
      (t                               (setq cur-padd 2 cur-pfirst 1))
    )

    ;; OSMODE Mapping
    (cond
      ((= (get_tile "pop_osm") "0") (setq target-osm 0))     ; All Off
      ((= (get_tile "pop_osm") "1") (setq target-osm 2215))  ; Standard 2D: End(1) + Mid(2) + Cen(4) + Int(32) + Perp(128) + Ext(2048) = 2215
      ((= (get_tile "pop_osm") "2") (setq target-osm 439))   ; Mechanical: End(1) + Mid(2) + Cen(4) + Quad(16) + Int(32) + Perp(128) + Tan(256) = 439
      ((= (get_tile "pop_osm") "3") (setq target-osm 2735))  ; Civil/Survey: End(1) + Mid(2) + Cen(4) + Node(8) + Int(32) + Perp(128) + Near(512) + Ext(2048) = 2735
      ((= (get_tile "pop_osm") "4") (setq target-osm 8191))  ; All Snaps Active
      ((= (get_tile "pop_osm") "5") (setq target-osm 16384)) ; Temporarily Suppressed (F3 Off)
      (t                            (setq target-osm orig-osm))
    )

    ;; Apply Safely
    (if (safe-setvar "LUNITS" cur-lun)                     (setq changed-count (1+ changed-count)))
    (if (safe-setvar "LUPREC" cur-lupr)                    (setq changed-count (1+ changed-count)))
    (if (safe-setvar "AUNITS" cur-aun)                     (setq changed-count (1+ changed-count)))
    (if (safe-setvar "AUPREC" cur-aupr)                    (setq changed-count (1+ changed-count)))
    (if (safe-setvar "INSUNITS" cur-ins)                   (setq changed-count (1+ changed-count)))
    (if (safe-setvar "MEASUREMENT" cur-meas)               (setq changed-count (1+ changed-count)))
    (if (safe-setvar "ANGDIR" cur-angd)                    (setq changed-count (1+ changed-count)))
    (if (safe-setvar "ANGBASE" cur-angb)                   (setq changed-count (1+ changed-count)))
    (if (safe-setvar "CURSORSIZE" cur-cur)                 (setq changed-count (1+ changed-count)))
    (if (safe-setvar "PICKBOX" cur-pb)                     (setq changed-count (1+ changed-count)))
    (if (safe-setvar "GRIPSIZE" cur-grp)                   (setq changed-count (1+ changed-count)))
    (if (safe-setvar "APERTURE" cur-ap)                    (setq changed-count (1+ changed-count)))
    (if (safe-setvar "PICKADD" cur-padd)                   (setq changed-count (1+ changed-count)))
    (if (safe-setvar "PICKFIRST" cur-pfirst)               (setq changed-count (1+ changed-count)))
    (if (safe-setvar "DYNMODE" cur-dy)                     (setq changed-count (1+ changed-count)))
    (if (safe-setvar "SELECTIONPREVIEW" cur-sp)            (setq changed-count (1+ changed-count)))
    (if (safe-setvar "DRAGMODE" cur-drag)                  (setq changed-count (1+ changed-count)))
    (if (safe-setvar "SNAPSTYLE" cur-snst)                 (setq changed-count (1+ changed-count)))
    (if (safe-setvar "GRIDMODE" cur-gr)                    (setq changed-count (1+ changed-count)))
    (if (safe-setvar "SNAPMODE" cur-sn)                    (setq changed-count (1+ changed-count)))
    (if (safe-setvar "ORTHOMODE" cur-orth)                 (setq changed-count (1+ changed-count)))
    (if (safe-setvar "LWDISPLAY" cur-lw)                   (setq changed-count (1+ changed-count)))
    (if (safe-setvar "MIRRTEXT" cur-mirr)                  (setq changed-count (1+ changed-count)))
    (if (and target-osm (safe-setvar "OSMODE" target-osm)) (setq changed-count (1+ changed-count)))

    (setq is-applied t)
    changed-count
  )

  ;; -------------------------------------------------------------------------
  ;; 8. HELP & INFO DIALOG
  ;; -------------------------------------------------------------------------
  (defun show-help-dialog (/ h-dcl h-fp h-id)
    (setq h-dcl (vl-filename-mktemp "cad_help.dcl"))
    (setq h-fp (open h-dcl "w"))
    (if h-fp
      (progn
        (write-line "cad_help_diag : dialog {" h-fp)
        (write-line "  label = \"About AutoCAD Settings Setup Manager\";" h-fp)
        (write-line "  fixed_width = true;" h-fp)
        (write-line "  width = 78;" h-fp)
        (write-line "  : boxed_column {" h-fp)
        (write-line "    label = \"Presets & Industry Profiles\";" h-fp)
        (write-line "    : text { label = \"• Arch (mm)     : Standard ISO Architectural (0 mm layout, INSUNITS=mm)\"; }" h-fp)
        (write-line "    : text { label = \"• Masterplan (m) : Metric Masterplanning & Site Layout (0.000 m, INSUNITS=m)\"; }" h-fp)
        (write-line "    : text { label = \"• Arch (ft-in)  : Imperial 1/16\\\" (INSUNITS=Inches, Ortho ON, 100% Crosshair)\"; }" h-fp)
        (write-line "    : text { label = \"• Struct (mm)   : Structural Steel & RC Detailing (0.0 mm, Grips/LW ON)\"; }" h-fp)
        (write-line "    : text { label = \"• Mech (mm)     : Mechanical 0.00 mm with Tangent & Quadrant Snaps (OSMODE=439)\"; }" h-fp)
        (write-line "    : text { label = \"• Civil / Surv  : Metric Meters 0.000, DMS Angles, North Azimuth & Node Snaps (2735)\"; }" h-fp)
        (write-line "    : text { label = \"• Electrical/MEP: Schematics & Diagrams, Snap/Grid Aligned, Ortho ON\"; }" h-fp)
        (write-line "    : text { label = \"• Iso 2.5D      : Isometric 2.5D Snap Grid (SNAPSTYLE=1), Snap ON, Ortho ON\"; }" h-fp)
        (write-line "    : text { label = \"• 100% Drafter  : Full-screen crosshairs with tuned 4K ergonomics & visibility\"; }" h-fp)
        (write-line "    : text { label = \"• Plot / Present: Clean Presentation Mode (Lineweights ON, Grid/Snaps OFF)\"; }" h-fp)
        (write-line "  }" h-fp)
        (write-line "  : spacer { height = 1; }" h-fp)
        (write-line "  : boxed_column {" h-fp)
        (write-line "    label = \"System Information & Hotkeys\";" h-fp)
        (write-line "    : text { label = \"• F3  : Toggle Object Snap (OSMODE)\"; }" h-fp)
        (write-line "    : text { label = \"• F7  : Toggle Background Grid Display (GRIDMODE)\"; }" h-fp)
        (write-line "    : text { label = \"• F8  : Toggle Orthographic Axis Constraint (ORTHOMODE)\"; }" h-fp)
        (write-line "    : text { label = \"• F9  : Toggle Snap to Grid Mode (SNAPMODE)\"; }" h-fp)
        (write-line "    : text { label = \"• F12 : Dynamic Input & Heads-Up Dimensioning (DYNMODE)\"; }" h-fp)
        (write-line "    : text { label = \"• LWT : Lineweight Display in Viewport (LWDISPLAY)\"; }" h-fp)
        (write-line "  }" h-fp)
        (write-line "  : spacer { height = 1; }" h-fp)
        (write-line "  : boxed_column {" h-fp)
        (write-line "    label = \"Live Preview Canvas Color Legend\";" h-fp)
        (write-line "    : text { label = \"• White Lines : Crosshairs (CURSORSIZE)\"; }" h-fp)
        (write-line "    : text { label = \"• Red Box     : Selection Pickbox (PICKBOX)\"; }" h-fp)
        (write-line "    : text { label = \"• Green Box   : Osnap Target Aperture (APERTURE)\"; }" h-fp)
        (write-line "    : text { label = \"• Cyan Nodes  : Grips on Selected Entities (GRIPSIZE)\"; }" h-fp)
        (write-line "    : text { label = \"• Yellow Box  : Object Snap Marker Target Indicator\"; }" h-fp)
        (write-line "  }" h-fp)
        (write-line "  : spacer { height = 1; }" h-fp)
        (write-line "  ok_only;" h-fp)
        (write-line "}" h-fp)
        (close h-fp)
        (setq h-fp nil)

        (setq h-id (load_dialog h-dcl))
        (if (and h-id (>= h-id 0) (new_dialog "cad_help_diag" h-id))
          (progn
            (start_dialog)
            (unload_dialog h-id)
          )
        )
        (if (and h-dcl (findfile h-dcl))
          (vl-file-delete h-dcl)
        )
      )
    )
  )

  ;; -------------------------------------------------------------------------
  ;; 9. READ CURRENT AUTOCAD ENVIRONMENT
  ;; -------------------------------------------------------------------------
  (setvar "CMDECHO" 0)

  (setq orig-lun   (safe-getvar "LUNITS" 2)
        orig-lupr  (safe-getvar "LUPREC" 4)
        orig-aun   (safe-getvar "AUNITS" 0)
        orig-aupr  (safe-getvar "AUPREC" 0)
        orig-ins   (safe-getvar "INSUNITS" 4)
        orig-meas  (safe-getvar "MEASUREMENT" 1)
        orig-angd  (safe-getvar "ANGDIR" 0)
        orig-angb  (safe-getvar "ANGBASE" 0.0)
        orig-cur   (safe-getvar "CURSORSIZE" 5)
        orig-pb    (safe-getvar "PICKBOX" 3)
        orig-grp   (safe-getvar "GRIPSIZE" 5)
        orig-ap    (safe-getvar "APERTURE" 10)
        orig-padd  (safe-getvar "PICKADD" 2)
        orig-pfirst (safe-getvar "PICKFIRST" 1)
        orig-high  (safe-getvar "HIGHLIGHT" 1)
        orig-dy    (abs (safe-getvar "DYNMODE" 3))
        orig-sp    (safe-getvar "SELECTIONPREVIEW" 3)
        orig-drag  (safe-getvar "DRAGMODE" 2)
        orig-snst  (safe-getvar "SNAPSTYLE" 0)
        orig-gr    (safe-getvar "GRIDMODE" 0)
        orig-sn    (safe-getvar "SNAPMODE" 0)
        orig-orth  (safe-getvar "ORTHOMODE" 0)
        orig-lw    (safe-getvar "LWDISPLAY" 0)
        orig-mirr  (safe-getvar "MIRRTEXT" 0)
        orig-osm   (safe-getvar "OSMODE" 47))

  ;; -------------------------------------------------------------------------
  ;; 10. GENERATE RICH DCL DIALOG SPECIFICATION
  ;; -------------------------------------------------------------------------
  (setq dcl-file (vl-filename-mktemp "acad_modern_settings.dcl"))
  (setq dcl-fp (open dcl-file "w"))

  (if (null dcl-fp)
    (progn
      (princ "\n[Error]: Unable to create temporary DCL definition file.")
      (exit)
    )
  )

  (write-line "cad_main_dialog : dialog {" dcl-fp)
  (write-line "  label = \"AutoCAD Major Settings & Drafting Setup Manager\";" dcl-fp)
  (write-line "  fixed_width = true;" dcl-fp)
  (write-line "  alignment = centered;" dcl-fp)
  (write-line "  : spacer { height = 1; }" dcl-fp)

  ;; Top Presets Bar
  (write-line "  : boxed_column {" dcl-fp)
  (write-line "    label = \"Quick Drafting Presets & Industry Profiles\";" dcl-fp)
  (write-line "    alignment = centered;" dcl-fp)
  
  ;; Row 1: Architectural & Structural
  (write-line "    : row {" dcl-fp)
  (write-line "      alignment = centered;" dcl-fp)
  (write-line "      : spacer { width = 1; }" dcl-fp)
  (write-line "      : button {" dcl-fp)
  (write-line "        key = \"btn_arch_mm\";" dcl-fp)
  (write-line "        label = \"Architectural (mm)\";" dcl-fp)
  (write-line "        width = 22;" dcl-fp)
  (write-line "        fixed_width = true;" dcl-fp)
  (write-line "        tooltip = \"Metric Millimeters, Precision 0 mm, INSUNITS=mm, Ortho ON, 100% Crosshair.\";" dcl-fp)
  (write-line "      }" dcl-fp)
  (write-line "      : button {" dcl-fp)
  (write-line "        key = \"btn_arch_m\";" dcl-fp)
  (write-line "        label = \"Arch Masterplan (m)\";" dcl-fp)
  (write-line "        width = 22;" dcl-fp)
  (write-line "        fixed_width = true;" dcl-fp)
  (write-line "        tooltip = \"Metric Meters, Precision 0.000 m, INSUNITS=Meters, Ortho ON, 100% Crosshair.\";" dcl-fp)
  (write-line "      }" dcl-fp)
  (write-line "      : button {" dcl-fp)
  (write-line "        key = \"btn_arch\";" dcl-fp)
  (write-line "        label = \"Architectural (ft-in)\";" dcl-fp)
  (write-line "        width = 22;" dcl-fp)
  (write-line "        fixed_width = true;" dcl-fp)
  (write-line "        tooltip = \"Feet & Fractional Inches (1/16\\\"), INSUNITS=Inches, Ortho ON, 100% Crosshair.\";" dcl-fp)
  (write-line "      }" dcl-fp)
  (write-line "      : button {" dcl-fp)
  (write-line "        key = \"btn_struct\";" dcl-fp)
  (write-line "        label = \"Structural (mm)\";" dcl-fp)
  (write-line "        width = 22;" dcl-fp)
  (write-line "        fixed_width = true;" dcl-fp)
  (write-line "        tooltip = \"Metric Millimeters, Precision 0.0 mm, High Visibility Grips, Lineweights ON.\";" dcl-fp)
  (write-line "      }" dcl-fp)
  (write-line "      : spacer { width = 1; }" dcl-fp)
  (write-line "    }" dcl-fp)
  
  (write-line "    : spacer { height = 1; }" dcl-fp)
  
  ;; Row 2: Engineering & Schematics
  (write-line "    : row {" dcl-fp)
  (write-line "      alignment = centered;" dcl-fp)
  (write-line "      : spacer { width = 1; }" dcl-fp)
  (write-line "      : button {" dcl-fp)
  (write-line "        key = \"btn_mech\";" dcl-fp)
  (write-line "        label = \"Mechanical (mm)\";" dcl-fp)
  (write-line "        width = 22;" dcl-fp)
  (write-line "        fixed_width = true;" dcl-fp)
  (write-line "        tooltip = \"Metric Millimeters, Precision 0.00, Tangent & Quadrant Snaps, Focused Cursor.\";" dcl-fp)
  (write-line "      }" dcl-fp)
  (write-line "      : button {" dcl-fp)
  (write-line "        key = \"btn_civil\";" dcl-fp)
  (write-line "        label = \"Civil / Survey (m)\";" dcl-fp)
  (write-line "        width = 22;" dcl-fp)
  (write-line "        fixed_width = true;" dcl-fp)
  (write-line "        tooltip = \"Metric Meters, 0.000 Precision, DMS Angles, North Azimuth, Node Snaps.\";" dcl-fp)
  (write-line "      }" dcl-fp)
  (write-line "      : button {" dcl-fp)
  (write-line "        key = \"btn_elec\";" dcl-fp)
  (write-line "        label = \"Electrical / MEP\";" dcl-fp)
  (write-line "        width = 22;" dcl-fp)
  (write-line "        fixed_width = true;" dcl-fp)
  (write-line "        tooltip = \"Metric mm, Grid & Snap Alignment, Ortho ON, Readable Text (MIRRTEXT=0).\";" dcl-fp)
  (write-line "      }" dcl-fp)
  (write-line "      : button {" dcl-fp)
  (write-line "        key = \"btn_iso\";" dcl-fp)
  (write-line "        label = \"Isometric 2.5D\";" dcl-fp)
  (write-line "        width = 22;" dcl-fp)
  (write-line "        fixed_width = true;" dcl-fp)
  (write-line "        tooltip = \"Isometric Grid Snap (SNAPSTYLE=1), Snap ON, Ortho ON, 100% Crosshair.\";" dcl-fp)
  (write-line "      }" dcl-fp)
  (write-line "      : spacer { width = 1; }" dcl-fp)
  (write-line "    }" dcl-fp)
  
  (write-line "    : spacer { height = 1; }" dcl-fp)
  
  ;; Row 3: Ergonomics & Utilities
  (write-line "    : row {" dcl-fp)
  (write-line "      alignment = centered;" dcl-fp)
  (write-line "      : spacer { width = 1; }" dcl-fp)
  (write-line "      : button {" dcl-fp)
  (write-line "        key = \"btn_cross\";" dcl-fp)
  (write-line "        label = \"100% Pro Drafter\";" dcl-fp)
  (write-line "        width = 22;" dcl-fp)
  (write-line "        fixed_width = true;" dcl-fp)
  (write-line "        tooltip = \"Full-Screen Crosshairs, tuned pickbox, grips & aperture for 4K / power drafting.\";" dcl-fp)
  (write-line "      }" dcl-fp)
  (write-line "      : button {" dcl-fp)
  (write-line "        key = \"btn_plot\";" dcl-fp)
  (write-line "        label = \"Plot / Present\";" dcl-fp)
  (write-line "        width = 22;" dcl-fp)
  (write-line "        fixed_width = true;" dcl-fp)
  (write-line "        tooltip = \"Lineweights ON, Grid OFF, Snaps OFF, Clean Viewport Presentation.\";" dcl-fp)
  (write-line "      }" dcl-fp)
  (write-line "      : button {" dcl-fp)
  (write-line "        key = \"btn_default\";" dcl-fp)
  (write-line "        label = \"Factory Default\";" dcl-fp)
  (write-line "        width = 22;" dcl-fp)
  (write-line "        fixed_width = true;" dcl-fp)
  (write-line "        tooltip = \"Standard AutoCAD out-of-the-box factory defaults.\";" dcl-fp)
  (write-line "      }" dcl-fp)
  (write-line "      : button {" dcl-fp)
  (write-line "        key = \"btn_reset_orig\";" dcl-fp)
  (write-line "        label = \"Current Drawing\";" dcl-fp)
  (write-line "        width = 22;" dcl-fp)
  (write-line "        fixed_width = true;" dcl-fp)
  (write-line "        tooltip = \"Revert all controls to the drawing's initial configuration.\";" dcl-fp)
  (write-line "      }" dcl-fp)
  (write-line "      : spacer { width = 1; }" dcl-fp)
  (write-line "    }" dcl-fp)
  (write-line "  }" dcl-fp)

  (write-line "  : spacer { height = 1; }" dcl-fp)

  ;; Main Three Columns Layout
  (write-line "  : row {" dcl-fp)

  ;; COLUMN 1: Units & Coordinates
  (write-line "    : column {" dcl-fp)
  (write-line "      width = 46;" dcl-fp)
  (write-line "      : boxed_column {" dcl-fp)
  (write-line "        label = \"Units & Coordinate Formatting\";" dcl-fp)
  
  (write-line "        : row {" dcl-fp)
  (write-line "          : text {" dcl-fp)
  (write-line "            label = \"Linear Units:\";" dcl-fp)
  (write-line "            width = 18;" dcl-fp)
  (write-line "            alignment = left;" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "          : spacer { width = 1; }" dcl-fp)
  (write-line "          : popup_list {" dcl-fp)
  (write-line "            key = \"pop_lunits\";" dcl-fp)
  (write-line "            width = 25;" dcl-fp)
  (write-line "            fixed_width = true;" dcl-fp)
  (write-line "            tooltip = \"[LUNITS] 1:Scientific, 2:Decimal, 3:Engineering, 4:Architectural, 5:Fractional.\";" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "        }" dcl-fp)

  (write-line "        : spacer { height = 1; }" dcl-fp)
  (write-line "        : row {" dcl-fp)
  (write-line "          : text {" dcl-fp)
  (write-line "            label = \"Linear Precision:\";" dcl-fp)
  (write-line "            width = 18;" dcl-fp)
  (write-line "            alignment = left;" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "          : spacer { width = 1; }" dcl-fp)
  (write-line "          : popup_list {" dcl-fp)
  (write-line "            key = \"pop_luprec\";" dcl-fp)
  (write-line "            width = 25;" dcl-fp)
  (write-line "            fixed_width = true;" dcl-fp)
  (write-line "            tooltip = \"[LUPREC] Decimal places or fraction denominator resolution.\";" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "        }" dcl-fp)

  (write-line "        : spacer { height = 1; }" dcl-fp)
  (write-line "        : row {" dcl-fp)
  (write-line "          : text {" dcl-fp)
  (write-line "            label = \"Angular Units:\";" dcl-fp)
  (write-line "            width = 18;" dcl-fp)
  (write-line "            alignment = left;" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "          : spacer { width = 1; }" dcl-fp)
  (write-line "          : popup_list {" dcl-fp)
  (write-line "            key = \"pop_aunits\";" dcl-fp)
  (write-line "            width = 25;" dcl-fp)
  (write-line "            fixed_width = true;" dcl-fp)
  (write-line "            tooltip = \"[AUNITS] 0:Decimal Deg, 1:Deg/Min/Sec, 2:Grads, 3:Radians, 4:Surveyor.\";" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "        }" dcl-fp)

  (write-line "        : spacer { height = 1; }" dcl-fp)
  (write-line "        : row {" dcl-fp)
  (write-line "          : text {" dcl-fp)
  (write-line "            label = \"Angular Precision:\";" dcl-fp)
  (write-line "            width = 18;" dcl-fp)
  (write-line "            alignment = left;" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "          : spacer { width = 1; }" dcl-fp)
  (write-line "          : popup_list {" dcl-fp)
  (write-line "            key = \"pop_auprec\";" dcl-fp)
  (write-line "            width = 25;" dcl-fp)
  (write-line "            fixed_width = true;" dcl-fp)
  (write-line "            tooltip = \"[AUPREC] Angular decimal display precision (0 to 8).\";" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "        }" dcl-fp)

  (write-line "        : spacer { height = 1; }" dcl-fp)
  (write-line "        : row {" dcl-fp)
  (write-line "          : text {" dcl-fp)
  (write-line "            label = \"Insertion Scale:\";" dcl-fp)
  (write-line "            width = 18;" dcl-fp)
  (write-line "            alignment = left;" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "          : spacer { width = 1; }" dcl-fp)
  (write-line "          : popup_list {" dcl-fp)
  (write-line "            key = \"pop_insunits\";" dcl-fp)
  (write-line "            width = 25;" dcl-fp)
  (write-line "            fixed_width = true;" dcl-fp)
  (write-line "            tooltip = \"[INSUNITS] Standard unit used when inserting blocks, xrefs, and copy-pasting.\";" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "        }" dcl-fp)

  (write-line "        : spacer { height = 1; }" dcl-fp)
  (write-line "        : row {" dcl-fp)
  (write-line "          : text {" dcl-fp)
  (write-line "            label = \"Measurement Std:\";" dcl-fp)
  (write-line "            width = 18;" dcl-fp)
  (write-line "            alignment = left;" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "          : spacer { width = 1; }" dcl-fp)
  (write-line "          : popup_list {" dcl-fp)
  (write-line "            key = \"pop_meas\";" dcl-fp)
  (write-line "            width = 25;" dcl-fp)
  (write-line "            fixed_width = true;" dcl-fp)
  (write-line "            tooltip = \"[MEASUREMENT] 0: Imperial (ANSI/Inches), 1: Metric (ISO/Millimeters).\";" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "        }" dcl-fp)

  (write-line "        : spacer { height = 1; }" dcl-fp)
  (write-line "        : row {" dcl-fp)
  (write-line "          : text {" dcl-fp)
  (write-line "            label = \"Angle Direction:\";" dcl-fp)
  (write-line "            width = 18;" dcl-fp)
  (write-line "            alignment = left;" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "          : spacer { width = 1; }" dcl-fp)
  (write-line "          : popup_list {" dcl-fp)
  (write-line "            key = \"pop_angd\";" dcl-fp)
  (write-line "            width = 25;" dcl-fp)
  (write-line "            fixed_width = true;" dcl-fp)
  (write-line "            tooltip = \"[ANGDIR] Positive angle direction: Counter-Clockwise (Standard) or Clockwise.\";" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "        }" dcl-fp)

  (write-line "        : spacer { height = 1; }" dcl-fp)
  (write-line "        : row {" dcl-fp)
  (write-line "          : text {" dcl-fp)
  (write-line "            label = \"Angle Base (0°):\";" dcl-fp)
  (write-line "            width = 18;" dcl-fp)
  (write-line "            alignment = left;" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "          : spacer { width = 1; }" dcl-fp)
  (write-line "          : popup_list {" dcl-fp)
  (write-line "            key = \"pop_angb\";" dcl-fp)
  (write-line "            width = 25;" dcl-fp)
  (write-line "            fixed_width = true;" dcl-fp)
  (write-line "            tooltip = \"[ANGBASE] Zero angle azimuth: East (0°), North (90° - Surveying), West (180°), South (270°).\";" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "        }" dcl-fp)

  (write-line "      }" dcl-fp)
  (write-line "    }" dcl-fp)

  ;; COLUMN 2: Cursor, Sizing & Real-Time Live Preview
  (write-line "    : column {" dcl-fp)
  (write-line "      width = 46;" dcl-fp)
  (write-line "      : boxed_column {" dcl-fp)
  (write-line "        label = \"Interactive Cursor & Sizing Controls\";" dcl-fp)
  
  ;; Crosshair Size
  (write-line "        : row {" dcl-fp)
  (write-line "          : text {" dcl-fp)
  (write-line "            label = \"Crosshair (1-100%):\";" dcl-fp)
  (write-line "            width = 18;" dcl-fp)
  (write-line "            alignment = left;" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "          : edit_box {" dcl-fp)
  (write-line "            key = \"eb_cur\";" dcl-fp)
  (write-line "            edit_width = 4;" dcl-fp)
  (write-line "            fixed_width = true;" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "          : spacer { width = 1; }" dcl-fp)
  (write-line "          : slider {" dcl-fp)
  (write-line "            key = \"sld_cur\";" dcl-fp)
  (write-line "            min_value = 1;" dcl-fp)
  (write-line "            max_value = 100;" dcl-fp)
  (write-line "            small_increment = 1;" dcl-fp)
  (write-line "            big_increment = 10;" dcl-fp)
  (write-line "            width = 18;" dcl-fp)
  (write-line "            fixed_width = true;" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "        }" dcl-fp)

  (write-line "        : spacer { height = 1; }" dcl-fp)

  ;; Pickbox Size
  (write-line "        : row {" dcl-fp)
  (write-line "          : text {" dcl-fp)
  (write-line "            label = \"Pickbox (1-50 px):\";" dcl-fp)
  (write-line "            width = 18;" dcl-fp)
  (write-line "            alignment = left;" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "          : edit_box {" dcl-fp)
  (write-line "            key = \"eb_pb\";" dcl-fp)
  (write-line "            edit_width = 4;" dcl-fp)
  (write-line "            fixed_width = true;" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "          : spacer { width = 1; }" dcl-fp)
  (write-line "          : slider {" dcl-fp)
  (write-line "            key = \"sld_pb\";" dcl-fp)
  (write-line "            min_value = 1;" dcl-fp)
  (write-line "            max_value = 50;" dcl-fp)
  (write-line "            small_increment = 1;" dcl-fp)
  (write-line "            big_increment = 5;" dcl-fp)
  (write-line "            width = 18;" dcl-fp)
  (write-line "            fixed_width = true;" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "        }" dcl-fp)

  (write-line "        : spacer { height = 1; }" dcl-fp)

  ;; Grip Size
  (write-line "        : row {" dcl-fp)
  (write-line "          : text {" dcl-fp)
  (write-line "            label = \"Grip Size (1-50 px):\";" dcl-fp)
  (write-line "            width = 18;" dcl-fp)
  (write-line "            alignment = left;" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "          : edit_box {" dcl-fp)
  (write-line "            key = \"eb_grp\";" dcl-fp)
  (write-line "            edit_width = 4;" dcl-fp)
  (write-line "            fixed_width = true;" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "          : spacer { width = 1; }" dcl-fp)
  (write-line "          : slider {" dcl-fp)
  (write-line "            key = \"sld_grp\";" dcl-fp)
  (write-line "            min_value = 1;" dcl-fp)
  (write-line "            max_value = 50;" dcl-fp)
  (write-line "            small_increment = 1;" dcl-fp)
  (write-line "            big_increment = 5;" dcl-fp)
  (write-line "            width = 18;" dcl-fp)
  (write-line "            fixed_width = true;" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "        }" dcl-fp)

  (write-line "        : spacer { height = 1; }" dcl-fp)

  ;; Aperture Size
  (write-line "        : row {" dcl-fp)
  (write-line "          : text {" dcl-fp)
  (write-line "            label = \"Aperture (1-50 px):\";" dcl-fp)
  (write-line "            width = 18;" dcl-fp)
  (write-line "            alignment = left;" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "          : edit_box {" dcl-fp)
  (write-line "            key = \"eb_ap\";" dcl-fp)
  (write-line "            edit_width = 4;" dcl-fp)
  (write-line "            fixed_width = true;" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "          : spacer { width = 1; }" dcl-fp)
  (write-line "          : slider {" dcl-fp)
  (write-line "            key = \"sld_ap\";" dcl-fp)
  (write-line "            min_value = 1;" dcl-fp)
  (write-line "            max_value = 50;" dcl-fp)
  (write-line "            small_increment = 1;" dcl-fp)
  (write-line "            big_increment = 5;" dcl-fp)
  (write-line "            width = 18;" dcl-fp)
  (write-line "            fixed_width = true;" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "        }" dcl-fp)

  ;; Live Preview Tile
  (write-line "        : spacer { height = 1; }" dcl-fp)
  (write-line "        : boxed_column {" dcl-fp)
  (write-line "          label = \"Real-Time Visual Preview Canvas\";" dcl-fp)
  (write-line "          alignment = centered;" dcl-fp)
  (write-line "          : image {" dcl-fp)
  (write-line "            key = \"img_preview\";" dcl-fp)
  (write-line "            width = 42;" dcl-fp)
  (write-line "            height = 9;" dcl-fp)
  (write-line "            fixed_width = true;" dcl-fp)
  (write-line "            fixed_height = true;" dcl-fp)
  (write-line "            color = 250;" dcl-fp)
  (write-line "            alignment = centered;" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "          : spacer { height = 1; }" dcl-fp)
  (write-line "          : text {" dcl-fp)
  (write-line "            label = \"White=Crosshair  |  Red=Pickbox  |  Green=Aperture  |  Cyan=Grips\";" dcl-fp)
  (write-line "            alignment = centered;" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "        }" dcl-fp)
  (write-line "      }" dcl-fp)
  (write-line "    }" dcl-fp)

  ;; COLUMN 3: Drafting Aids, Selection & Snapping
  (write-line "    : column {" dcl-fp)
  (write-line "      width = 46;" dcl-fp)
  (write-line "      : boxed_column {" dcl-fp)
  (write-line "        label = \"Drafting Aids & Snapping\";" dcl-fp)
  
  (write-line "        : row {" dcl-fp)
  (write-line "          : text {" dcl-fp)
  (write-line "            label = \"Dynamic Input:\";" dcl-fp)
  (write-line "            width = 18;" dcl-fp)
  (write-line "            alignment = left;" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "          : spacer { width = 1; }" dcl-fp)
  (write-line "          : popup_list {" dcl-fp)
  (write-line "            key = \"pop_dyn\";" dcl-fp)
  (write-line "            width = 25;" dcl-fp)
  (write-line "            fixed_width = true;" dcl-fp)
  (write-line "            tooltip = \"[DYNMODE] 0:Off, 1:Pointer, 2:Dimensional, 3:Pointer & Dimensional (Full).\";" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "        }" dcl-fp)

  (write-line "        : spacer { height = 1; }" dcl-fp)
  (write-line "        : row {" dcl-fp)
  (write-line "          : text {" dcl-fp)
  (write-line "            label = \"Selection Preview:\";" dcl-fp)
  (write-line "            width = 18;" dcl-fp)
  (write-line "            alignment = left;" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "          : spacer { width = 1; }" dcl-fp)
  (write-line "          : popup_list {" dcl-fp)
  (write-line "            key = \"pop_sp\";" dcl-fp)
  (write-line "            width = 25;" dcl-fp)
  (write-line "            fixed_width = true;" dcl-fp)
  (write-line "            tooltip = \"[SELECTIONPREVIEW] 0:Off, 1:When No Command, 2:During Command, 3:Always Active.\";" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "        }" dcl-fp)

  (write-line "        : spacer { height = 1; }" dcl-fp)
  (write-line "        : row {" dcl-fp)
  (write-line "          : text {" dcl-fp)
  (write-line "            label = \"Drag Dynamics:\";" dcl-fp)
  (write-line "            width = 18;" dcl-fp)
  (write-line "            alignment = left;" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "          : spacer { width = 1; }" dcl-fp)
  (write-line "          : popup_list {" dcl-fp)
  (write-line "            key = \"pop_drag\";" dcl-fp)
  (write-line "            width = 25;" dcl-fp)
  (write-line "            fixed_width = true;" dcl-fp)
  (write-line "            tooltip = \"[DRAGMODE] 0:Off, 1:On Demand, 2:Auto Continuous Dragging.\";" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "        }" dcl-fp)

  (write-line "        : spacer { height = 1; }" dcl-fp)
  (write-line "        : row {" dcl-fp)
  (write-line "          : text {" dcl-fp)
  (write-line "            label = \"Snap Grid Style:\";" dcl-fp)
  (write-line "            width = 18;" dcl-fp)
  (write-line "            alignment = left;" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "          : spacer { width = 1; }" dcl-fp)
  (write-line "          : popup_list {" dcl-fp)
  (write-line "            key = \"pop_snst\";" dcl-fp)
  (write-line "            width = 25;" dcl-fp)
  (write-line "            fixed_width = true;" dcl-fp)
  (write-line "            tooltip = \"[SNAPSTYLE] 0:Standard Rectangular, 1:Isometric Snap Grid.\";" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "        }" dcl-fp)

  (write-line "        : spacer { height = 1; }" dcl-fp)
  (write-line "        : row {" dcl-fp)
  (write-line "          : text {" dcl-fp)
  (write-line "            label = \"Selection Mode:\";" dcl-fp)
  (write-line "            width = 18;" dcl-fp)
  (write-line "            alignment = left;" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "          : spacer { width = 1; }" dcl-fp)
  (write-line "          : popup_list {" dcl-fp)
  (write-line "            key = \"pop_selopt\";" dcl-fp)
  (write-line "            width = 25;" dcl-fp)
  (write-line "            fixed_width = true;" dcl-fp)
  (write-line "            tooltip = \"[PICKADD/PICKFIRST] Additive selection vs single-selection replace.\";" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "        }" dcl-fp)

  (write-line "        : spacer { height = 1; }" dcl-fp)
  (write-line "        : row {" dcl-fp)
  (write-line "          : text {" dcl-fp)
  (write-line "            label = \"Object Snap (F3):\";" dcl-fp)
  (write-line "            width = 18;" dcl-fp)
  (write-line "            alignment = left;" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "          : spacer { width = 1; }" dcl-fp)
  (write-line "          : popup_list {" dcl-fp)
  (write-line "            key = \"pop_osm\";" dcl-fp)
  (write-line "            width = 25;" dcl-fp)
  (write-line "            fixed_width = true;" dcl-fp)
  (write-line "            tooltip = \"[OSMODE] Quick presets for 2D drafting object snaps.\";" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "        }" dcl-fp)

  (write-line "        : spacer { height = 1; }" dcl-fp)
  (write-line "        : boxed_column {" dcl-fp)
  (write-line "          label = \"Quick Status Toggles\";" dcl-fp)
  (write-line "          : toggle {" dcl-fp)
  (write-line "            key = \"tog_grid\";" dcl-fp)
  (write-line "            label = \"  Display Drawing Grid (F7 / GRIDMODE)\";" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "          : toggle {" dcl-fp)
  (write-line "            key = \"tog_snap\";" dcl-fp)
  (write-line "            label = \"  Lock Cursor to Grid Snap (F9 / SNAPMODE)\";" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "          : toggle {" dcl-fp)
  (write-line "            key = \"tog_ortho\";" dcl-fp)
  (write-line "            label = \"  Ortho Mode 90° Constraint (F8 / ORTHO)\";" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "          : toggle {" dcl-fp)
  (write-line "            key = \"tog_lw\";" dcl-fp)
  (write-line "            label = \"  Display Entity Lineweights (LWDISPLAY)\";" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "          : toggle {" dcl-fp)
  (write-line "            key = \"tog_mirr\";" dcl-fp)
  (write-line "            label = \"  Mirror Text Upside Down (MIRRTEXT)\";" dcl-fp)
  (write-line "          }" dcl-fp)
  (write-line "        }" dcl-fp)
  (write-line "      }" dcl-fp)
  (write-line "    }" dcl-fp)
  (write-line "  }" dcl-fp)

  ;; Bottom Status / Hint Bar
  (write-line "  : spacer { height = 1; }" dcl-fp)
  (write-line "  : boxed_row {" dcl-fp)
  (write-line "    : text {" dcl-fp)
  (write-line "      key = \"txt_status\";" dcl-fp)
  (write-line "      width = 85;" dcl-fp)
  (write-line "      alignment = left;" dcl-fp)
  (write-line "    }" dcl-fp)
  (write-line "  }" dcl-fp)

  (write-line "  : spacer { height = 1; }" dcl-fp)

  ;; Bottom Action Buttons
  (write-line "  : row {" dcl-fp)
  (write-line "    alignment = right;" dcl-fp)
  (write-line "    : button {" dcl-fp)
  (write-line "      key = \"btn_help\";" dcl-fp)
  (write-line "      label = \"&Help / Shortcuts\";" dcl-fp)
  (write-line "      width = 18;" dcl-fp)
  (write-line "      fixed_width = true;" dcl-fp)
  (write-line "    }" dcl-fp)
  (write-line "    : spacer { width = 4; }" dcl-fp)
  (write-line "    : button {" dcl-fp)
  (write-line "      key = \"btn_apply\";" dcl-fp)
  (write-line "      label = \"&Apply Now\";" dcl-fp)
  (write-line "      width = 14;" dcl-fp)
  (write-line "      fixed_width = true;" dcl-fp)
  (write-line "    }" dcl-fp)
  (write-line "    : button {" dcl-fp)
  (write-line "      key = \"accept\";" dcl-fp)
  (write-line "      label = \"&OK\";" dcl-fp)
  (write-line "      is_default = true;" dcl-fp)
  (write-line "      width = 12;" dcl-fp)
  (write-line "      fixed_width = true;" dcl-fp)
  (write-line "    }" dcl-fp)
  (write-line "    : button {" dcl-fp)
  (write-line "      key = \"cancel\";" dcl-fp)
  (write-line "      label = \"&Cancel\";" dcl-fp)
  (write-line "      is_cancel = true;" dcl-fp)
  (write-line "      width = 12;" dcl-fp)
  (write-line "      fixed_width = true;" dcl-fp)
  (write-line "    }" dcl-fp)
  (write-line "  }" dcl-fp)

  (write-line "}" dcl-fp)
  (close dcl-fp)
  (setq dcl-fp nil)

  ;; -------------------------------------------------------------------------
  ;; 11. LOAD AND INITIALIZE DIALOG
  ;; -------------------------------------------------------------------------
  (setq dcl-id (load_dialog dcl-file))
  (if (or (not dcl-id) (< dcl-id 0) (not (new_dialog "cad_main_dialog" dcl-id)))
    (progn
      (if (and dcl-file (findfile dcl-file)) (vl-file-delete dcl-file))
      (princ "\n[Error]: Unable to load dialog interface.")
      (exit)
    )
  )

  ;; -------------------------------------------------------------------------
  ;; 12. POPULATE DROPDOWNS & LIST CHOICES
  ;; -------------------------------------------------------------------------
  ;; Linear Units
  (start_list "pop_lunits")
  (mapcar 'add_list '("1. Scientific" "2. Decimal (Standard)" "3. Engineering (Feet/Inches)" "4. Architectural (Feet/Fraction)" "5. Fractional"))
  (end_list)

  ;; Linear Precision
  (start_list "pop_luprec")
  (mapcar 'add_list '("0 (0)" "1 (0.0)" "2 (0.00)" "3 (0.000)" "4 (0.0000)" "5 (0.00000)" "6 (0.000000)" "7 (0.0000000)" "8 (0.00000000)"))
  (end_list)

  ;; Angular Units
  (start_list "pop_aunits")
  (mapcar 'add_list '("0. Decimal Degrees" "1. Deg/Min/Sec" "2. Grads" "3. Radians" "4. Surveyor's Units"))
  (end_list)

  ;; Angular Precision
  (start_list "pop_auprec")
  (mapcar 'add_list '("0 (0)" "1 (0.0)" "2 (0.00)" "3 (0.000)" "4 (0.0000)" "5 (0.00000)" "6 (0.000000)" "7 (0.0000000)" "8 (0.00000000)"))
  (end_list)

  ;; Insertion Units
  (start_list "pop_insunits")
  (mapcar '(lambda (item) (add_list (cadr item))) *insunits-map*)
  (end_list)

  ;; Drawing Standard (Measurement)
  (start_list "pop_meas")
  (mapcar 'add_list '("0. Imperial (Inches / Feet)" "1. Metric (Millimeters / ISO)"))
  (end_list)

  ;; Angle Direction
  (start_list "pop_angd")
  (mapcar 'add_list '("0. Counter-Clockwise (Standard CAD)" "1. Clockwise (Surveying / Compass)"))
  (end_list)

  ;; Base Angle Orientation
  (start_list "pop_angb")
  (mapcar 'add_list '("0° East (Standard 3 O'Clock)" "90° North (Surveying / Compass)" "180° West" "270° South"))
  (end_list)

  ;; Dynamic Input
  (start_list "pop_dyn")
  (mapcar 'add_list '("0. Off" "1. Pointer Input Only" "2. Dimensional Input Only" "3. Pointer & Dimensional (Full)"))
  (end_list)

  ;; Selection Preview
  (start_list "pop_sp")
  (mapcar 'add_list '("0. Off" "1. Active When No Command" "2. Active During Command" "3. Always Active (Full Highlight)"))
  (end_list)

  ;; Dragging Mode
  (start_list "pop_drag")
  (mapcar 'add_list '("0. Off (No dynamic drag)" "1. On Request (Only when asked)" "2. Auto (Continuous live dragging)"))
  (end_list)

  ;; Snap Grid Style
  (start_list "pop_snst")
  (mapcar 'add_list '("0. Standard 2D Rectangular Grid" "1. Isometric 2.5D Snap Grid"))
  (end_list)

  ;; Selection Behavior
  (start_list "pop_selopt")
  (mapcar 'add_list '("0. Replace Selection (Shift to Add)" "1. Additive Selection (Standard PICKADD=2)" "2. Legacy Accumulate (PICKADD=1)"))
  (end_list)

  ;; Osnap Preset
  (start_list "pop_osm")
  (mapcar 'add_list '(
    "0. Disable All Snaps (F3 Off)"
    "1. Standard 2D (End/Mid/Cen/Int/Perp/Ext - 2215)"
    "2. Mechanical Suite (Standard + Quad/Tan - 439)"
    "3. Civil / Survey Suite (Standard + Node/Near - 2735)"
    "4. Full Drafting Suite (All Snaps Active - 8191)"
    "5. Suppressed Snap Tracking (Bit 16384)"
  ))
  (end_list)

  ;; Populate Initial States
  (populate-dialog-controls)
  (update-cursor-preview 
    (get_tile "eb_cur") 
    (get_tile "eb_pb") 
    (get_tile "eb_grp") 
    (get_tile "eb_ap") 
    (get_tile "tog_grid")
  )

  ;; -------------------------------------------------------------------------
  ;; 13. DYNAMIC EVENT CALLBACKS
  ;; -------------------------------------------------------------------------
  ;; Preset Buttons
  (action_tile "btn_arch_mm"    "(apply-preset \"ARCH_MM\")")
  (action_tile "btn_arch_m"     "(apply-preset \"ARCH_M\")")
  (action_tile "btn_arch"       "(apply-preset \"ARCH\")")
  (action_tile "btn_struct"     "(apply-preset \"STRUCT_MM\")")
  (action_tile "btn_mech"       "(apply-preset \"MECH_MM\")")
  (action_tile "btn_civil"      "(apply-preset \"CIVIL_M\")")
  (action_tile "btn_elec"       "(apply-preset \"ELEC_MEP\")")
  (action_tile "btn_iso"        "(apply-preset \"ISO_25D\")")
  (action_tile "btn_cross"      "(apply-preset \"FULL_CROSS\")")
  (action_tile "btn_plot"       "(apply-preset \"PRESENT_PLOT\")")
  (action_tile "btn_default"    "(apply-preset \"DEFAULT\")")
  (action_tile "btn_reset_orig" "(apply-preset \"RESET_ORIG\")")

  ;; Synchronized Sliders & Edit Boxes
  (action_tile "sld_cur" "(sync-slider-to-eb \"sld_cur\" \"eb_cur\" 1 100)")
  (action_tile "eb_cur"  "(sync-eb-to-slider \"eb_cur\" \"sld_cur\" 1 100)")

  (action_tile "sld_pb"  "(sync-slider-to-eb \"sld_pb\" \"eb_pb\" 1 50)")
  (action_tile "eb_pb"   "(sync-eb-to-slider \"eb_pb\" \"sld_pb\" 1 50)")

  (action_tile "sld_grp" "(sync-slider-to-eb \"sld_grp\" \"eb_grp\" 1 50)")
  (action_tile "eb_grp"  "(sync-eb-to-slider \"eb_grp\" \"sld_grp\" 1 50)")

  (action_tile "sld_ap"  "(sync-slider-to-eb \"sld_ap\" \"eb_ap\" 1 50)")
  (action_tile "eb_ap"   "(sync-eb-to-slider \"eb_ap\" \"sld_ap\" 1 50)")

  ;; Toggle Redraws
  (action_tile "tog_grid" "(update-cursor-preview (get_tile \"eb_cur\") (get_tile \"eb_pb\") (get_tile \"eb_grp\") (get_tile \"eb_ap\") $value)")

  ;; Help & Actions
  (action_tile "btn_help" "(show-help-dialog)")
  (action_tile "btn_apply" 
    "(progn 
       (setq count (apply-all-settings)) 
       (set_tile \"txt_status\" (strcat \"[Applied]: Updated \" (itoa count) \" system variables successfully.\"))
     )"
  )

  (action_tile "accept" 
    "(progn 
       (apply-all-settings) 
       (done_dialog 1)
     )"
  )

  (action_tile "cancel" "(done_dialog 0)")

  ;; -------------------------------------------------------------------------
  ;; 14. RUN DIALOG & FINAL STATUS REPORT
  ;; -------------------------------------------------------------------------
  (setq act (start_dialog))
  (unload_dialog dcl-id)

  (if (and dcl-file (findfile dcl-file))
    (vl-file-delete dcl-file)
  )

  (setvar "CMDECHO" 1)

  (if (= act 1)
    (progn
      (princ "\n==========================================================================")
      (princ "\n[AutoCAD Setup Manager]: Settings successfully applied to current drawing:")
      (princ (strcat "\n • Linear Units      : " (vl-princ-to-string (getvar "LUNITS")) " (Precision: " (vl-princ-to-string (getvar "LUPREC")) ")"))
      (princ (strcat "\n • Angular Units     : " (vl-princ-to-string (getvar "AUNITS")) " (Precision: " (vl-princ-to-string (getvar "AUPREC")) ")"))
      (princ (strcat "\n • Insertion Units   : " (vl-princ-to-string (getvar "INSUNITS"))))
      (princ (strcat "\n • Measurement Std   : " (if (= (getvar "MEASUREMENT") 1) "Metric (ISO)" "Imperial (ANSI)")))
      (princ (strcat "\n • Crosshair Size    : " (vl-princ-to-string (getvar "CURSORSIZE")) "%"))
      (princ (strcat "\n • Pickbox / Grip/ Ap: " (vl-princ-to-string (getvar "PICKBOX")) " px / " (vl-princ-to-string (getvar "GRIPSIZE")) " px / " (vl-princ-to-string (getvar "APERTURE")) " px"))
      (princ (strcat "\n • Dynamic Input     : Mode " (vl-princ-to-string (getvar "DYNMODE"))))
      (princ (strcat "\n • Drafting Toggles  : Grid=" (vl-princ-to-string (getvar "GRIDMODE")) " | Snap=" (vl-princ-to-string (getvar "SNAPMODE")) " | Ortho=" (vl-princ-to-string (getvar "ORTHOMODE")) " | LWT=" (vl-princ-to-string (getvar "LWDISPLAY"))))
      (princ "\n==========================================================================")
    )
    (if is-applied
      (princ "\n[AutoCAD Setup Manager]: Closed. Changes made via 'Apply Now' remain in effect.")
      (princ "\n[AutoCAD Setup Manager]: Cancelled. No changes made.")
    )
  )

  (princ)
)

;; ---------------------------------------------------------------------------
;; 15. COMMAND ALIASES
;; ---------------------------------------------------------------------------
(defun c:CADSETTINGS () (c:CAD-SETTINGS))

(princ "\n[Loaded]: AutoCAD Settings Manager 2.0. Type CAD-SETTINGS or CADSETTINGS.")
(princ)