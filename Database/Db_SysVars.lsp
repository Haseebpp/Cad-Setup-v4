;;; ==========================================================================
;;; Db_SysVars.lsp - Recommended System Variables & Drawing States Dictionary
;;; Layer: Database (Level 2 - Pure Data)
;;; ==========================================================================

;; ===========================================================================
;; MASTER SYSTEM VARIABLE SPECIFICATION TABLE
;; Format:
;; (
;;   0: Variable Name (string)
;;   1: Recommended Target Value (integer, real, or string)
;;   2: Category (string)
;;   3: Description / Rationale (string)
;; )
;; ===========================================================================

(setq *CadSetup-SysVars-Data* '(
  ;; -------------------------------------------------------------------------
  ;; 1. Drawing Units & Insertion Scale (Metric ISO Standards)
  ;; -------------------------------------------------------------------------
  ("INSUNITS"           4          "Units"       "Millimeters - Prevents scaling chaos on XREF insert")
  ("MEASUREMENT"        1          "Units"       "Metric standards (loads acadiso.lin & acadiso.pat)")
  ("LUNITS"             2          "Units"       "Decimal units format")
  ("LUPREC"             1          "Units"       "Linear display precision (0.0 mm)")
  ("AUNITS"             0          "Units"       "Decimal degrees angular format")
  ("AUPREC"             2          "Units"       "Angular display precision (0.00°)")
  ("ANGDIR"             0          "Units"       "Counter-clockwise positive angle calculation")
  ("ANGBASE"            0.0        "Units"       "0.0 = East base zero azimuth")

  ;; -------------------------------------------------------------------------
  ;; 2. Core Drafting & Selection Behavior
  ;; -------------------------------------------------------------------------
  ("CMDECHO"            0          "Drafting"    "Suppresses command line echoes during automation")
  ("PICKFIRST"          1          "Drafting"    "Noun/verb selection enabled (select before command)")
  ("PICKADD"            2          "Drafting"    "Accumulate selection sets without holding Shift")
  ("HIGHLIGHT"          1          "Drafting"    "Highlight selected drawing entities")
  ("FILEDIA"            1          "Drafting"    "Force standard Windows file navigation dialogs")
  ("ATTDIA"             1          "Drafting"    "Force GUI dialog for block attribute input")
  ("MIRRTEXT"           0          "Drafting"    "Keep text legible and un-mirrored when mirroring")
  ("EDGEMODE"           1          "Drafting"    "Trim/Extend to implied intersecting boundary edges")
  ("DYNMODE"            3          "Drafting"    "Full Dynamic Input (pointer & dimensional input)")
  ("DRAGMODE"           2          "Drafting"    "Automatic continuous dynamic dragging")
  ("SNAPSTYLE"          0          "Drafting"    "Standard 2D rectangular grid layout")

  ;; -------------------------------------------------------------------------
  ;; 3. Cursor Ergonomics & Sizing
  ;; -------------------------------------------------------------------------
  ("CURSORSIZE"         25         "Cursor"      "Crosshair size percentage (25% or 100% per preset)")
  ("PICKBOX"            6          "Cursor"      "Object selection target box size (6 px)")
  ("GRIPSIZE"           7          "Cursor"      "Grip box display size (7 px)")
  ("APERTURE"           10         "Cursor"      "Object snap target box aperture (10 px)")

  ;; -------------------------------------------------------------------------
  ;; 4. Snaps, Grid & Drafting Toggles
  ;; -------------------------------------------------------------------------
  ("OSMODE"             2215       "Snaps"       "Standard 2D snap suite (End, Mid, Cen, Int, Perp, Ext)")
  ("OSOPTIONS"          7          "Snaps"       "Suppress OSNAP on hatches and underlays for speed")
  ("GRIDMODE"           1          "Snaps"       "Background drafting grid display ON")
  ("SNAPMODE"           0          "Snaps"       "Snap to grid cursor lock OFF")
  ("ORTHOMODE"          1          "Snaps"       "Orthographic 90° constraint ON")

  ;; -------------------------------------------------------------------------
  ;; 5. Display, Lineweights & Entity Defaults
  ;; -------------------------------------------------------------------------
  ("LWDISPLAY"          0          "Display"     "Default lineweight display OFF for drafting performance")
  ("TRANSPARENCYDISPLAY" 1         "Display"     "Render entity and layer transparency")
  ("MSLTSCALE"          1          "Display"     "Scale linetypes by annotative scale in modelspace")
  ("PSLTSCALE"          1          "Display"     "Scale linetypes uniformly across paperspace viewports")
  ("LTSCALE"            1.0        "Display"     "Global linetype scale factor")
  ("ANNOALLVISIBLE"     1          "Display"     "Keep annotative elements visible during drafting")
  ("CELWEIGHT"          -1         "Display"     "Lock new entity lineweight to ByLayer (-1)")
  ("CECOLOR"            "BYLAYER"  "Display"     "Current entity color ByLayer")
  ("CELTYPE"            "BYLAYER"  "Display"     "Current entity linetype ByLayer")
  ("DRAWORDERCTL"       3          "Display"     "Full draw order display inheritance enabled")
  ("LAYLOCKFADECTL"     50         "Display"     "50% contrast dimming on locked background layers")

  ;; -------------------------------------------------------------------------
  ;; 6. Performance & Visual Optimization
  ;; -------------------------------------------------------------------------
  ("SELECTIONPREVIEW"   3          "Performance" "Dynamic hover preview for cursor selection")
  ("HPQUICKPREVIEW"     0          "Performance" "Disable laggy hatch hover preview on dense drawings")
))

;; ===========================================================================
;; DATABASE ACCESSORS
;; ===========================================================================

;; CadSetup:GetRecommendedSysVars - Returns complete list of recommended sysvars
(defun CadSetup:GetRecommendedSysVars ()
  *CadSetup-SysVars-Data*
)

(if *CadSetup-Debug*
  (princ "\n[Database/Db_SysVars.lsp] System Variables Dictionary loaded.")
)
(princ)
