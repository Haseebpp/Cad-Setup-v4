;;; ==========================================================================
;;; CAD-SETUP-AUTORUN.lsp - Production Architectural & Fitout Drafting System
;;; Automatic Initialization File for AutoCAD APPLOAD / Startup Suite
;;; Commands     : RELOAD-COMMAND-SUITES, RCS, LOAD-COMMANDS
;;; ==========================================================================

(vl-load-com)

;; ===========================================================================
;; 1. PROJECT DIRECTORY CONFIGURATION (MANUAL PATH)
;; ===========================================================================

;; Set your project directory path manually below:
(setq *CAD-SETUP-DIR* "D:\\Cad-Setup-v3")

;; Helper: Retrieve project directory
;; Uses *CAD-SETUP-DIR*, or the folder containing CAD-SETUP-AUTORUN.lsp
(defun CadSetup:GetDir ( / p )
  (cond
    ;; 1. Configured manual path (trimmed of trailing slashes)
    ((and (boundp '*CAD-SETUP-DIR*)
          *CAD-SETUP-DIR*
          (= (type *CAD-SETUP-DIR*) 'STR)
          (vl-file-directory-p (vl-string-right-trim "\\/" *CAD-SETUP-DIR*)))
     (vl-string-right-trim "\\/" *CAD-SETUP-DIR*))

    ;; 2. Folder containing CAD-SETUP-AUTORUN.lsp (Commands folder is always beside it)
    ((and (setq p (findfile "CAD-SETUP-AUTORUN.lsp"))
          (setq p (vl-filename-directory p))
          (vl-file-directory-p p))
     p)

    ;; 3. Default fallback
    (t "D:\\Cad-Setup-v3")
  )
)

;; ===========================================================================
;; 2. MODULAR COMMAND SUITE LOADER
;; ===========================================================================

;; Core function: Discovers and loads all .lsp files in "Commands" folder
(defun LOAD-COMMANDS ( / baseDir cmdDir lspFiles fName fullPath loadedCount failedCount res )
  (vl-load-com)
  (setq baseDir (CadSetup:GetDir))

  (if (and baseDir (vl-file-directory-p baseDir))
    (progn
      (setq cmdDir (strcat baseDir "\\Commands"))

      (if (and cmdDir (vl-file-directory-p cmdDir))
        (progn
          ;; Discover and sort files ascending (00 -> 99)
          (setq lspFiles (vl-directory-files cmdDir "*.lsp" 1))
          (setq lspFiles (vl-sort lspFiles (function (lambda (a b) (< (strcase a) (strcase b))))))
          (setq loadedCount 0
                failedCount 0)

          (princ "\n------------------------------------------------------------")
          (princ (strcat "\n[Commands] Loading modular drafting suites from: " cmdDir))
          (princ "\n[Commands] Priority-ordered execution (Low -> High / 00 -> 99)...")
          (princ "\n------------------------------------------------------------")

          (while lspFiles
            (setq fName    (car lspFiles)
                  lspFiles (cdr lspFiles)
                  fullPath (strcat cmdDir "\\" fName))
            (setq res (vl-catch-all-apply 'load (list fullPath)))
            (if (vl-catch-all-error-p res)
              (progn
                (setq failedCount (1+ failedCount))
                (princ (strcat "\n [X] Failed: " fName " -> " (vl-catch-all-error-message res)))
              )
              (progn
                (setq loadedCount (1+ loadedCount))
                (princ (strcat "\n [✓] Loaded: " fName))
              )
            )
          )

          (princ "\n------------------------------------------------------------")
          (princ (strcat "\n[Commands] Complete: " (itoa loadedCount) " suite(s) loaded successfully."))
          (if (> failedCount 0)
            (princ (strcat "\n[Commands] Warning: " (itoa failedCount) " suite(s) encountered errors."))
          )
          (princ "\n------------------------------------------------------------\n")
        )
        (princ (strcat "\n[Commands] Error: Directory '" cmdDir "' not found.\n"))
      )
    )
    (princ (strcat "\n[Commands] Error: Base directory '" (vl-princ-to-string baseDir) "' not found.\n"))
  )
  (princ)
)

;; AutoCAD Command Aliases (type without parentheses at AutoCAD command line)
(defun c:RELOAD-COMMAND-SUITES () (LOAD-COMMANDS))
(defun c:RCS () (LOAD-COMMANDS))

;; ===========================================================================
;; 3. CORE DRAWING INITIALIZATION ROUTINE (AUTORUN)
;; ===========================================================================

(defun CadSetup:Initialize ( / *error* acadApp doc mSpace layersLts
                               oldCmdecho oldOsmode oldClayer dataList 
                               row lName lCol lPlotCol lType lWt lPlot lHatch lHScale lHRot lTrans lLocked lDesc
                               layObj LoadLinetype linFile lt )

  (vl-load-com)

  ;; ----------------------------------------------------------------------------
  ;; ENVIRONMENT & OBJECTS INITIALIZATION
  ;; ----------------------------------------------------------------------------
  (setq acadApp   (vlax-get-acad-object)
        doc       (if acadApp (vla-get-activedocument acadApp))
        mSpace    (if doc (vla-get-modelspace doc))
        layersLts (if doc (vla-get-linetypes doc)))

  ;; Cache Original System Variables (runtime state only)
  (setq oldCmdecho   (getvar "CMDECHO")
        oldOsmode    (getvar "OSMODE")
        oldClayer    (getvar "CLAYER"))

  ;; ----------------------------------------------------------------------------
  ;; ERROR HANDLER & UNDO MARK (Scoped locally to avoid altering global handler)
  ;; ----------------------------------------------------------------------------
  (defun *error* (msg)
    (if oldCmdecho   (setvar "CMDECHO"   oldCmdecho))
    (if oldOsmode    (setvar "OSMODE"    oldOsmode))
    (if oldClayer    (if (tblsearch "LAYER" oldClayer) (setvar "CLAYER" oldClayer)))
    (if doc (vl-catch-all-apply 'vla-endundomark (list doc)))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )
  (if doc (vla-startundomark doc))

  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)

  (princ "\nInitialising advanced production palette...")

  ;; -------------------------------------------------------------------------
  ;; A. DRAWING UNITS & INSERTION SCALE (Millimeters)
  ;; -------------------------------------------------------------------------
  (setvar "INSUNITS" 4)       ; 4 = Millimeters (prevents scaling chaos on XREF insert)
  (setvar "MEASUREMENT" 1)    ; 1 = Metric standards
  (setvar "LUNITS" 2)         ; 2 = Decimal units
  (setvar "LUPREC" 1)         ; 1 = Display precision (0.0 mm)
  (setvar "AUNITS" 0)         ; 0 = Decimal degrees
  (setvar "AUPREC" 2)         ; 2 = Display angular precision (0.00)
  (setvar "ANGDIR" 0)         ; 0 = Counter-clockwise angle calculation
  (setvar "ANGBASE" 0.0)      ; 0.0 = 0° East base angle

  ;; -------------------------------------------------------------------------
  ;; B. LOAD LINETYPES SAFELY
  ;; -------------------------------------------------------------------------
  (setq linFile (if (= (getvar "MEASUREMENT") 0) "acad.lin" "acadiso.lin"))
  (foreach lt '(
                ;; Hidden & Concealed Details
                "HIDDEN"   "HIDDEN2"   "HIDDENX2"
                ;; Centerlines & Grid References
                "CENTER"   "CENTER2"   "CENTERX2"
                ;; Phantom & Overhead / Boundary / Alternates
                "PHANTOM"  "PHANTOM2"  "PHANTOMX2"
                ;; Dashed (Standard, Half, Double)
                "DASHED"   "DASHED2"   "DASHEDX2"
                ;; Dot & Dash-Dot Series
                "DOT"      "DOT2"      "DOTX2"
                "DASHDOT"  "DASHDOT2"  "DASHDOTX2"
                ;; Divide & Border Series
                "DIVIDE"   "DIVIDE2"   "DIVIDEX2"
                "BORDER"   "BORDER2"   "BORDERX2"
                ;; Specialized Detail Patterns
                "BATTING"
                "ZIGZAG"
               )
    (if (not (tblsearch "ltype" lt))
      (vl-catch-all-apply
        '(lambda ()
           (command "-linetype" "_load" lt linFile "")
         )
      )
    )
  )

  ;; ==============================================================================
  ;; Master Data Definition
  ;; Format: (Layer Name | Layer Color (ACI) | Plot Color (RGB) | Linetype | Lineweight (0.01 mm) | Plot (T/nil) | Hatch Pattern | Hatch Scale | Hatch Rotation (deg) | Transparency (%) | Locked (T/nil) | Description)
  ;; ==============================================================================
  (setq dataList '(
    ;; Utilities
    ("01-HELP-LINE"       6   "180,0,180"   "CONTINUOUS"  5   nil  "NONE"      1.0  0.0  0   nil  "Color 6 (Magenta): Construction Lines — Auxiliary guides, temporary offsets, alignment rays (Non-Plotting)")
    ("02-VIEW-PORT"       6   "180,0,180"   "CONTINUOUS"  5   nil  "NONE"      1.0  0.0  0   nil  "Color 6 (Magenta): Viewports — Layout viewport frames and detail sheet cutouts (Non-Plotting)")
    ("03-GRID-LINE"       8   "100,100,100" "CONTINUOUS"  18  T    "NONE"      1.0  0.0  0   nil  "Color 8 (Dark Gray): Structural Grid — Primary building grid lines and column datums")

    ;; Annotations
    ("R-ANNO-DIMS"        20  "180,75,0"    "CONTINUOUS"  18  T    "NONE"      1.0  0.0  0   nil  "Color 20 (Orange/Tan): Dimensions — Primary and secondary dimension strings, overall gauges")
    ("R-ANNO-EQMT"        130 "0,120,130"   "CONTINUOUS"  25  T    "NONE"      1.0  0.0  0   nil  "Color 130 (Teal): Equipment Tags — Joinery & appliance equipment tags, hardware codes")
    ("R-ANNO-LEDR"        252 "80,80,80"    "CONTINUOUS"  15  T    "NONE"      1.0  0.0  0   nil  "Color 252 (Pale Gray): Leaders — Multileader lines, pointers, item balloon callout lines")
    ("R-ANNO-NOTE"        3   "0,120,50"    "CONTINUOUS"  25  T    "NONE"      1.0  0.0  0   nil  "Color 3 (Green): General Notes — General notes, legends, drawing schedules")
    ("R-ANNO-REVN"        10  "200,30,0"    "CONTINUOUS"  35  T    "NONE"      1.0  0.0  0   nil  "Color 10 (Red-Orange): Revision Cloud — Revision clouds, delta revision tags, drawing change marks")
    ("R-ANNO-SECT"        1   "180,0,0"     "DASHDOT"     50  T    "NONE"      1.0  0.0  0   nil  "Color 1 (Red): Section Cut — Section cutting plane lines, detail bubble markers")
    ("R-ANNO-SYMB"        2   "170,115,0"   "CONTINUOUS"  25  T    "NONE"      1.0  0.0  0   nil  "Color 2 (Yellow): Symbols — Elevation markers, detail markers, level symbols")
    ("R-ANNO-SPEC"        3   "0,120,50"    "CONTINUOUS"  25  T    "NONE"      1.0  0.0  0   nil  "Color 3 (Green): Specification Text — General specification callouts, finish tags")
    ("R-ANNO-TTLB"        7   "0,0,0"       "CONTINUOUS"  35  T    "NONE"      1.0  0.0  0   T    "Color 7 (White/Black): Titleblock Border — Title block geometry, border frame (Locked: Yes)")

    ;; Electrical
    ("R-ELEC-LIGHTING"    5   "0,80,190"    "CONTINUOUS"  25  T    "NONE"      1.0  0.0  0   nil  "Color 5 (Blue): Lighting — LED strip profiles, puck lights, driver housings, wire channels")
    ("R-ELEC-WIRING"      221 "120,30,155"  "PHANTOM2"    18  T    "NONE"      1.0  0.0  0   nil  "Color 221 (Violet): Wiring Path — Power feeds, conduit paths, driver connection routes")

    ;; Hardware
    ("R-HARD-FITTINGS"    252 "90,95,105"   "CONTINUOUS"  18  T    "NONE"      1.0  0.0  0   nil  "Color 252 (Pale Gray): Hardware Fittings — Concealed hinges, brackets, minifix connectors")
    ("R-HARD-HANDLES"     210 "145,25,115"  "CONTINUOUS"  25  T    "NONE"      1.0  0.0  0   nil  "Color 210 (Magenta/Violet): Hardware Handles — Exposed handles, edge pulls, knobs profiles")
    ("R-HARD-LOCKS"       212 "160,20,90"   "CONTINUOUS"  18  T    "NONE"      1.0  0.0  0   nil  "Color 212 (Magenta): Locks & Security — Cam locks, electronic card locks, espagnolette bolts")
    ("R-HARD-RUNNERS"     252 "85,90,100"   "CONTINUOUS"  25  T    "NONE"      1.0  0.0  0   nil  "Color 252 (Pale Gray): Hardware Runners — Drawer slides, soft-close mechanisms, sliding tracks")

    ;; Hatches
    ("R-HTCH-GENR"        8   "130,130,130" "CONTINUOUS"  5   T    "ANSI31"    1.0  0.0  40  nil  "Color 8 (Dark Gray): General Hatch — Generic pattern lines, section cross-hatching, surface fills")
    ("R-HTCH-SOLI"        250 "30,30,30"    "CONTINUOUS"  5   T    "SOLID"     1.0  0.0  50  nil  "Color 250 (Black): Solid Fill — Solid surface fills, tonal shading, opaque fills")

    ;; Linework
    ("R-LINE-CNTR"        1   "180,20,20"   "CENTER2"     13  T    "NONE"      1.0  0.0  0   nil  "Color 1 (Red): Center Line — Alignment centerlines, symmetry axes, positioning datums")
    ("R-LINE-CUT"         7   "0,0,0"       "CONTINUOUS"  50  T    "NONE"      1.0  0.0  0   nil  "Color 7 (White/Black): Cut Line — Primary section cuts, heavy substrate slicing profiles")
    ("R-LINE-DETL"        2   "165,110,0"   "CONTINUOUS"  18  T    "NONE"      1.0  0.0  0   nil  "Color 2 (Yellow): Detail Line — Secondary internal visible edges, panel grooves, rebates")
    ("R-LINE-HIDD"        9   "110,115,120" "HIDDEN2"     15  T    "NONE"      1.0  0.0  0   nil  "Color 9 (Light Gray): Hidden Line — Concealed framing, rear battens, internal shelf positions")
    ("R-LINE-VISB"        4   "0,110,150"   "CONTINUOUS"  25  T    "NONE"      1.0  0.0  0   nil  "Color 4 (Cyan): Visible Line — External carcass outlines, visible elevation edges")

    ;; Materials
    ("R-MAT-FABR-UPHL"    211 "160,50,90"   "CONTINUOUS"  18  T    "HOUND"     4.0  0.0  0   nil  "Color 211 (Pink): Fabric / Leather — Upholstery, acoustic wall paneling, foam padding")
    ("R-MAT-GLASS-MIRR"   140 "20,120,140"  "CONTINUOUS"  18  T    "AR-RROOF"  2.0  45.0 20  nil  "Color 140 (Light Cyan): Glass / Mirror — Clear/frosted glass, back-painted glass, mirrors (Transp: 20%)")
    ("R-MAT-INSL-CORE"    43  "105,115,30"  "CONTINUOUS"  15  T    "BATTS"     8.0  0.0  0   nil  "Color 43 (Olive): Insulation Core — Mineral wool, acoustic batts, structural backing cores")
    ("R-MAT-METAL"        90  "45,115,55"   "CONTINUOUS"  25  T    "ANSI34"    3.0  0.0  0   nil  "Color 90 (Light Green): Metal Structure — Brass trims, stainless steel bases, metal frames")
    ("R-MAT-PANEL-BOARD"  34  "135,70,25"   "CONTINUOUS"  25  T    "LINE"      5.0  0.0  0   nil  "Color 34 (Brown): Panel Board — MDF, Plywood, Chipboard substrate cores")
    ("R-MAT-PLAS-ACRY"    141 "20,110,170"  "CONTINUOUS"  18  T    "ANSI37"    6.0  0.0  15  nil  "Color 141 (Sky Blue): Plastic / Acrylic — Acrylic diffusers, PVC trims, synthetic profiles (Transp: 15%)")
    ("R-MAT-PNT-COAT"     11  "175,65,45"   "CONTINUOUS"  13  T    "DOTS"      8.0  0.0  0   nil  "Color 11 (Salmon): Paint / Coating — PU lacquer, powder-coat layers, back-coat primers")
    ("R-MAT-SEAL-GASK"    253 "65,65,65"    "CONTINUOUS"  13  T    "HONEY"     3.0  0.0  0   nil  "Color 253 (Dark Gray): Seals / Gaskets — Silicone joints, glazing gaskets, dust seals, neoprene")
    ("R-MAT-SOLID-WOOD"   30  "170,80,15"   "CONTINUOUS"  25  T    "ANSI38"    6.0  0.0  0   nil  "Color 30 (Orange): Solid Wood — Hardwood/softwood framing, solid timber lippings")
    ("R-MAT-STONE-SOLID"  150 "80,95,110"   "CONTINUOUS"  25  T    "AR-CONC"   1.0  0.0  0   nil  "Color 150 (Slate Gray): Solid Stone — Marble, quartz, granite, sintered stone surfaces")
    ("R-MAT-VENEER-LAM"   40  "150,95,30"   "CONTINUOUS"  15  T    "TRANS"     5.0  0.0  0   nil  "Color 40 (Light Brown): Veneer / Laminate — Natural wood veneer, HPL sheet, edge banding")
  ))

  ;; ==============================================================================
  ;;  LAYER CREATION & INITIAL UNLOCKING
  ;; ==============================================================================
  (princ "\nCreating & unlocking CAD layers for drawing generation...")

  ;; Ensure document & linetype collection references exist
  (if (not doc)
    (if (vlax-get-acad-object)
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
    )
  )
  (if doc (setq layersLts (vla-get-Linetypes doc)))

  ;; Helper: Load Linetype dynamically if missing
  (defun LoadLinetype (ltName / linFile)
    (if (and ltName (/= (strcase ltName) "CONTINUOUS") (not (tblsearch "LTYPE" ltName)))
      (progn
        (setq linFile (if (= (getvar "MEASUREMENT") 1) "acadiso.lin" "acad.lin"))
        (if layersLts
          (vl-catch-all-apply
            (function (lambda () (vla-load layersLts ltName linFile))))
          (vl-catch-all-apply
            (function (lambda () (command "._-linetype" "_load" ltName linFile ""))))
        )
      )
    )
  )

  ;; Layer Processing Loop
  (foreach row dataList
    (setq lName    (if (nth 0 row) (vl-princ-to-string (nth 0 row)) "0")
          lCol     (if (numberp (nth 1 row)) (nth 1 row) 7)
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

    ;; 1. Load custom linetype from LIN file if not already loaded in drawing
    (LoadLinetype lType)

    ;; 2. Create or access layer object
    (if doc
      (progn
        (setq layObj (vla-add (vla-get-layers doc) lName))

        ;; 3. Unlock layer so drawing & hatching operations succeed without errors
        (vla-put-lock layObj :vlax-false)
        (vla-put-color layObj lCol)

        ;; 4. Assign properties
        (if (tblsearch "LTYPE" lType)
          (vl-catch-all-apply 'vla-put-linetype (list layObj lType)))

        (vl-catch-all-apply 'vla-put-lineweight (list layObj lWt))
        (vla-put-plottable layObj (if lPlot :vlax-true :vlax-false))
        (vl-catch-all-apply 'vla-put-description (list layObj lDesc))
      )
      (progn
        ;; Command-based fallback if COM document object is unavailable
        (vl-catch-all-apply
          (function
            (lambda ()
              (command "._-layer" "_m" lName "_c" lCol lName)
              (if (tblsearch "LTYPE" lType) (command "_l" lType lName))
              (if (not lPlot) (command "_p" "_n" lName))
              (command "")
            )
          )
        )
      )
    )

    ;; 5. Set layer transparency via command if specified
    (if (> lTrans 0)
      (vl-catch-all-apply
        (function (lambda () (command "._LAYER" "_TR" (itoa lTrans) lName "")))))
  )

  ;; -------------------------------------------------------------------------
  ;; C. TYPOGRAPHY (Annotative & Scalable)
  ;; -------------------------------------------------------------------------
  (if (not (tblsearch "style" "ARCH-TEXT"))
    (command "-style" "ARCH-TEXT" "arial.ttf" "0.0" "1.0" "0" "_N" "_N")
  )
  (if (not (tblsearch "style" "ARCH-TITLE"))
    (command "-style" "ARCH-TITLE" "arialbd.ttf" "0.0" "1.0" "0" "_N" "_N")
  )

  ;; -------------------------------------------------------------------------
  ;; D. ANNOTATIVE DIMENSION STYLES
  ;; -------------------------------------------------------------------------
  (setvar "DIMTXSTY" "ARCH-TEXT")
  (setvar "DIMTXT"   2.5)         ; Plotted height = 2.5 mm
  (setvar "DIMTAD"   1)           ; Text above line
  (setvar "DIMJUST"  0)           ; Centered
  (setvar "DIMGAP"   0.8)         ; Gap between line and text
  (setvar "DIMEXE"   1.2)         ; Extension past dim line
  (setvar "DIMEXO"   1.0)         ; Extension line origin offset
  (setvar "DIMLUNIT" 2)           ; Decimal
  (setvar "DIMDEC"   0)           ; 0 decimal places
  (setvar "DIMZIN"   8)           ; Suppress trailing zeros (e.g. 150 instead of 150.0)
  (setvar "DIMCLRD"  256)         ; ByLayer
  (setvar "DIMCLRE"  256)         ; ByLayer
  (setvar "DIMCLRT"  256)         ; ByLayer
  (setvar "DIMLWD"   18)          ; 0.18 mm
  (setvar "DIMLWE"   18)          ; 0.18 mm
  (setvar "DIMTOFL"  1)           ; Force line between points

  ;; D1. Save ARCH-TICK (Annotative)
  (setvar "DIMBLK" "_ArchTick")
  (setvar "DIMASZ" 1.5)
  (if (tblsearch "dimstyle" "ARCH-TICK")
    (command "-dimstyle" "_save" "ARCH-TICK" "_yes")
    (command "-dimstyle" "_save" "ARCH-TICK")
  )

  ;; D2. Save ARCH-ARROW (Annotative)
  (setvar "DIMBLK" ".")           ; Closed Filled Arrow
  (setvar "DIMASZ" 2.2)
  (if (tblsearch "dimstyle" "ARCH-ARROW")
    (command "-dimstyle" "_save" "ARCH-ARROW" "_yes")
    (command "-dimstyle" "_save" "ARCH-ARROW")
  )

  (command "-dimstyle" "_restore" "ARCH-TICK")

  ;; -------------------------------------------------------------------------
  ;; E. ENVIRONMENT CONFIGURATION & COMMAND SUITES
  ;; -------------------------------------------------------------------------
  ;; Set current drawing layer safely (fallback to 0 if not found)
  (if (tblsearch "LAYER" "R-LINE-VISB")
    (setvar "CLAYER" "R-LINE-VISB")
    (if (and oldClayer (tblsearch "LAYER" oldClayer))
      (setvar "CLAYER" oldClayer)
      (setvar "CLAYER" "0")
    )
  )

  ;; Load Custom Command Suites (Priority-ordered execution from 00 to 99)
  (c:RELOAD-COMMAND-SUITES)

  ;; Undo Mark Finalization
  (if doc (vla-endundomark doc))
  
  (princ "\n[✓] Layers generated and visual swatch table built successfully.")
  (princ "\n[✓] CAD-SETUP-AUTORUN successfully configured with Annotative styles and Production layers.")
  (princ "\n[✓] Drafting environment & custom command suites initialized from Commands/.\n")
  (princ)
)

;; ===========================================================================
;; 4. AUTOMATIC INITIALIZATION ON APPLOAD / STARTUP SUITE
;; ===========================================================================
(CadSetup:Initialize)
(princ "\n[Loaded]: CAD-SETUP-AUTORUN initialized. Type RELOAD-COMMAND-SUITES or RCS to reload command suites.\n")
(princ)
