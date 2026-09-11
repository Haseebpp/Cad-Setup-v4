;;; ==========================================================================
;;; CAD-SETUP-AUTORUN.lsp - Production Architectural & Fitout Drafting System
;;; Automatic Initialization File for AutoCAD APPLOAD / Startup Suite
;;; Architecture  : Layered / Horizontal (Core -> Helpers -> Database -> Commands -> UI)
;;; Commands      : RELOAD-COMMAND-SUITES, RCS, LOAD-COMMANDS
;;; ==========================================================================

(vl-load-com)

;; ===========================================================================
;; 1. SYSTEM ROOT DIRECTORY RESOLUTION
;; ===========================================================================

;; CadSetup:GetDir - Dynamic discovery of Cad-Setup root folder
(defun CadSetup:GetDir ( / p candidate )
  (cond
    ;; 1. Folder containing CAD-SETUP-AUTORUN.lsp (Dynamic discovery)
    ((and (setq p (findfile "CAD-SETUP-AUTORUN.lsp"))
          (setq p (vl-filename-directory p))
          (vl-file-directory-p p))
     (vl-string-right-trim "\\/" p))

    ;; 2. Optional manual override path (if set externally)
    ((and (boundp '*CAD-SETUP-DIR*)
          *CAD-SETUP-DIR*
          (= (type *CAD-SETUP-DIR*) 'STR)
          (setq candidate (vl-string-right-trim "\\/" *CAD-SETUP-DIR*))
          (vl-file-directory-p candidate))
     candidate)
  )
)

;; ===========================================================================
;; 2. TIERED ARCHITECTURAL SUITE LOADER
;; ===========================================================================

;; Helper: Loads all *.lsp files from a subdirectory sorted alphabetically
(defun CadSetup:LoadFolder (subDirName label / baseDir folderPath files fName fullPath res count errCount)
  (setq baseDir (CadSetup:GetDir))
  (if (and baseDir (vl-file-directory-p baseDir))
    (progn
      (setq folderPath (strcat baseDir "\\" subDirName))
      (if (and folderPath (vl-file-directory-p folderPath))
        (progn
          (setq files (vl-directory-files folderPath "*.lsp" 1))
          (setq files (vl-sort files (function (lambda (a b) (< (strcase a) (strcase b))))))
          (setq count 0 errCount 0)

          (princ (strcat "\n[" label "] Loading: " folderPath))
          (while files
            (setq fName    (car files)
                  files    (cdr files)
                  fullPath (strcat folderPath "\\" fName))
            (setq res (vl-catch-all-apply 'load (list fullPath)))
            (if (vl-catch-all-error-p res)
              (progn
                (setq errCount (1+ errCount))
                (princ (strcat "\n  [✖] Error in " fName ": " (vl-catch-all-error-message res)))
              )
              (progn
                (setq count (1+ count))
                (princ (strcat "\n  [✓] " fName))
              )
            )
          )
          count
        )
        (progn
          (princ (strcat "\n[" label "] Notice: Directory '" folderPath "' not found."))
          0
        )
      )
    )
    (progn
      (princ (strcat "\n[" label "] Error: Base directory not found."))
      0
    )
  )
)

;; Master Loader: Executes horizontal tiers in exact dependency order:
;; Core -> Helpers -> Database -> Commands -> UI
(defun LOAD-COMMAND-SUITES ( / totalLoaded )
  (vl-load-com)
  (princ "\n============================================================")
  (princ "\n  CAD-SETUP-AUTORUN: INITIALIZING HORIZONTAL ARCHITECTURE   ")
  (princ "\n  Execution Flow: Core -> Helpers -> Database -> Commands -> UI")
  (princ "\n============================================================")

  ;; 1. Core Layer (Level 0: Path, error handling, undo manager)
  (CadSetup:LoadFolder "Core" "1. Core Layer")

  ;; 2. Helpers Layer (Level 1: Safe COM, selection, geometry, layers)
  (CadSetup:LoadFolder "Helpers" "2. Helpers Layer")

  ;; 3. Database Layer (Level 2: Pure data dictionaries - layers, sysvars, presets)
  (CadSetup:LoadFolder "Database" "3. Database Layer")

  ;; 4. Commands Layer (Level 3: Ergonomic drafting keys, blocks, utilities, aliases)
  (CadSetup:LoadFolder "Commands" "4. Commands Layer")

  ;; 5. UI Layer (Level 4: Dialog controls, preview canvas, presets GUI)
  (CadSetup:LoadFolder "UI" "5. UI Layer")

  (princ "\n------------------------------------------------------------")
  (princ "\n[✓] All horizontal architecture suites loaded successfully.")
  (princ "\n============================================================\n")
  (princ)
)

;; Command Aliases for reloading
(defun c:RELOAD-COMMAND-SUITES () (LOAD-COMMAND-SUITES))
(defun c:RCS () (LOAD-COMMAND-SUITES))
(defun c:LOAD-COMMANDS () (LOAD-COMMAND-SUITES))


;; ===========================================================================
;; 3. CORE DRAWING INITIALIZATION ROUTINE (RUNS ON DRAWING OPEN)
;; ===========================================================================

(defun CadSetup:Initialize ( / *error* doc dataList row
                               lName lCol lPlotCol lType lWt lPlot 
                               lHatch lHScale lHRot lTrans lLocked lDesc )

  (vl-load-com)

  ;; First, load all architecture layers so all helpers & database are active
  (LOAD-COMMAND-SUITES)

  ;; Scoped local error handler
  (defun *error* (msg)
    (CadSetup:UndoReset)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*")))
      (princ (strcat "\n[CadSetup Error]: " msg))
    )
    (princ)
  )

  (CadSetup:UndoStart)
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)

  (princ "\nGenerating production layer standards and styles...")

  ;; -------------------------------------------------------------------------
  ;; A. LOAD STANDARD LINETYPES
  ;; -------------------------------------------------------------------------
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
    (CadSetup:LoadLinetype lt)
  )

  ;; -------------------------------------------------------------------------
  ;; B. GENERATE LAYERS FROM DATABASE/Db_Layers.lsp
  ;; -------------------------------------------------------------------------
  (setq dataList (CadSetup:GetAllLayers))
  (if dataList
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

      (CadSetup:EnsureLayer lName lCol lPlotCol lType lWt lPlot 
                            lHatch lHScale lHRot lTrans lLocked lDesc)
    )
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
  (setvar "DIMZIN"   8)           ; Suppress trailing zeros
  (setvar "DIMCLRD"  256)         ; ByLayer
  (setvar "DIMCLRE"  256)         ; ByLayer
  (setvar "DIMCLRT"  256)         ; ByLayer
  (setvar "DIMLWD"   18)          ; 0.18 mm
  (setvar "DIMLWE"   18)          ; 0.18 mm
  (setvar "DIMTOFL"  1)           ; Force line between points

  ;; D1. Save ARCH-TICK (Annotative architectural tick)
  (setvar "DIMBLK" "_ArchTick")
  (setvar "DIMASZ" 1.5)
  (if (tblsearch "dimstyle" "ARCH-TICK")
    (command "-dimstyle" "_save" "ARCH-TICK" "_yes")
    (command "-dimstyle" "_save" "ARCH-TICK")
  )

  ;; D2. Save ARCH-ARROW (Annotative closed arrow)
  (setvar "DIMBLK" ".")
  (setvar "DIMASZ" 2.2)
  (if (tblsearch "dimstyle" "ARCH-ARROW")
    (command "-dimstyle" "_save" "ARCH-ARROW" "_yes")
    (command "-dimstyle" "_save" "ARCH-ARROW")
  )

  (command "-dimstyle" "_restore" "ARCH-TICK")

  ;; -------------------------------------------------------------------------
  ;; E. ENVIRONMENT DEFAULTS & ACTIVE LAYERS
  ;; -------------------------------------------------------------------------
  (CadSetup:SetCurrentLayerSafe "R-LINE-VISB")

  (if (and (getvar "DIMLAYER") (tblsearch "LAYER" "R-ANNO-DIMS"))
    (setvar "DIMLAYER" "R-ANNO-DIMS")
  )
  (if (and (getvar "HPLAYER") (tblsearch "LAYER" "R-HTCH-GENR"))
    (setvar "HPLAYER" "R-HTCH-GENR")
  )

  (CadSetup:UndoEnd)

  (princ "\n------------------------------------------------------------")
  (princ "\n[✓] Drawing environment initialized with production standards.")
  (princ "\n[✓] Annotative styles, Dimension standards, and Layers generated.")
  (princ "\n[✓] Commands active: 1=HL, 2=VP, 3=GL, 4=ML, CB, TC, CAD-SETTINGS, RCS")
  (princ "\n============================================================\n")
  (princ)
)

;; ===========================================================================
;; 4. AUTOMATIC INITIALIZATION ON APPLOAD / STARTUP SUITE
;; ===========================================================================
(CadSetup:Initialize)
(princ "\n[Loaded]: CAD-SETUP-AUTORUN ready. Type CAD-SETTINGS to configure or RCS to reload.\n")
(princ)
