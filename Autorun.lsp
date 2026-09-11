;;; ==========================================================================
;;; Autorun.lsp - Production Architectural & Fitout Drafting System
;;; Automatic Initialization File for AutoCAD APPLOAD / Startup Suite
;;; Architecture  : Layered / Horizontal (Core -> Helpers -> Database -> Commands -> UI)
;;; Commands      : RELOAD-COMMAND-SUITES, RCS, LOAD-COMMANDS
;;; ==========================================================================

(vl-load-com)

;; ===========================================================================
;; 1. SYSTEM ROOT DIRECTORY RESOLUTION
;; ===========================================================================

;; CadSetup:GetDir - Dynamic discovery of Cad-Setup root folder
(defun CadSetup:GetDir ( / p )
  (if (and (setq p (findfile "Autorun.lsp"))
           (setq p (vl-filename-directory p))
           (vl-file-directory-p p))
    (vl-string-right-trim "\\/" p)
    (progn
      (princ "\n[CadSetup ERR]: System path resolution failed. Could not locate 'Autorun.lsp'.")
      (princ "\nPlease add 'Autorun.lsp' to AutoCAD APPLOAD (Startup Suite) or Support File Search Paths.")
      nil
    )
  )
)

;; ===========================================================================
;; 2. TIERED ARCHITECTURAL SUITE LOADER
;; ===========================================================================

;; Helper: Loads all *.lsp files from a subdirectory sorted alphabetically
;; Returns list: (successCount errorCount)
(defun CadSetup:LoadFolder (subDirName label / baseDir folderPath files fName fullPath res count errCount)
  (setq baseDir (CadSetup:GetDir))
  (setq count 0 errCount 0)
  (if (and baseDir (vl-file-directory-p baseDir))
    (progn
      (setq folderPath (strcat baseDir "\\" subDirName))
      (if (and folderPath (vl-file-directory-p folderPath))
        (progn
          (setq files (vl-directory-files folderPath "*.lsp" 1))
          (setq files (vl-sort files (function (lambda (a b) (< (strcase a) (strcase b))))))

          (if *CadSetup-Debug*
            (princ (strcat "\n[" label "] Loading: " folderPath))
          )
          (while files
            (setq fName    (car files)
                  files    (cdr files)
                  fullPath (strcat folderPath "\\" fName))
            (setq res (vl-catch-all-apply 'load (list fullPath)))
            (if (vl-catch-all-error-p res)
              (progn
                (setq errCount (1+ errCount))
                (princ (strcat "\n[CadSetup ERR] Failed loading " fName ": " (vl-catch-all-error-message res)))
              )
              (progn
                (setq count (1+ count))
                (if *CadSetup-Debug*
                  (princ (strcat "\n  [OK] " fName))
                )
              )
            )
          )
        )
        (progn
          (princ (strcat "\n[CadSetup WARN]: Directory '" folderPath "' not found."))
        )
      )
    )
    (progn
      (princ (strcat "\n[CadSetup ERR]: Base directory not found."))
    )
  )
  (list count errCount)
)

;; Banner Presentation: Clean, compact 4-line status summary
(defun CadSetup:PrintBanner (errCount / )
  (princ "\n----------------------------------------------------------------")
  (if (> errCount 0)
    (princ "\n CadSetup v4.0 - Architectural & Fitout Drafting Suite [WARN]")
    (princ "\n CadSetup v4.0 - Architectural & Fitout Drafting Suite [OK]")
  )
  (princ "\n Standards : Production Layers, Linetypes, & Dimstyles Active")
  (princ "\n Shortcuts : CAD-SETTINGS | RL (Reload Layers) | RCS | Keys: 1-4")
  (if (> errCount 0)
    (princ (strcat "\n Notice    : " (itoa errCount) " module(s) had load errors. Type CAD-SETUP-DEBUG and RCS to inspect."))
  )
  (princ "\n----------------------------------------------------------------\n")
  (princ)
)

;; Master Loader: Executes horizontal tiers in exact dependency order:
;; Core -> Helpers -> Database -> Commands -> UI
;; Returns list: (totalLoaded totalErrors)
(defun LOAD-COMMAND-SUITES ( / res1 res2 res3 res4 res5 totalLoaded totalErrors )
  (vl-load-com)
  (if *CadSetup-Debug*
    (progn
      (princ "\n============================================================")
      (princ "\n  AUTORUN: INITIALIZING HORIZONTAL ARCHITECTURE             ")
      (princ "\n  Execution Flow: Core -> Helpers -> Database -> Commands -> UI")
      (princ "\n============================================================")
    )
  )

  ;; 1. Core Layer (Level 0: Path, error handling, undo manager)
  (setq res1 (CadSetup:LoadFolder "Core" "1. Core Layer"))

  ;; 2. Helpers Layer (Level 1: Safe COM, selection, geometry, layers)
  (setq res2 (CadSetup:LoadFolder "Helpers" "2. Helpers Layer"))

  ;; 3. Database Layer (Level 2: Pure data dictionaries - layers, sysvars, presets)
  (setq res3 (CadSetup:LoadFolder "Database" "3. Database Layer"))

  ;; 4. Commands Layer (Level 3: Ergonomic drafting keys, blocks, utilities, aliases)
  (setq res4 (CadSetup:LoadFolder "Commands" "4. Commands Layer"))

  ;; 5. UI Layer (Level 4: Dialog controls, preview canvas, presets GUI)
  (setq res5 (CadSetup:LoadFolder "UI" "5. UI Layer"))

  (setq totalLoaded (+ (car res1) (car res2) (car res3) (car res4) (car res5)))
  (setq totalErrors (+ (cadr res1) (cadr res2) (cadr res3) (cadr res4) (cadr res5)))

  (if *CadSetup-Debug*
    (progn
      (princ "\n------------------------------------------------------------")
      (princ (strcat "\n[OK] " (itoa totalLoaded) " horizontal architecture files loaded."))
      (if (> totalErrors 0)
        (princ (strcat "\n[WARN] " (itoa totalErrors) " error(s) encountered during load."))
      )
      (princ "\n============================================================\n")
    )
  )
  (list totalLoaded totalErrors)
)

;; Command Aliases for reloading
(defun c:RELOAD-COMMAND-SUITES ( / res )
  (setq res (LOAD-COMMAND-SUITES))
  (CadSetup:PrintBanner (cadr res))
  (princ)
)
(defun c:RCS () (c:RELOAD-COMMAND-SUITES))
(defun c:LOAD-COMMANDS () (c:RELOAD-COMMAND-SUITES))


;; ===========================================================================
;; 3. CORE DRAWING INITIALIZATION ROUTINE (RUNS ON DRAWING OPEN)
;; ===========================================================================

(defun CadSetup:Initialize ( / *error* doc loadRes )

  (vl-load-com)

  ;; First, load all architecture layers so all helpers & database are active
  (setq loadRes (LOAD-COMMAND-SUITES))

  ;; Scoped local error handler
  (defun *error* (msg)
    (CadSetup:UndoReset)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*")))
      (princ (strcat "\n[CadSetup ERR]: " msg))
    )
    (princ)
  )

  (CadSetup:UndoStart)
  (setvar "CMDECHO" 0)

  (if *CadSetup-Debug*
    (princ "\nGenerating production layer standards and styles...")
  )

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
  (CadSetup:LoadAllLayers)

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
  (CadSetup:SetDefaultCurrentLayers)

  (CadSetup:UndoEnd)

  (CadSetup:PrintBanner (cadr loadRes))
  (princ)
)

;; ===========================================================================
;; 4. AUTOMATIC INITIALIZATION ON APPLOAD / STARTUP SUITE
;; ===========================================================================
(CadSetup:Initialize)
