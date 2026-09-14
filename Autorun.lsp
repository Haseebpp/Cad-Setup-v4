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
  (princ "\n Status    : Commands Loaded (Run 'LOAD-LAYERS' or 'RL' to load standards)")
  (princ "\n Shortcuts : LOAD-LAYERS (RL) | LOAD-STYLES (LST) | CAD-SETTINGS | Keys: 1-4")
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

(defun CadSetup:Initialize ( / loadRes )
  (vl-load-com)

  ;; Load all architecture layers so all helpers, database, commands & UI are active
  (setq loadRes (LOAD-COMMAND-SUITES))

  ;; Display clean status summary banner
  (CadSetup:PrintBanner (cadr loadRes))
  (princ)
)

;; ===========================================================================
;; 4. AUTOMATIC INITIALIZATION ON APPLOAD / STARTUP SUITE
;; ===========================================================================
(CadSetup:Initialize)
