;;; ==========================================================================
;;; Autorun.lsp - Production Architectural & Fitout Drafting System
;;; Automatic Initialization File for AutoCAD APPLOAD / Startup Suite
;;; Architecture  : Layered / Horizontal (Core -> Helpers -> Database -> Commands -> UI -> DotNet)
;;; Commands      : RELOAD-COMMAND-SUITES, RCS, LOAD-COMMANDS,
;;;                 DOTNET-BUILD (DNB), DOTNET-REBUILD, DOTNET-STATUS
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

;; ===========================================================================
;; 2b. DOTNET LAYER LOADER (.NET 10 single-file C# -> NETLOAD)
;; ===========================================================================
;; Every DotNet\DotNet_XXXX.cs is compiled with the .NET 10 file-based build
;; ("dotnet build DotNet_XXXX.cs", no .csproj) into:
;;     DotNet\bin\DotNet_XXXX\<yyyymmddhhmmss>\DotNet_XXXX_<yyyymmddhhmmss>.dll
;; - Only rebuilt when the source changed (stamp kept in bin\DotNet_XXXX\last.stamp)
;; - Each build gets its own folder + assembly name, because NETLOAD locks a DLL
;;   and AutoCAD ignores a second load of the same assembly name in one session.
;; - Loaded DLLs are tracked on the blackboard: .NET assemblies are per-process,
;;   while Autorun runs once per drawing.
;; - Without a .NET SDK the build step is skipped and the newest committed DLL loads.

(setq *CadSetup-DotNet-TFM* "net10.0-windows")

(defun CadSetup:DotNet-Pad (n w / s)
  (setq s (itoa n))
  (while (< (strlen s) w) (setq s (strcat "0" s)))
  s
)

;; Source change stamp: file modified time + size
(defun CadSetup:DotNet-SourceStamp (path / st)
  (if (setq st (vl-file-systime path))
    (strcat (CadSetup:DotNet-Pad (nth 0 st) 4) (CadSetup:DotNet-Pad (nth 1 st) 2)
            (CadSetup:DotNet-Pad (nth 3 st) 2) (CadSetup:DotNet-Pad (nth 4 st) 2)
            (CadSetup:DotNet-Pad (nth 5 st) 2) (CadSetup:DotNet-Pad (nth 6 st) 2)
            "-" (itoa (vl-file-size path)))
  )
)

;; Build stamp from current date/time: "yyyymmddhhmmss" (DIMZIN-safe)
(defun CadSetup:DotNet-NowStamp ( / s )
  (setq s (vl-string-subst "" "." (rtos (getvar "CDATE") 2 6)))
  (while (< (strlen s) 14) (setq s (strcat s "0")))
  (substr s 1 14)
)

(defun CadSetup:DotNet-ReadLine (file / h l)
  (if (and (findfile file) (setq h (open file "r")))
    (progn (setq l (read-line h)) (close h) l)
  )
)

(defun CadSetup:DotNet-WriteLine (file str / h)
  (if (setq h (open file "w"))
    (progn (write-line str h) (close h) T)
  )
)

;; Runs a shell command hidden and WAITS for it. Returns the exit code (-1 on failure).
(defun CadSetup:DotNet-Exec (cmdLine / wsh rc)
  (setq rc -1)
  (if (setq wsh (vlax-create-object "WScript.Shell"))
    (progn
      (setq rc (vl-catch-all-apply 'vlax-invoke-method (list wsh 'Run cmdLine 0 :vlax-true)))
      (vlax-release-object wsh)
      (if (vl-catch-all-error-p rc) (setq rc -1))
    )
  )
  rc
)

;; .NET SDK detection (cached once per AutoCAD session)
(defun CadSetup:DotNet-HasSdk ( / v )
  (setq v (vl-bb-ref '*CadSetup-DotNet-Sdk*))
  (if (null v)
    (progn
      (setq v (if (= 0 (CadSetup:DotNet-Exec "cmd.exe /c dotnet --version >nul 2>&1")) "YES" "NO"))
      (vl-bb-set '*CadSetup-DotNet-Sdk* v)
    )
  )
  (= v "YES")
)

;; Adds DotNet\bin\... (incl. subfolders) to TRUSTEDPATHS so SECURELOAD does not prompt
(defun CadSetup:DotNet-Trust (binDir / tp entry)
  (setq tp    (getvar "TRUSTEDPATHS")
        entry (strcat binDir "\\..."))
  (if (not (vl-string-search (strcase entry) (strcase tp)))
    (vl-catch-all-apply 'setvar
      (list "TRUSTEDPATHS" (if (= tp "") entry (strcat (vl-string-right-trim ";" tp) ";" entry))))
  )
)

;; Stamp sub-folders of a module bin folder, newest first
(defun CadSetup:DotNet-StampDirs (modDir)
  (if (vl-file-directory-p modDir)
    (vl-sort
      (vl-remove-if-not '(lambda (d) (wcmatch d "##############"))
                        (vl-directory-files modDir nil -1))
      '>)
  )
)

;; Newest compiled DLL for a module, or nil
(defun CadSetup:DotNet-Newest (modDir modName / dirs dll res)
  (setq dirs (CadSetup:DotNet-StampDirs modDir))
  (while (and dirs (null res))
    (setq dll (strcat modDir "\\" (car dirs) "\\" modName "_" (car dirs) ".dll"))
    (if (findfile dll) (setq res dll))
    (setq dirs (cdr dirs))
  )
  res
)

(defun CadSetup:DotNet-Loaded () (vl-bb-ref '*CadSetup-DotNet-Loaded*))

;; Returns T (newly loaded), 'SKIP (already loaded this session) or nil (failed)
(defun CadSetup:DotNet-Load (dll / oldDia res)
  (if (member (strcase dll) (CadSetup:DotNet-Loaded))
    'SKIP
    (progn
      (setq oldDia (getvar "FILEDIA"))
      (setvar "FILEDIA" 0)
      (setq res (vl-catch-all-apply 'command-s (list "_.NETLOAD" dll)))
      (setvar "FILEDIA" oldDia)
      (if (vl-catch-all-error-p res)
        (progn
          (princ (strcat "\n[CadSetup ERR] NETLOAD failed for " dll ": " (vl-catch-all-error-message res)))
          nil
        )
        (progn
          (vl-bb-set '*CadSetup-DotNet-Loaded* (cons (strcase dll) (CadSetup:DotNet-Loaded)))
          T
        )
      )
    )
  )
)

;; Compiles one DotNet_XXXX.cs. Returns T on success.
(defun CadSetup:DotNet-Build (dnDir src modName modDir / stamp outDir logFile cmd rc)
  (vl-mkdir modDir)
  (setq stamp   (CadSetup:DotNet-NowStamp)
        outDir  (strcat modDir "\\" stamp)
        logFile (strcat modDir "\\build.log"))
  (princ (strcat "\n[CadSetup .NET] Compiling " modName ".cs ..."))
  ;; cmd /s /c "<line>" -> outer quotes stripped, inner quoting preserved
  (setq cmd (strcat "cmd.exe /s /c \"cd /d \"" dnDir "\" && dotnet build \"" src "\""
                    " --nologo -c Release -o \"" outDir "\""
                    " -p:AssemblyName=" modName "_" stamp
                    " -p:TargetFramework=" *CadSetup-DotNet-TFM*
                    " -p:OutputType=Library -p:PublishAot=false -p:UseAppHost=false"
                    " > \"" logFile "\" 2>&1\""))
  (CadSetup:DotNet-LogDbg (strcat "  CMD: " cmd))
  (setq rc (CadSetup:DotNet-Exec cmd))
  (if (and (= rc 0) (findfile (strcat outDir "\\" modName "_" stamp ".dll")))
    (progn
      (CadSetup:DotNet-WriteLine (strcat modDir "\\last.stamp") (CadSetup:DotNet-SourceStamp src))
      (princ " OK")
      T
    )
    (progn
      (princ (strcat " FAILED (exit " (itoa rc) "). See: " logFile))
      nil
    )
  )
)

;; Deletes old stamp folders, keeping the newest and anything loaded this session.
;; Locked folders fail silently and are retried on a later run.
(defun CadSetup:DotNet-Cleanup (modDir modName / dirs keep fso full loaded)
  (setq dirs   (CadSetup:DotNet-StampDirs modDir)
        keep   (car dirs)
        dirs   (cdr dirs)
        loaded (CadSetup:DotNet-Loaded))
  (if (and dirs (setq fso (vlax-create-object "Scripting.FileSystemObject")))
    (progn
      (foreach d dirs
        (setq full (strcat modDir "\\" d))
        (if (not (member (strcase (strcat full "\\" modName "_" d ".dll")) loaded))
          (vl-catch-all-apply 'vlax-invoke-method (list fso 'DeleteFolder full :vlax-true))
        )
      )
      (vlax-release-object fso)
    )
  )
)

(defun CadSetup:DotNet-LogDbg (msg)
  (if *CadSetup-Debug* (princ (strcat "\n" msg)))
)

;; Master DotNet loader. forceAll = T rebuilds every file.
;; Returns list: (successCount errorCount) like CadSetup:LoadFolder
(defun CadSetup:LoadDotNet (forceAll / baseDir dnDir binDir files hasSdk count errCount
                                       src modName modDir dll res)
  (setq count 0 errCount 0)
  (setq *CadSetup-DotNet-Status* "no modules")
  (if (and (setq baseDir (CadSetup:GetDir))
           (vl-file-directory-p (setq dnDir (strcat baseDir "\\DotNet"))))
    (progn
      (setq binDir (strcat dnDir "\\bin"))
      (vl-mkdir binDir)
      (CadSetup:DotNet-Trust binDir)
      (setq files (vl-sort (vl-directory-files dnDir "DotNet_*.cs" 1)
                           (function (lambda (a b) (< (strcase a) (strcase b))))))
      (if files (setq hasSdk (CadSetup:DotNet-HasSdk)))
      (CadSetup:DotNet-LogDbg (strcat "[6. DotNet Layer] Loading: " dnDir
                                      (if hasSdk " (SDK found)" " (no SDK - prebuilt DLLs only)")))
      (foreach f files
        (setq modName (vl-filename-base f)
              src     (strcat dnDir "\\" f)
              modDir  (strcat binDir "\\" modName))
        ;; 1. Build if changed (or forced) and the SDK is available
        (if (and hasSdk
                 (or forceAll
                     (/= (CadSetup:DotNet-SourceStamp src)
                         (CadSetup:DotNet-ReadLine (strcat modDir "\\last.stamp")))))
          (if (not (CadSetup:DotNet-Build dnDir src modName modDir))
            (setq errCount (1+ errCount))
          )
        )
        ;; 2. NETLOAD the newest DLL (falls back to the previous one if the build failed)
        (if (setq dll (CadSetup:DotNet-Newest modDir modName))
          (progn
            (setq res (CadSetup:DotNet-Load dll))
            (if res
              (progn
                (setq count (1+ count))
                (CadSetup:DotNet-LogDbg (strcat "  [" (if (= res 'SKIP) "LOADED" "OK") "] " modName))
              )
              (setq errCount (1+ errCount))
            )
            (if hasSdk (CadSetup:DotNet-Cleanup modDir modName))
          )
          (progn
            (setq errCount (1+ errCount))
            (princ (strcat "\n[CadSetup WARN]: No compiled DLL for " modName
                           (if hasSdk " (build failed)." " and no .NET SDK installed to build it.")))
          )
        )
      )
      (if files
        (setq *CadSetup-DotNet-Status*
               (strcat (itoa count) " of " (itoa (length files)) " module(s) active"
                       (if hasSdk "" " (no SDK: prebuilt DLLs)")))
      )
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
  (if *CadSetup-DotNet-Status*
    (princ (strcat "\n DotNet    : " *CadSetup-DotNet-Status* " | DOTNET-BUILD (DNB) | DOTNET-STATUS"))
  )
  (if (> errCount 0)
    (princ (strcat "\n Notice    : " (itoa errCount) " module(s) had load errors. Type CAD-SETUP-DEBUG and RCS to inspect."))
  )
  (princ "\n----------------------------------------------------------------\n")
  (princ)
)

;; Master Loader: Executes horizontal tiers in exact dependency order:
;; Core -> Helpers -> Database -> Commands -> UI -> DotNet
;; Returns list: (totalLoaded totalErrors)
(defun LOAD-COMMAND-SUITES ( / res1 res2 res3 res4 res5 res6 totalLoaded totalErrors )
  (vl-load-com)
  (if *CadSetup-Debug*
    (progn
      (princ "\n============================================================")
      (princ "\n  AUTORUN: INITIALIZING HORIZONTAL ARCHITECTURE             ")
      (princ "\n  Execution Flow: Core -> Helpers -> Database -> Commands -> UI -> DotNet")
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

  ;; 6. DotNet Layer (Level 5: compiled C# commands, built only when changed)
  (setq res6 (CadSetup:LoadDotNet nil))

  (setq totalLoaded (+ (car res1) (car res2) (car res3) (car res4) (car res5) (car res6)))
  (setq totalErrors (+ (cadr res1) (cadr res2) (cadr res3) (cadr res4) (cadr res5) (cadr res6)))

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

;; DotNet commands
(defun CadSetup:DotNet-RunCmd (forceAll / res)
  (if (and (= (vl-bb-ref '*CadSetup-DotNet-Sdk*) "NO") forceAll)
    (vl-bb-set '*CadSetup-DotNet-Sdk* nil) ; re-check SDK on explicit rebuild
  )
  (setq res (CadSetup:LoadDotNet forceAll))
  (princ (strcat "\n[CadSetup .NET] " *CadSetup-DotNet-Status*
                 (if (> (cadr res) 0) (strcat ", " (itoa (cadr res)) " error(s)") "")))
  (princ)
)
(defun c:DOTNET-BUILD () (CadSetup:DotNet-RunCmd nil))
(defun c:DNB () (c:DOTNET-BUILD))
(defun c:DOTNET-REBUILD () (CadSetup:DotNet-RunCmd T))

(defun c:DOTNET-STATUS ( / baseDir dnDir modName modDir dll )
  (princ "\n---------------------- CadSetup .NET modules ----------------------")
  (princ (strcat "\n SDK: " (if (CadSetup:DotNet-HasSdk) "found" "NOT found (prebuilt DLLs only)")
                 "   Target: " *CadSetup-DotNet-TFM*))
  (if (and (setq baseDir (CadSetup:GetDir))
           (vl-file-directory-p (setq dnDir (strcat baseDir "\\DotNet"))))
    (foreach f (vl-directory-files dnDir "DotNet_*.cs" 1)
      (setq modName (vl-filename-base f)
            modDir  (strcat dnDir "\\bin\\" modName)
            dll     (CadSetup:DotNet-Newest modDir modName))
      (princ (strcat "\n " modName))
      (princ (strcat "\n   DLL    : " (if dll dll "<not built>")))
      (princ (strcat "\n   Loaded : " (if (and dll (member (strcase dll) (CadSetup:DotNet-Loaded))) "yes" "no")
                     "   Changed since build: "
                     (if (= (CadSetup:DotNet-SourceStamp (strcat dnDir "\\" f))
                            (CadSetup:DotNet-ReadLine (strcat modDir "\\last.stamp")))
                       "no" "yes")))
      (if (findfile (strcat modDir "\\build.log"))
        (princ (strcat "\n   Log    : " modDir "\\build.log"))
      )
    )
  )
  (princ "\n-------------------------------------------------------------------\n")
  (princ)
)


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
