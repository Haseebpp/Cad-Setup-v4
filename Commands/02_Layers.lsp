;;; ==========================================================================
;;; 02_Layers.lsp - Lightning-Fast Layer Management Shortcuts & Utilities
;;; Layer: Commands (Priority 02)
;;; Author   : Haseeb
;;; Commands : LOAD-LAYERS (RL, RELOAD-LAYERS),
;;;            LOAD-DEFAULT-CURRENT-LAYERS (DCL, DEFAULT-LAYERS), L0
;;; ==========================================================================

(vl-load-com)

;;; ==========================================================================
;;; 0. DEFAULT ACTIVE LAYERS CONFIGURATION
;;; Edit these variables to customize your default production working layers:
;;; ==========================================================================
(setq *DEFAULT-CLAYER*   "R-LINE-VISB")   ;; Active drawing layer (CLAYER)
(setq *DEFAULT-DIMLAYER* "R-ANNO-DIMS")   ;; Dimensioning layer (DIMLAYER) - "." for Use Current
(setq *DEFAULT-HPLAYER*  "R-HTCH-GENR")   ;; Hatching layer (HPLAYER) - "." for Use Current

;;; ==========================================================================
;;; 1. LAYER SYNCHRONIZATION & GENERATION ENGINE
;;; ==========================================================================

;; CadSetup:LoadAllLayers - Generates or restores all production layers from Db_Layers.lsp
(defun CadSetup:LoadAllLayers ( / dataList count row
                                lName lCol lPlotCol lType lWt lPlot 
                                lHatch lHScale lHRot lTrans lLocked lDesc )
  (setq dataList (CadSetup:GetAllLayers)
        count    0)
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

      (if (CadSetup:EnsureLayer lName lCol lPlotCol lType lWt lPlot 
                                lHatch lHScale lHRot lTrans lLocked lDesc)
        (setq count (1+ count))
      )
    )
  )
  count
)

;;; ==========================================================================
;;; 2. ACTIVE ENVIRONMENT LAYER DEFAULTS
;;; ==========================================================================

;; CadSetup:SetDefaultCurrentLayers - Safely sets configured default active layers
;; If a configured layer does not exist in the drawing, it falls back to layer "0" with a notice.
(defun CadSetup:SetDefaultCurrentLayers ( / cLay dimLay hLay res )
  ;; 1. CLAYER - Active Working Layer
  (setq cLay (if (and *DEFAULT-CLAYER* (= (type *DEFAULT-CLAYER*) 'STR) (> (strlen *DEFAULT-CLAYER*) 0))
               *DEFAULT-CLAYER*
               "0"))
  (if (tblsearch "LAYER" cLay)
    (CadSetup:SetCurrentLayerSafe cLay)
    (progn
      (princ (strcat "\n[Notice]: Layer \"" cLay "\" not found. Falling back CLAYER to \"0\"."))
      (CadSetup:SetCurrentLayerSafe "0")
    )
  )

  ;; 2. DIMLAYER - Dedicated Dimensioning Layer
  (if (getvar "DIMLAYER")
    (progn
      (setq dimLay (if (and *DEFAULT-DIMLAYER* (= (type *DEFAULT-DIMLAYER*) 'STR) (> (strlen *DEFAULT-DIMLAYER*) 0))
                     *DEFAULT-DIMLAYER*
                     "."))
      (cond
        ;; "." or "USECURRENT" means place dimensions on active current layer
        ((or (= dimLay ".") (= (strcase dimLay) "USECURRENT"))
         (vl-catch-all-apply 'setvar (list "DIMLAYER" "."))
        )
        ;; Layer exists in the drawing
        ((tblsearch "LAYER" dimLay)
         (setq res (vl-catch-all-apply 'setvar (list "DIMLAYER" dimLay)))
         (if (vl-catch-all-error-p res)
           (vl-catch-all-apply 'setvar (list "DIMLAYER" "0"))
         )
        )
        ;; Layer does not exist -> fall back to layer "0"
        (t
         (princ (strcat "\n[Notice]: Layer \"" dimLay "\" not found. Falling back DIMLAYER to \"0\"."))
         (vl-catch-all-apply 'setvar (list "DIMLAYER" "0"))
        )
      )
    )
  )

  ;; 3. HPLAYER - Dedicated Hatching Layer
  (if (getvar "HPLAYER")
    (progn
      (setq hLay (if (and *DEFAULT-HPLAYER* (= (type *DEFAULT-HPLAYER*) 'STR) (> (strlen *DEFAULT-HPLAYER*) 0))
                   *DEFAULT-HPLAYER*
                   "."))
      (cond
        ;; "." or "USECURRENT" means place hatches on active current layer
        ((or (= hLay ".") (= (strcase hLay) "USECURRENT"))
         (vl-catch-all-apply 'setvar (list "HPLAYER" "."))
        )
        ;; Layer exists in the drawing
        ((tblsearch "LAYER" hLay)
         (setq res (vl-catch-all-apply 'setvar (list "HPLAYER" hLay)))
         (if (vl-catch-all-error-p res)
           (vl-catch-all-apply 'setvar (list "HPLAYER" "0"))
         )
        )
        ;; Layer does not exist -> fall back to layer "0"
        (t
         (princ (strcat "\n[Notice]: Layer \"" hLay "\" not found. Falling back HPLAYER to \"0\"."))
         (vl-catch-all-apply 'setvar (list "HPLAYER" "0"))
        )
      )
    )
  )
  T
)

;;; ==========================================================================
;;; 3. INTERACTIVE LAYER COMMANDS & SHORTCUTS
;;; ==========================================================================

;; LOAD-LAYERS / RELOAD-LAYERS / RL
;; Restores all layers from standard database and optionally resets current working layers
(defun c:LOAD-LAYERS ( / *error* count ans curLay )
  (defun *error* (msg)
    (CadSetup:UndoReset)
    (if (and msg (not (wcmatch (strcase msg t) "*break*,*cancel*,*exit*")))
      (princ (strcat "\n[RL] Error: " msg))
    )
    (princ)
  )

  (CadSetup:UndoStart)
  (princ "\n[RL] Loading and synchronizing production layers from database...")
  (setq count (CadSetup:LoadAllLayers))
  (princ (strcat "\n[✓] " (itoa count) " production layers verified and loaded."))

  ;; Interactive prompt to set default active layers
  (initget "Yes No")
  (setq ans (getkword "\nSet configured default active layers? [Yes/No] <Yes>: "))
  (if (or (null ans) (= ans "Yes"))
    (progn
      (CadSetup:SetDefaultCurrentLayers)
      (princ (strcat "\n[✓] Configured default active layers enforced (CLAYER: \"" (getvar "CLAYER") "\")."))
    )
    (progn
      (setq curLay (getvar "CLAYER"))
      (princ (strcat "\n[RL] Preserved current active layer: \"" curLay "\"."))
    )
  )

  (CadSetup:UndoEnd)
  (princ)
)

(defun c:RELOAD-LAYERS () (c:LOAD-LAYERS))
(defun c:RL () (c:LOAD-LAYERS))


;; LOAD-DEFAULT-CURRENT-LAYERS / DEFAULT-LAYERS / DCL
;; Enforces default active layers (CLAYER, DIMLAYER, HPLAYER)
(defun c:LOAD-DEFAULT-CURRENT-LAYERS ( / *error* )
  (defun *error* (msg)
    (CadSetup:UndoReset)
    (if (and msg (not (wcmatch (strcase msg t) "*break*,*cancel*,*exit*")))
      (princ (strcat "\n[DCL] Error: " msg))
    )
    (princ)
  )

  (CadSetup:UndoStart)
  (CadSetup:SetDefaultCurrentLayers)
  (CadSetup:UndoEnd)

  (princ "\n[DCL] Default active layers enforced:")
  (princ (strcat "\n  [✓] CLAYER   : \"" (getvar "CLAYER") "\""))
  (if (getvar "DIMLAYER")
    (princ (strcat "\n  [✓] DIMLAYER : \"" (getvar "DIMLAYER") "\""))
  )
  (if (getvar "HPLAYER")
    (princ (strcat "\n  [✓] HPLAYER  : \"" (getvar "HPLAYER") "\""))
  )
  (princ)
)

(defun c:DEFAULT-LAYERS () (c:LOAD-DEFAULT-CURRENT-LAYERS))
(defun c:DCL () (c:LOAD-DEFAULT-CURRENT-LAYERS))


;;; --------------------------------------------------------------------------
;;; 4. LAYER SELECTION & STATE TOOLS
;;; --------------------------------------------------------------------------

;; L0 : Switch active layer to "0" immediately
(defun c:L0 ( / *error* )
  (defun *error* (msg)
    (CadSetup:UndoReset)
    (if (and msg (not (wcmatch (strcase msg t) "*break*,*cancel*,*exit*")))
      (princ (strcat "\n[L0] Error: " msg))
    )
    (princ)
  )

  (CadSetup:UndoStart)
  (CadSetup:SetCurrentLayerSafe "0")
  (CadSetup:UndoEnd)
  (princ "\n[L0] Current Layer is now: \"0\"")
  (princ)
)

(princ "\n[02_Layers.lsp] Production layer management and shortcuts loaded (RL, DCL, L0).")
(princ)

