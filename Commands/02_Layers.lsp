;;; ==========================================================================
;;; 02_Layers.lsp - Lightning-Fast Layer Management Shortcuts & Utilities
;;; Layer: Commands (Priority 02)
;;; Author   : Haseeb
;;; Commands : LOAD-LAYERS (RL, RELOAD-LAYERS),
;;;            LOAD-DEFAULT-CURRENT-LAYERS (DCL, DEFAULT-LAYERS), L0,
;;;            TOGGLE-HELP-LINE (THL, /),
;;;            BUILD-LAYER-LEGEND (BLL, LAYER-LEGEND)
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
  (princ (strcat "\n[OK] " (itoa count) " production layers verified and loaded."))

  ;; Interactive prompt to set default active layers
  (initget "Yes No")
  (setq ans (getkword "\nSet configured default active layers? [Yes/No] <Yes>: "))
  (if (or (null ans) (= ans "Yes"))
    (progn
      (CadSetup:SetDefaultCurrentLayers)
      (princ (strcat "\n[OK] Configured default active layers enforced (CLAYER: \"" (getvar "CLAYER") "\")."))
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
  (princ (strcat "\n  [OK] CLAYER   : \"" (getvar "CLAYER") "\""))
  (if (getvar "DIMLAYER")
    (princ (strcat "\n  [OK] DIMLAYER : \"" (getvar "DIMLAYER") "\""))
  )
  (if (getvar "HPLAYER")
    (princ (strcat "\n  [OK] HPLAYER  : \"" (getvar "HPLAYER") "\""))
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

;; TOGGLE-HELP-LINE / THL / /
;; Rapidly toggles visibility (ON/OFF) of the "01-HELP-LINE" layer.
;; - If layer is missing from the drawing, creates it standardly from database and sets it ON.
;; - If layer is currently CLAYER when turning OFF, automatically switches CLAYER to default layer.
(defun c:TOGGLE-HELP-LINE ( / *error* acadApp doc layObj curLay defLay isCur layName wasMissing )
  (setq layName "01-HELP-LINE")
  (defun *error* (msg)
    (if (boundp 'CadSetup:UndoReset) (CadSetup:UndoReset))
    (if (and msg (not (wcmatch (strcase msg t) "*break*,*cancel*,*exit*")))
      (princ (strcat "\n[/] Error: " msg))
    )
    (princ)
  )

  (if (boundp 'CadSetup:UndoStart) (CadSetup:UndoStart))

  ;; 1. Check if layer exists in drawing, or create from DB
  (setq wasMissing (not (tblsearch "LAYER" layName)))
  (if wasMissing
    (if (boundp 'CadSetup:EnsureLayerFromDb)
      (CadSetup:EnsureLayerFromDb layName)
    )
  )

  (setq acadApp (vlax-get-acad-object))
  (if acadApp (setq doc (vla-get-activedocument acadApp)))

  (if (and doc (tblsearch "LAYER" layName))
    (progn
      (setq layObj (vl-catch-all-apply 'vla-item (list (vla-get-layers doc) layName)))
      (if (and (not (vl-catch-all-error-p layObj)) (= (type layObj) 'VLA-OBJECT))
        (progn
          (setq curLay (getvar "CLAYER")
                isCur  (= (strcase curLay) (strcase layName)))
          (cond
            ;; Case A: Layer was just freshly created because it didn't exist -> ensure ON
            (wasMissing
             (vla-put-layeron layObj :vlax-true)
             (if (= (vla-get-freeze layObj) :vlax-true) (vla-put-freeze layObj :vlax-false))
             (princ "\n[/] 01-HELP-LINE: ON")
            )
            ;; Case B: Layer is currently ON -> turn it OFF
            ((= (vla-get-layeron layObj) :vlax-true)
             (if isCur
               (progn
                 (setq defLay (if (and *DEFAULT-CLAYER*
                                       (= (type *DEFAULT-CLAYER*) 'STR)
                                       (> (strlen *DEFAULT-CLAYER*) 0)
                                       (tblsearch "LAYER" *DEFAULT-CLAYER*))
                                *DEFAULT-CLAYER*
                                "0"))
                 (if (boundp 'CadSetup:SetCurrentLayerSafe)
                   (CadSetup:SetCurrentLayerSafe defLay)
                   (setvar "CLAYER" defLay)
                 )
                 (vla-put-layeron layObj :vlax-false)
                 (princ (strcat "\n[/] 01-HELP-LINE: OFF (CLAYER switched to \"" defLay "\")"))
               )
               (progn
                 (vla-put-layeron layObj :vlax-false)
                 (princ "\n[/] 01-HELP-LINE: OFF")
               )
             )
            )
            ;; Case C: Layer is currently OFF -> turn it ON
            (t
             (vla-put-layeron layObj :vlax-true)
             (if (= (vla-get-freeze layObj) :vlax-true) (vla-put-freeze layObj :vlax-false))
             (princ "\n[/] 01-HELP-LINE: ON")
            )
          )
        )
        (princ "\n[/] Error: Unable to access 01-HELP-LINE layer object.")
      )
    )
    (princ "\n[/] Error: 01-HELP-LINE layer not available.")
  )

  (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))
  (princ)
)

(defun c:THL () (c:TOGGLE-HELP-LINE))
(defun c:/ () (c:TOGGLE-HELP-LINE))


;;; --------------------------------------------------------------------------
;;; 5. VISUAL SPECIFICATION & MATERIAL LAYER LEGEND GENERATOR
;;; --------------------------------------------------------------------------

;; CadSetup:DrawColorSwatch
;; Helper to render solid color swatch box and text label with outline layer and RGB assignment
(defun CadSetup:DrawColorSwatch (colXList colIdx yTop hgt sc aci colRgb strLabel isBlackText swatchLay tblLayer /
                                 x1 x2 yMid boxObj hObj txtObj)
  (setq x1     (nth colIdx colXList)
        x2     (nth (1+ colIdx) colXList)
        yMid   (- yTop (/ hgt 2.0))
        boxObj (CadSetup:DrawBox (list (+ x1 (* 2.5 sc)) (- yTop (* 3.0 sc)))
                                 (list (+ x1 (* 9.5 sc)) (- yTop (- hgt (* 3.0 sc))))
                                 (if swatchLay swatchLay tblLayer)))
  (if colRgb
    (progn
      ;; Apply TrueColor RGB directly to the bounding box outline and the hatch fill
      (CadSetup:SetEntityRGB boxObj colRgb)
      (setq hObj (CadSetup:CreateHatch boxObj "SOLID" 1.0 0.0 nil "R-HTCH-SOLI"))
      (if hObj
        (progn
          (CadSetup:SetEntityRGB hObj colRgb)
          (CadSetup:BringToFront hObj)
        )
      )
    )
    (progn
      (setq hObj (CadSetup:CreateHatch boxObj "SOLID" 1.0 0.0 aci "R-HTCH-SOLI"))
      (if hObj
        (CadSetup:BringToFront hObj)
      )
    )
  )
  (setq txtObj (CadSetup:AddText (list (+ x1 (* 11.5 sc)) yMid 0.0)
                                 strLabel
                                 (* 1.9 sc)
                                 7
                                 tblLayer
                                 "MIDDLE-LEFT"))
  (if isBlackText
    (CadSetup:SetEntityRGB txtObj '(0 0 0))
  )
  txtObj
)

;; CadSetup:GenerateLayerLegend
;; Generates the complete 8-column visual layer legend at insPt scaled by sc
(defun CadSetup:GenerateLayerLegend (insPt sc /
                                     tblLayer dataList tblX tblY baseColW colW totalW
                                     rowH titleH headH catH curY curCat colXList
                                     headers idx cStart cWidth hTitle alnPos ptX sepX
                                     row lName lCol lPlotCol lType lWt lPlot lHatch
                                     lHScale lHRot lTrans lLocked lDesc catName midY
                                     cellPlotBox bgPlotHatch rgbList txtRGB
                                     hBox)
  (setq tblLayer "03-GRID-LINE")
  (if (not (tblsearch "LAYER" tblLayer))
    (if (boundp 'CadSetup:EnsureLayerFromDb)
      (CadSetup:EnsureLayerFromDb tblLayer)
    )
  )

  (setq dataList (CadSetup:GetAllLayers))
  (if (null dataList)
    (progn
      (princ "\n[BLL ERR]: Master layer data not found in Database/Db_Layers.lsp.")
      nil
    )
    (progn
      (setq tblX     (car insPt)
            tblY     (cadr insPt)
            rowH     (* 12.0 sc)
            titleH   (* 14.0 sc)
            headH    (* 9.0 sc)
            catH     (* 7.5 sc)
            baseColW '(48.0 22.0 26.0 42.0 24.0 38.0 26.0 124.0)
            colW     (mapcar '(lambda (w) (* w sc)) baseColW)
            totalW   (apply '+ colW)
            curY     tblY
            curCat   "")

      ;; Cumulative X-coordinates for column boundaries
      (setq colXList (list tblX))
      (foreach w colW
        (setq colXList (append colXList (list (+ (last colXList) w))))
      )

      ;; ----------------------------------------------------------------------
      ;; 1. MAIN TITLE BANNER
      ;; ----------------------------------------------------------------------
      (CadSetup:DrawBox (list tblX curY) (list (+ tblX totalW) (- curY titleH)) tblLayer)
      (CadSetup:AddText (list (+ tblX (/ totalW 2.0)) (- curY (/ titleH 2.0)) 0.0)
                        "CAD STANDARD SPECIFICATION & MATERIAL LEGEND"
                        (* 4.0 sc)
                        7
                        tblLayer
                        "CENTER")
      (setq curY (- curY titleH))

      ;; ----------------------------------------------------------------------
      ;; 2. COLUMN HEADERS
      ;; ----------------------------------------------------------------------
      (CadSetup:DrawBox (list tblX curY) (list (+ tblX totalW) (- curY headH)) tblLayer)
      (setq headers '("LAYER NAME" "ACI COLOR" "PLOT (RGB)" "LINEWORK SPEC"
                      "HATCH" "PATTERN & SCALE" "PROPERTIES" "DESCRIPTION & USAGE")
            idx     0)

      (while (< idx (length headers))
        (setq cStart (nth idx colXList)
              cWidth (nth idx colW)
              hTitle (nth idx headers)
              alnPos (if (or (= idx 0) (= idx 7)) "MIDDLE-LEFT" "CENTER")
              ptX    (if (= alnPos "CENTER") (+ cStart (/ cWidth 2.0)) (+ cStart (* 3.0 sc))))
        (CadSetup:AddText (list ptX (- curY (/ headH 2.0)) 0.0)
                          hTitle
                          (* 1.9 sc)
                          7
                          tblLayer
                          alnPos)
        (setq idx (1+ idx))
      )

      ;; Header Vertical Separators
      (setq idx 1)
      (while (< idx (length colXList))
        (setq sepX (nth idx colXList))
        (CadSetup:DrawLine (list sepX curY) (list sepX (- curY headH)) tblLayer)
        (setq idx (1+ idx))
      )
      (setq curY (- curY headH))

      ;; ----------------------------------------------------------------------
      ;; 3. DATA ROWS GENERATION
      ;; ----------------------------------------------------------------------
      (foreach row dataList
        (setq lName    (if (nth 0 row) (vl-princ-to-string (nth 0 row)) "UNNAMED")
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
              lDesc    (if (nth 11 row) (vl-princ-to-string (nth 11 row)) "")
              catName  (cond
                          ((wcmatch (strcase lName) "0#*")     "UTILITIES")
                          ((wcmatch (strcase lName) "R-ANNO*") "ANNOTATIONS")
                          ((wcmatch (strcase lName) "R-ELEC*") "ELECTRICAL")
                          ((wcmatch (strcase lName) "R-HARD*") "HARDWARE")
                          ((wcmatch (strcase lName) "R-HTCH*") "HATCHES")
                          ((wcmatch (strcase lName) "R-LINE*") "LINEWORK")
                          ((wcmatch (strcase lName) "R-MAT*")  "MATERIALS")
                          (T "GENERAL")
                        ))

        ;; Ensure dynamic linetype is loaded prior to linework preview
        (CadSetup:LoadLinetype lType)

        ;; Section Category Banner
        (if (/= catName curCat)
          (progn
            (setq curCat catName)
            (CadSetup:DrawBox (list tblX curY) (list (+ tblX totalW) (- curY catH)) tblLayer)
            (CadSetup:AddText (list (+ tblX (* 4.0 sc)) (- curY (/ catH 2.0)) 0.0)
                              (strcat "■ SECTION: " curCat)
                              (* 2.4 sc)
                              2
                              tblLayer
                              "MIDDLE-LEFT")
            (setq curY (- curY catH))
          )
        )

        ;; Outer Row Cell Frame & Column Dividers
        (CadSetup:DrawBox (list tblX curY) (list (+ tblX totalW) (- curY rowH)) tblLayer)
        (setq idx 1)
        (while (< idx (length colXList))
          (setq sepX (nth idx colXList))
          (CadSetup:DrawLine (list sepX curY) (list sepX (- curY rowH)) tblLayer)
          (setq idx (1+ idx))
        )

        (setq midY (- curY (/ rowH 2.0)))

        ;; --- COL 1: LAYER NAME ---
        (CadSetup:AddText (list (+ (nth 0 colXList) (* 3.0 sc)) midY 0.0)
                          lName
                          (* 2.1 sc)
                          lCol
                          tblLayer
                          "MIDDLE-LEFT")

        ;; --- COL 2: ACI COLOR SWATCH + CODE (OUTLINE ON ROW LAYER) ---
        (CadSetup:DrawColorSwatch colXList 1 curY rowH sc lCol nil (itoa lCol) nil lName tblLayer)

        ;; --- COL 3: PLOT RGB COLOR SWATCH + VALUE (OUTLINE WITH RGB OVERRIDE) ---
        ;; 1. Draw solid white cell backdrop on 03-GRID-LINE and push to absolute back
        (setq cellPlotBox (CadSetup:DrawBox (list (nth 2 colXList) curY)
                                            (list (nth 3 colXList) (- curY rowH))
                                            tblLayer))
        (setq bgPlotHatch (CadSetup:CreateHatch cellPlotBox "SOLID" 1.0 0.0 nil tblLayer))
        (if bgPlotHatch
          (progn
            (CadSetup:SetEntityRGB bgPlotHatch '(255 255 255))
            (CadSetup:SendToBack bgPlotHatch)
          )
        )

        ;; 2. Render swatch with RGB outline & pure black text in front of white background
        (setq rgbList (if (and lPlotCol (/= lPlotCol ""))
                        (read (strcat "(" (vl-string-translate ",;/-" "    " lPlotCol) ")"))
                        nil))
        (if (and (listp rgbList) (= (length rgbList) 3) (vl-every 'numberp rgbList))
          (CadSetup:DrawColorSwatch colXList 2 curY rowH sc nil rgbList lPlotCol T tblLayer tblLayer)
          (progn
            (setq txtRGB (CadSetup:AddText (list (+ (nth 2 colXList) (/ (nth 2 colW) 2.0)) midY 0.0)
                                           (if (and lPlotCol (/= lPlotCol "")) lPlotCol "—")
                                           (* 1.8 sc)
                                           7
                                           tblLayer
                                           "CENTER"))
            (CadSetup:SetEntityRGB (cond (txtRGB) ((entlast))) '(0 0 0))
          )
        )

        ;; --- COL 4: LINEWORK SAMPLE & ANNOTATION ---
        (if (and lName (tblsearch "LAYER" lName))
          (progn
            (CadSetup:EnsureLayerUnlocked lName)
            (setvar "CLAYER" lName)
          )
        )
        (setvar "CECOLOR" "BYLAYER")
        (setvar "CELTYPE" "BYLAYER")
        (setvar "CELWEIGHT" -1)
        (CadSetup:DrawLine (list (+ (nth 3 colXList) (* 3.0 sc)) (+ midY (* 1.8 sc)))
                           (list (- (nth 4 colXList) (* 3.0 sc)) (+ midY (* 1.8 sc)))
                           lName)
        (CadSetup:AddText (list (+ (nth 3 colXList) (/ (nth 3 colW) 2.0)) (- midY (* 2.4 sc)) 0.0)
                          (strcat lType "  |  " (rtos (/ lWt 100.0) 2 2) "mm")
                          (* 1.6 sc)
                          7
                          tblLayer
                          "CENTER")

        ;; --- COL 5: HATCH SWATCH ---
        (if (and lHatch (/= (strcase lHatch) "NONE"))
          (progn
            (setq hBox (CadSetup:DrawBox (list (+ (nth 4 colXList) (* 2.5 sc)) (- curY (* 2.5 sc)))
                                         (list (- (nth 5 colXList) (* 2.5 sc)) (- curY (- rowH (* 2.5 sc))))
                                         lName))
            (CadSetup:CreateHatch hBox lHatch (/ (* lHScale sc) 20.0) lHRot nil
                                  (if (= (strcase lHatch) "SOLID") "R-HTCH-SOLI" "R-HTCH-GENR"))
          )
          (CadSetup:AddText (list (+ (nth 4 colXList) (/ (nth 4 colW) 2.0)) midY 0.0)
                            "—"
                            (* 1.8 sc)
                            8
                            tblLayer
                            "CENTER")
        )

        ;; --- COL 6: PATTERN SPECIFICATION ---
        (if (and lHatch (/= (strcase lHatch) "NONE"))
          (progn
            (CadSetup:AddText (list (+ (nth 5 colXList) (/ (nth 5 colW) 2.0)) (+ midY (* 1.8 sc)) 0.0)
                              lHatch
                              (* 1.8 sc)
                              7
                              tblLayer
                              "CENTER")
            (CadSetup:AddText (list (+ (nth 5 colXList) (/ (nth 5 colW) 2.0)) (- midY (* 2.2 sc)) 0.0)
                              (strcat "S:" (rtos lHScale 2 1) "  R:" (rtos lHRot 2 0) "°")
                              (* 1.5 sc)
                              8
                              tblLayer
                              "CENTER")
          )
          (CadSetup:AddText (list (+ (nth 5 colXList) (/ (nth 5 colW) 2.0)) midY 0.0)
                            "NONE"
                            (* 1.8 sc)
                            8
                            tblLayer
                            "CENTER")
        )

        ;; --- COL 7: PROPERTIES / STATUS FLAGS ---
        (CadSetup:AddText (list (+ (nth 6 colXList) (/ (nth 6 colW) 2.0)) (+ midY (* 1.8 sc)) 0.0)
                          (strcat "PLT: " (if lPlot "YES" "NO") "  |  LCK: " (if lLocked "YES" "NO"))
                          (* 1.5 sc)
                          (if (or (not lPlot) lLocked) 1 7)
                          tblLayer
                          "CENTER")
        (CadSetup:AddText (list (+ (nth 6 colXList) (/ (nth 6 colW) 2.0)) (- midY (* 2.2 sc)) 0.0)
                          (strcat "TRANS: " (itoa lTrans) "%")
                          (* 1.5 sc)
                          8
                          tblLayer
                          "CENTER")

        ;; --- COL 8: DESCRIPTION & USAGE ---
        (CadSetup:AddText (list (+ (nth 7 colXList) (* 3.0 sc)) midY 0.0)
                          lDesc
                          (* 1.8 sc)
                          lCol
                          lName
                          "MIDDLE-LEFT")

        (setq curY (- curY rowH))
      )
      T
    )
  )
)

;; BUILD-LAYER-LEGEND / LAYER-LEGEND / BLL
;; Generates the complete AutoCAD visual specification & material swatch legend table
(defun c:BUILD-LAYER-LEGEND ( / *error* oldEcho oldOsm oldClay oldCol oldLtype oldLwt
                               sc scName insPt promptMsg lockedList )
  (setq oldEcho  (getvar "CMDECHO")
        oldOsm   (getvar "OSMODE")
        oldClay  (getvar "CLAYER")
        oldCol   (getvar "CECOLOR")
        oldLtype (getvar "CELTYPE")
        oldLwt   (getvar "CELWEIGHT")
        lockedList nil)

  (defun *error* (msg)
    (if lockedList (CadSetup:RestoreLockedLayers lockedList))
    (if oldLwt   (setvar "CELWEIGHT" oldLwt))
    (if oldLtype (setvar "CELTYPE"   oldLtype))
    (if oldCol   (setvar "CECOLOR"   oldCol))
    (if oldClay  (setvar "CLAYER"    oldClay))
    (if oldOsm   (setvar "OSMODE"    oldOsm))
    (if oldEcho  (setvar "CMDECHO"   oldEcho))
    (CadSetup:UndoReset)
    (if (and msg (not (wcmatch (strcase msg t) "*break*,*cancel*,*exit*")))
      (princ (strcat "\n[BLL] Error: " msg))
    )
    (princ)
  )

  (CadSetup:UndoStart)
  (setvar "CMDECHO" 0)

  ;; 1. Silently synchronize & ensure all 31 layers and linetypes exist
  (princ "\n[BLL] Verifying production layers from database...")
  (CadSetup:LoadAllLayers)

  ;; 2. Temporarily unlock all locked layers for table construction
  (setq lockedList (CadSetup:UnlockAllLayers))

  ;; 3. Detect annotative scale factor & name
  (setq sc     (CadSetup:GetAnnoScaleRatio)
        scName (CadSetup:GetAnnoScaleName))

  (setq promptMsg (strcat "\nSelect insertion point for Legend Table [Default: 0,0] <Scale " scName ">: "))
  (setq insPt (getpoint promptMsg))
  (if (null insPt) (setq insPt '(0.0 0.0 0.0)))

  ;; 4. Disable OSNAP during geometric construction
  (setvar "OSMODE" 0)

  ;; 5. Generate legend
  (princ (strcat "\n[BLL] Generating Layer Legend at " (rtos (car insPt) 2 1) ", " (rtos (cadr insPt) 2 1)
                 " (Scale: " scName ", Multiplier: " (rtos sc 2 2) "x)..."))
  (if (CadSetup:GenerateLayerLegend insPt sc)
    (princ (strcat "\n[OK] Layer Legend generated successfully. (Table width: "
                   (rtos (* 350.0 sc) 2 1) " mm)"))
  )

  ;; 6. Restore drafting environment and layer lock states
  (if lockedList (CadSetup:RestoreLockedLayers lockedList))
  (if oldLwt   (setvar "CELWEIGHT" oldLwt))
  (if oldLtype (setvar "CELTYPE"   oldLtype))
  (if oldCol   (setvar "CECOLOR"   oldCol))
  (if oldClay  (setvar "CLAYER"    oldClay))
  (if oldOsm   (setvar "OSMODE"    oldOsm))
  (if oldEcho  (setvar "CMDECHO"   oldEcho))

  (CadSetup:UndoEnd)
  (princ)
)

(defun c:BLL () (c:BUILD-LAYER-LEGEND))
(defun c:LAYER-LEGEND () (c:BUILD-LAYER-LEGEND))

(if *CadSetup-Debug*
  (princ "\n[02_Layers.lsp] Production layer management and shortcuts loaded (RL, DCL, L0, THL, /, BLL).")
)
(princ)

