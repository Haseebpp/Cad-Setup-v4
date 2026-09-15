;;; ==========================================================================
;;; Help_Layers.lsp - Safe Layer & Linetype Management Engine
;;; Layer: Helpers (Level 1)
;;; ==========================================================================

(vl-load-com)

;; ===========================================================================
;; DYNAMIC LINETYPE LOADER
;; ===========================================================================

;; CadSetup:LoadLinetype - Loads linetype safely from acad.lin / acadiso.lin
(defun CadSetup:LoadLinetype (ltName / linFile acadApp doc lts)
  (if (and ltName (/= (strcase ltName) "CONTINUOUS") (not (tblsearch "LTYPE" ltName)))
    (progn
      (setq linFile (if (= (getvar "MEASUREMENT") 1) "acadiso.lin" "acad.lin"))
      (setq acadApp (vlax-get-acad-object))
      (if acadApp (setq doc (vla-get-activedocument acadApp)))
      (if doc     (setq lts (vla-get-linetypes doc)))

      (if lts
        (vl-catch-all-apply
          (function (lambda () (vla-load lts ltName linFile))))
        (vl-catch-all-apply
          (function (lambda () (command "._-linetype" "_load" ltName linFile ""))))
      )
    )
  )
)

;; ===========================================================================
;; LAYER CREATION & CONFIGURATION
;; ===========================================================================

;; CadSetup:EnsureLayer
;; Creates or updates a single layer with production-grade styling.
;; Parameters:
;;   lName    : Layer name (string)
;;   lCol     : ACI Color (integer 1-255)
;;   lPlotCol : RGB string (e.g. "180,0,180") or nil
;;   lType    : Linetype name (string)
;;   lWt      : Lineweight in 0.01 mm (integer, e.g. 25 = 0.25mm)
;;   lPlot    : Plottable flag (T or nil)
;;   lHatch   : Hatch pattern name or "NONE"
;;   lHScale  : Hatch scale (real)
;;   lHRot    : Hatch rotation (real)
;;   lTrans   : Transparency percentage (0 to 90)
;;   lLocked  : Locked state flag (T or nil)
;;   lDesc    : Description string
(defun CadSetup:EnsureLayer (lName lCol lPlotCol lType lWt lPlot 
                             lHatch lHScale lHRot lTrans lLocked lDesc /
                             acadApp doc layObj)
  (setq acadApp (vlax-get-acad-object))
  (if acadApp (setq doc (vla-get-activedocument acadApp)))

  ;; 1. Load linetype if needed
  (if lType (CadSetup:LoadLinetype lType))

  ;; 2. Create or access layer via ActiveX
  (if doc
    (progn
      (setq layObj (vla-add (vla-get-layers doc) lName))

      ;; Unlock initially so geometry/hatching operations succeed
      (vla-put-lock layObj :vlax-false)
      (if (numberp lCol) (vla-put-color layObj lCol))

      ;; Assign linetype
      (if (and lType (tblsearch "LTYPE" lType))
        (vl-catch-all-apply 'vla-put-linetype (list layObj lType))
      )

      ;; Assign lineweight
      (if (numberp lWt)
        (vl-catch-all-apply 'vla-put-lineweight (list layObj lWt))
      )

      ;; Plottable
      (vla-put-plottable layObj (if lPlot :vlax-true :vlax-false))

      ;; Description
      (if (and lDesc (> (strlen lDesc) 0))
        (vl-catch-all-apply 'vla-put-description (list layObj lDesc))
      )

      ;; Lock if specified
      (if lLocked
        (vla-put-lock layObj :vlax-true)
      )
    )
    (progn
      ;; Command-based fallback
      (vl-catch-all-apply
        (function
          (lambda ()
            (command "._-layer" "_m" lName "_c" lCol lName)
            (if (and lType (tblsearch "LTYPE" lType))
              (command "_l" lType lName)
            )
            (if (not lPlot) (command "_p" "_n" lName))
            (command "")
          )
        )
      )
    )
  )

  ;; 3. Apply layer transparency if greater than 0
  (if (and (numberp lTrans) (> lTrans 0))
    (vl-catch-all-apply
      (function (lambda () (command "._LAYER" "_TR" (itoa lTrans) lName ""))))
  )
  T
)

;; ===========================================================================
;; DATABASE LAYER LOADER
;; ===========================================================================

;; CadSetup:EnsureLayerFromDb - Loads and ensures layer with specifications from Database/Db_Layers.lsp
;; Returns T if layer exists or was successfully created from DB, nil otherwise.
(defun CadSetup:EnsureLayerFromDb (layName / dbRow lName lCol lPlotCol lType lWt lPlot lHatch lHScale lHRot lTrans lLocked lDesc)
  (if (and layName (= (type layName) 'STR) (> (strlen layName) 0))
    (cond
      ;; Already exists in drawing database
      ((tblsearch "LAYER" layName)
       T
      )
      ;; Look up in master Database/Db_Layers.lsp
      ((setq dbRow (if (boundp 'CadSetup:GetLayerData)
                     (CadSetup:GetLayerData layName)
                     (if (boundp '*CadSetup-Layers-Data*)
                       (assoc (strcase layName) (mapcar '(lambda (x) (cons (strcase (car x)) (cdr x))) *CadSetup-Layers-Data*))
                       nil)))
       (setq lName    (if (nth 0 dbRow) (vl-princ-to-string (nth 0 dbRow)) layName)
             lCol     (if (numberp (nth 1 dbRow)) (nth 1 dbRow) 7)
             lPlotCol (if (nth 2 dbRow) (vl-princ-to-string (nth 2 dbRow)) "")
             lType    (if (nth 3 dbRow) (vl-princ-to-string (nth 3 dbRow)) "CONTINUOUS")
             lWt      (if (numberp (nth 4 dbRow)) (nth 4 dbRow) 25)
             lPlot    (nth 5 dbRow)
             lHatch   (if (nth 6 dbRow) (vl-princ-to-string (nth 6 dbRow)) "NONE")
             lHScale  (if (numberp (nth 7 dbRow)) (nth 7 dbRow) 1.0)
             lHRot    (if (numberp (nth 8 dbRow)) (nth 8 dbRow) 0.0)
             lTrans   (if (numberp (nth 9 dbRow)) (nth 9 dbRow) 0)
             lLocked  (nth 10 dbRow)
             lDesc    (if (nth 11 dbRow) (vl-princ-to-string (nth 11 dbRow)) ""))
       (CadSetup:EnsureLayer lName lCol lPlotCol lType lWt lPlot lHatch lHScale lHRot lTrans lLocked lDesc)
      )
      ;; Layer is not defined in Database/Db_Layers.lsp -> Refuse blind creation and notify
      (t
       (princ (strcat "\n[Notice]: Layer \"" layName "\" not found in Database/Db_Layers.lsp. Refusing unstandardized layer creation."))
       nil
      )
    )
    nil
  )
)

;; ===========================================================================
;; CURRENT LAYER SWITCHING
;; ===========================================================================

;; CadSetup:SetCurrentLayerSafe - Safely switches active layer (CLAYER)
;; Ensures layer exists by loading from master database; falls back to layer "0" if missing.
(defun CadSetup:SetCurrentLayerSafe (layName / acadApp doc layObj)
  (if (and layName (= (type layName) 'STR) (> (strlen layName) 0))
    (progn
      ;; Ensure layer exists via Database lookup
      (if (not (tblsearch "LAYER" layName))
        (CadSetup:EnsureLayerFromDb layName)
      )
      (if (tblsearch "LAYER" layName)
        (progn
          ;; Ensure layer is thawed and unlocked before making current
          (setq acadApp (vlax-get-acad-object))
          (if acadApp (setq doc (vla-get-activedocument acadApp)))
          (if doc
            (progn
              (setq layObj (vl-catch-all-apply 'vla-item (list (vla-get-layers doc) layName)))
              (if (and (not (vl-catch-all-error-p layObj)) (= (type layObj) 'VLA-OBJECT))
                (progn
                  (if (= (vla-get-freeze layObj) :vlax-true) (vla-put-freeze layObj :vlax-false))
                  (if (= (vla-get-lock layObj) :vlax-true)   (vla-put-lock layObj :vlax-false))
                )
              )
            )
          )
          (setvar "CLAYER" layName)
          T
        )
        (progn
          (princ (strcat "\n[Notice]: Layer \"" layName "\" unavailable. Falling back CLAYER to \"0\"."))
          (setvar "CLAYER" "0")
          nil
        )
      )
    )
    nil
  )
)

;; ===========================================================================
;; LAYER LOCK STATE MANAGEMENT
;; ===========================================================================

;; CadSetup:EnsureLayerUnlocked - Safely unlocks a single layer if it exists and is locked
(defun CadSetup:EnsureLayerUnlocked (layName / acadDoc layObj)
  (if (and layName (= (type layName) 'STR) (> (strlen layName) 0) (tblsearch "LAYER" layName))
    (progn
      (setq acadDoc (CadSetup:GetDoc))
      (if acadDoc
        (vl-catch-all-apply
          (function
            (lambda ()
              (setq layObj (vla-Item (vla-get-Layers acadDoc) layName))
              (if (and layObj (= (vla-get-Lock layObj) :vlax-true))
                (vla-put-Lock layObj :vlax-false)
              )
            )
          )
        )
      )
      T
    )
    nil
  )
)

;; CadSetup:UnlockAllLayers - Unlocks all currently locked layers in the active drawing
;; Returns a list of strings containing the names of layers that were previously locked.
(defun CadSetup:UnlockAllLayers ( / acadDoc layers lockedList layObj i count )
  (setq acadDoc (CadSetup:GetDoc)
        lockedList nil)
  (if acadDoc
    (progn
      (setq layers (vla-get-Layers acadDoc)
            count  (vla-get-Count layers)
            i      0)
      (while (< i count)
        (setq layObj (vla-Item layers i))
        (if (= (vla-get-Lock layObj) :vlax-true)
          (progn
            (setq lockedList (cons (vla-get-Name layObj) lockedList))
            (vl-catch-all-apply 'vla-put-Lock (list layObj :vlax-false))
          )
        )
        (setq i (1+ i))
      )
    )
  )
  lockedList
)

;; CadSetup:RestoreLockedLayers - Restores locked state to a specified list of layer names
(defun CadSetup:RestoreLockedLayers (layerNames / acadDoc layers layObj)
  (if (and layerNames (listp layerNames))
    (progn
      (setq acadDoc (CadSetup:GetDoc))
      (if acadDoc
        (progn
          (setq layers (vla-get-Layers acadDoc))
          (foreach lName layerNames
            (if (and lName (= (type lName) 'STR) (tblsearch "LAYER" lName))
              (vl-catch-all-apply
                (function
                  (lambda ()
                    (setq layObj (vla-Item layers lName))
                    (if layObj (vla-put-Lock layObj :vlax-true))
                  )
                )
              )
            )
          )
        )
      )
      T
    )
    nil
  )
)

(if *CadSetup-Debug*
  (princ "\n[Helpers/Help_Layers.lsp] Safe Layer & Linetype management loaded.")
)
(princ)
