;;; ==========================================================================
;;; 04_Layout-Views.lsp - Zoom, Views, Annotation Shortcuts & Layout Presentation
;;; Layer: Commands (Priority 04)
;;; Author   : Haseeb
;;; ==========================================================================

(vl-load-com)

;;; --------------------------------------------------------------------------
;;; 1. PRESENTATION THEME SWITCHER (LB)
;;; --------------------------------------------------------------------------

;; LB : Toggle Layout between Dark Mode and Presentation White Paper (with Plot Styles)
(defun c:LB ( / acadApp doc prefObj dispPref layouts actLay isPlotStyleOn darkColor whiteColor )
  (setq acadApp    (CadSetup:GetAcad)
        doc        (CadSetup:GetDoc)
        prefObj    (if acadApp (vla-get-preferences acadApp))
        dispPref   (if prefObj (vla-get-display prefObj))
        layouts    (if doc (vla-get-layouts doc))
        actLay     (if doc (vla-get-ActiveLayout doc))
        whiteColor 16777215  ;; RGB: 255, 255, 255
        darkColor  3156001)  ;; RGB: 33, 40, 48 (Dark Charcoal)

  (if (and actLay dispPref layouts)
    (progn
      (setq isPlotStyleOn (= (vla-get-ShowPlotStyles actLay) :vlax-true))

      (if isPlotStyleOn
        ;; Switch to Dark Drafting Mode
        (progn
          (vla-put-GraphicsWinLayoutBackgrndColor dispPref darkColor)
          (vla-put-LayoutDisplayPaper dispPref :vlax-false)
          (vla-put-LayoutDisplayMargins dispPref :vlax-true)
          (vlax-for lay layouts
            (if (/= (strcase (vla-get-name lay)) "MODEL")
              (vla-put-ShowPlotStyles lay :vlax-false)
            )
          )
          (vla-regen doc 1)
          (princ "\n[LB] Switched to Dark Drafting Mode.")
        )
        ;; Switch to White Presentation Mode
        (progn
          (vla-put-GraphicsWinLayoutBackgrndColor dispPref whiteColor)
          (vla-put-LayoutDisplayPaper dispPref :vlax-true)
          (vla-put-LayoutDisplayMargins dispPref :vlax-false)
          (vlax-for lay layouts
            (if (/= (strcase (vla-get-name lay)) "MODEL")
              (vla-put-ShowPlotStyles lay :vlax-true)
            )
          )
          (vla-regen doc 1)
          (princ "\n[LB] Switched to White Presentation Mode.")
        )
      )
    )
    (princ "\n[LB] Active layout or display preferences not available.")
  )
  (princ)
)

;;; --------------------------------------------------------------------------
;;; 2. A3 SCALE SERIES GENERATOR
;;; --------------------------------------------------------------------------

;; A3SERIES : Generate A3 Scaled Reference Frames at Origin (1:1 to 1:50)
(defun c:A3SERIES (/ *error* baseW baseH gap startPt currentScale 
                     curW curH pt1 pt2 textHt textPt scaleList old-echo old-osmode)
  (setq old-echo   (getvar "CMDECHO")
        old-osmode (getvar "OSMODE"))

  (defun *error* (msg)
    (if old-osmode (setvar "OSMODE"  old-osmode))
    (if old-echo   (setvar "CMDECHO" old-echo))
    (CadSetup:UndoReset)
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[A3SERIES] Error: " msg))
    )
    (princ)
  )

  (CadSetup:UndoStart)
  (setvar "CMDECHO" 0)
  (setvar "OSMODE"  0)

  (setq baseW 420.0 
        baseH 297.0
        gap   1000.0)

  (setq startPt '(0.0 0.0 0.0))

  (command "._-LAYER" "_M" "R-ANNO-FRAMES" "_C" "6" "" "")

  (setq scaleList '(1 5 10 15 20 25 30 35 40 45 50))
  
  (foreach currentScale scaleList
    (setq curW (* baseW currentScale)
          curH (* baseH currentScale))

    (setq pt1 startPt
          pt2 (list (+ (car pt1) curW) (+ (cadr pt1) curH) (caddr pt1)))

    (command "._RECTANG" pt1 pt2)

    (setq textHt (* 10.0 currentScale)
          textPt (list (+ (car pt1) (* 10.0 currentScale)) 
                       (+ (cadr pt1) (* 10.0 currentScale)) 
                       (caddr pt1)))
    
    (command "._TEXT" textPt textHt "0" (strcat "A3 @ 1:" (itoa currentScale)))

    (setq startPt (list (+ (car startPt) curW gap) (cadr startPt) (caddr startPt)))
  )
  
  (CadSetup:UndoEnd)
  (setvar "CMDECHO" old-echo)
  (setvar "OSMODE"  old-osmode)
  (princ "\n[A3SERIES] A3 scale series frames (1:1 to 1:50) created.")
  (princ)
)

(if *CadSetup-Debug*
  (princ "\n[04_Layout-Views.lsp] View navigation, dimensions, and layout tools loaded.")
)
(princ)
