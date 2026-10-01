;;; ==========================================================================
;;; 04_Layout-Views.lsp - Zoom, Views, Annotation Shortcuts & Layout Presentation
;;; Layer: Commands (Priority 04)
;;; Author   : Haseeb
;;; ==========================================================================

(vl-load-com)

;;; --------------------------------------------------------------------------
;;; 1. PRESENTATION THEME SWITCHER (TS)
;;; --------------------------------------------------------------------------

;; TS : Toggle Layout between Dark Mode and Presentation White Paper (with Plot Styles)
(defun c:TS ( / acadApp doc prefObj dispPref layouts actLay isPlotStyleOn darkColor whiteColor )
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
          (princ "\n[TS] Switched to Dark Drafting Mode.")
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
          (princ "\n[TS] Switched to White Presentation Mode.")
        )
      )
    )
    (princ "\n[TS] Active layout or display preferences not available.")
  )
  (princ)
)

;;; --------------------------------------------------------------------------
;;; 2. A3 SCALE SERIES GENERATOR
;;; --------------------------------------------------------------------------

;; A3SERIES : Generate A3 Scaled Reference Frames Diagonally (1:1 to 1:50)
(defun c:A3SERIES ( / *error* doc old-echo old-osmode old-layer
                      targetLayer userPt startPt baseW baseH scaleList
                      currentScale curW curH pt1 pt2 gapX gapY
                      textHt textMargin textPt textStr )
  (setq old-echo   (getvar "CMDECHO")
        old-osmode (getvar "OSMODE")
        old-layer  (getvar "CLAYER")
        doc        (if (vlax-get-acad-object)
                     (vla-get-activedocument (vlax-get-acad-object))
                   ))

  (defun *error* (msg)
    (if old-osmode (setvar "OSMODE"  old-osmode))
    (if old-layer  (setvar "CLAYER"  old-layer))
    (if old-echo   (setvar "CMDECHO" old-echo))
    (if (boundp 'CadSetup:UndoReset)
      (CadSetup:UndoReset)
      (if doc (vla-endundomark doc))
    )
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[A3SERIES] Error: " msg))
    )
    (princ)
  )

  ;; 1. Prompt for origin / starting insertion point (before turning off OSMODE)
  (setq userPt (getpoint "\n[A3SERIES] Specify starting insertion point <0,0,0>: "))
  (setq startPt (if userPt userPt '(0.0 0.0 0.0)))
  (if (not (caddr startPt))
    (setq startPt (list (car startPt) (cadr startPt) 0.0))
  )

  ;; 2. Start undo group and silence command echoing
  (if (boundp 'CadSetup:UndoStart)
    (CadSetup:UndoStart)
    (if doc (vla-startundomark doc))
  )
  (setvar "CMDECHO" 0)
  (setvar "OSMODE"  0)

  ;; 3. Ensure target layer exists and is active
  (setq targetLayer "R-ANNO-FRME")
  (if (and (boundp 'CadSetup:EnsureLayerFromDb) CadSetup:EnsureLayerFromDb)
    (CadSetup:EnsureLayerFromDb targetLayer)
  )
  (if (and (boundp 'CadSetup:SetCurrentLayerSafe) CadSetup:SetCurrentLayerSafe)
    (CadSetup:SetCurrentLayerSafe targetLayer)
    (if (tblsearch "LAYER" targetLayer)
      (setvar "CLAYER" targetLayer)
      (command "_.layer" "_make" targetLayer "")
    )
  )

  ;; 4. Dimensions and scale list
  (setq baseW 420.0   ;; ISO A3 width (mm)
        baseH 297.0)  ;; ISO A3 height (mm)

  (setq scaleList '(1 5 10 15 20 25 30 35 40 45 50))

  ;; 5. Generate frames and outside labels along ascending diagonal (+X, +Y)
  (foreach currentScale scaleList
    (setq curW (* baseW currentScale)
          curH (* baseH currentScale))

    (setq pt1 startPt
          pt2 (list (+ (car pt1) curW) (+ (cadr pt1) curH) (caddr pt1)))

    ;; Draw boundary frame (LWPOLYLINE with fallback to RECTANG)
    (if (not (entmake
               (list
                 '(0 . "LWPOLYLINE")
                 '(100 . "AcDbEntity")
                 (cons 8 targetLayer)
                 '(100 . "AcDbPolyline")
                 '(90 . 4)
                 '(70 . 1)  ;; Closed polyline
                 (cons 10 (list (car pt1) (cadr pt1)))
                 (cons 10 (list (car pt2) (cadr pt1)))
                 (cons 10 (list (car pt2) (cadr pt2)))
                 (cons 10 (list (car pt1) (cadr pt2)))
               )))
      (command "._RECTANG" "_non" pt1 "_non" pt2)
    )

    ;; Outside label: positioned 0.5x textHt below bottom-left corner, left-aligned
    (setq textHt     (* 10.0 currentScale)
          textMargin (* 5.0 currentScale)
          textPt     (list (car pt1) (- (cadr pt1) textMargin) (caddr pt1))
          textStr    (strcat "A3 @ 1:" (itoa currentScale)))

    ;; Create MTEXT with Top-Left justification (Attachment 71 = 1)
    (if (not (entmake
               (list
                 '(0 . "MTEXT")
                 '(100 . "AcDbEntity")
                 (cons 8 targetLayer)
                 '(100 . "AcDbMText")
                 (cons 10 textPt)
                 (cons 40 textHt)
                 '(41 . 0.0)
                 '(71 . 1)  ;; Top Left
                 '(72 . 1)  ;; Left to Right
                 (cons 1 textStr)
               )))
      (command "._TEXT" "_J" "_TL" "_non" textPt textHt "0" textStr)
    )

    ;; Calculate dynamic gap (10% of frame dimensions) and step diagonally (+X, +Y)
    (setq gapX (* curW 0.10)
          gapY (* curH 0.10))
    (setq startPt (list (+ (car pt1) curW gapX)
                        (+ (cadr pt1) curH gapY)
                        (caddr pt1)))
  )

  ;; 6. Restore environment and finish
  (if (and (boundp 'CadSetup:SetCurrentLayerSafe) CadSetup:SetCurrentLayerSafe)
    (CadSetup:SetCurrentLayerSafe old-layer)
    (if old-layer (setvar "CLAYER" old-layer))
  )
  (if (boundp 'CadSetup:UndoEnd)
    (CadSetup:UndoEnd)
    (if doc (vla-endundomark doc))
  )
  (setvar "CMDECHO" old-echo)
  (setvar "OSMODE"  old-osmode)
  (princ (strcat "\n[A3SERIES] Success: Created " (itoa (length scaleList)) " diagonal A3 scale frames (1:1 to 1:50) on layer \"" targetLayer "\"."))
  (princ)
)

;;; --------------------------------------------------------------------------
;;; 3. VPCLIP RECTANGLE DUPLICATOR (VPCLIPRECTANGLE / VR)
;;; --------------------------------------------------------------------------

;; Global Configuration: Target Viewport Layer
(if (not (boundp '*VPCLIP-LAYER*))
  (setq *VPCLIP-LAYER* "02-VIEW-PORT")
)

;; Helper: Resolves selection to the actual VIEWPORT entity
;; Accepts both regular viewports and clipping boundaries (LWPOLYLINE, REGION, etc.)
(defun CadSetup:GetViewportEntity (ent / ed ss i found vp)
  (if (and ent (setq ed (entget ent)))
    (cond
      ;; Direct Viewport entity (exclude layout overall viewport cvport=1)
      ((= (cdr (assoc 0 ed)) "VIEWPORT")
       (if (and (assoc 69 ed) (> (cdr (assoc 69 ed)) 1))
         ent
         nil
       )
      )
      ;; Boundary or other object: find parent viewport in current layout
      (t
       (setq ss (ssget "_X" (list '(0 . "VIEWPORT") (cons 410 (getvar 'ctab)))))
       (if ss
         (repeat (setq i (sslength ss))
           (setq vp (ssname ss (setq i (1- i))))
           (if (and (= (cdr (assoc 71 (entget vp))) 1)
                    (equal (cdr (assoc 340 (entget vp))) ent))
             (setq found vp)
           )
         )
       )
       found
      )
    )
  )
)

;; VPCLIPRECTANGLE : Copy Viewport and Clip with User-Drawn Rectangle
(defun c:VPCLIPRECTANGLE ( / *error* doc oldLayer oldCmdecho targetLayer
                             ssPre vpList i ent sel vpEnt
                             ssCopy preRect rect preCopy newVp e)

  ;; Error handler: safely restores environment on cancel or error
  (defun *error* (msg)
    (if oldCmdecho (setvar 'cmdecho oldCmdecho))
    (if oldLayer   (setvar 'clayer oldLayer))
    (sssetfirst nil nil)
    (if (boundp 'CadSetup:UndoReset)
      (CadSetup:UndoReset)
      (if doc (vla-endundomark doc))
    )
    (if (and msg (not (member msg '("Function cancelled" "quit / exit abort"))))
      (princ (strcat "\n[VR] Error: " msg))
    )
    (princ)
  )

  ;; Ensure execution is in Paper Space (Layout mode)
  (if (and (= (getvar 'tilemode) 0) (= (getvar 'cvport) 1))
    (progn
      (setq targetLayer (if *VPCLIP-LAYER* *VPCLIP-LAYER* "02-VIEW-PORT"))

      ;; ---------------------------------------------------------
      ;; CHECK PRE-SELECTION (PICKFIRST / IMPLIED SELECTION)
      ;; ---------------------------------------------------------
      (setq ssPre (ssget "_I"))
      (if ssPre
        (progn
          (setq vpList nil)
          (repeat (setq i (sslength ssPre))
            (setq ent (ssname ssPre (setq i (1- i))))
            (setq vpEnt (CadSetup:GetViewportEntity ent))
            ;; Deduplicate if both boundary and viewport were selected
            (if (and vpEnt (not (vl-position vpEnt vpList)))
              (setq vpList (cons vpEnt vpList))
            )
          )
          (sssetfirst nil nil) ; Clear active selection set

          (cond
            ;; Exactly one viewport found in pre-selection -> use it directly
            ((= (length vpList) 1)
             (setq vpEnt (car vpList))
            )
            ;; Multiple viewports found -> reset and prompt
            ((> (length vpList) 1)
             (setq vpEnt nil)
             (princ "\n[VR] Multiple viewports selected. Please pick one.")
            )
            ;; Non-viewport objects selected -> reset and prompt
            (t
             (setq vpEnt nil)
            )
          )
        )
      )

      ;; ---------------------------------------------------------
      ;; INTERACTIVE PROMPT (If not pre-selected or multiple picked)
      ;; ---------------------------------------------------------
      (while (and (not vpEnt)
                  (setq sel (entsel "\nSelect viewport or clipping boundary to copy & clip: ")))
        (setq ent (car sel))
        (setq vpEnt (CadSetup:GetViewportEntity ent))
        (if (not vpEnt)
          (princ "\n[VR] Selected object is not a valid viewport or clipping boundary. Try again.")
        )
      )

      ;; ---------------------------------------------------------
      ;; DRAW RECTANGLE & DUPLICATE/CLIP
      ;; ---------------------------------------------------------
      (if vpEnt
        (progn
          (setq doc (vla-get-activedocument (vlax-get-acad-object)))
          (if (boundp 'CadSetup:UndoStart)
            (CadSetup:UndoStart)
            (vla-startundomark doc)
          )

          (setq oldCmdecho (getvar 'cmdecho))
          (setvar 'cmdecho 0)
          (setq oldLayer (getvar 'clayer))

          ;; Ensure target layer exists in database and is thawed/unlocked
          (if (and (boundp 'CadSetup:EnsureLayerFromDb) CadSetup:EnsureLayerFromDb)
            (CadSetup:EnsureLayerFromDb targetLayer)
          )
          (if (and (boundp 'CadSetup:SetCurrentLayerSafe) CadSetup:SetCurrentLayerSafe)
            (CadSetup:SetCurrentLayerSafe targetLayer)
            (if (tblsearch "LAYER" targetLayer)
              (command "_.layer" "_on" targetLayer "_thaw" targetLayer "_unlock" targetLayer "_set" targetLayer "")
              (command "_.layer" "_make" targetLayer "")
            )
          )

          ;; Draw clipping rectangle directly on the target layer
          (setvar 'cmdecho 1)
          (setq preRect (entlast))
          (command "_.rectang")
          (while (> (getvar 'cmdactive) 0) (command pause))
          (setvar 'cmdecho 0)
          (setq rect (entlast))

          ;; Verify rectangle was drawn successfully
          (if (and rect (not (equal rect preRect)))
            (progn
              ;; Copy ONLY the viewport entity itself.
              ;; Omitting the old clipping boundary polyline prevents AutoCAD
              ;; from cloning it and leaving a duplicate polyline behind.
              (setq ssCopy (ssadd vpEnt))

              (setq preCopy (entlast))
              (command "_.copy" ssCopy "" "_non" '(0 0 0) "_non" '(0 0 0))
              (while (> (getvar 'cmdactive) 0) (command ""))

              ;; Identify the newly copied viewport
              (setq newVp nil)
              (setq e preCopy)
              (while (setq e (entnext e))
                (if (= (cdr (assoc 0 (entget e))) "VIEWPORT")
                  (setq newVp e)
                )
              )

              (if newVp
                (progn
                  ;; Assign viewport entity to the target layer and turn it on
                  (command "_.chprop" newVp "" "_layer" targetLayer "")
                  (command "_.mview" "_on" newVp "")

                  ;; Apply new rectangle boundary to the new viewport
                  (command "_.vpclip" newVp rect)
                  (while (> (getvar 'cmdactive) 0) (command ""))

                  (princ (strcat "\n[VR] Success: Viewport copied and clipped on layer \"" targetLayer "\"."))
                )
                (princ "\n[VR] Error: Failed to copy viewport.")
              )
            )
            (princ "\n[VR] Rectangle cancelled. No changes made.")
          )

          ;; Revert to the original working layer and restore settings
          (if (and (boundp 'CadSetup:SetCurrentLayerSafe) CadSetup:SetCurrentLayerSafe)
            (CadSetup:SetCurrentLayerSafe oldLayer)
            (setvar 'clayer oldLayer)
          )
          (setvar 'cmdecho oldCmdecho)
          (if (boundp 'CadSetup:UndoEnd)
            (CadSetup:UndoEnd)
            (vla-endundomark doc)
          )
        )
      )
    )
    (princ "\n[VR] Command must be used in Paper Space layout.")
  )
  (princ)
)

;; Alias: VR -> VPCLIPRECTANGLE
(defun c:VR ()
  (c:VPCLIPRECTANGLE)
)

(if *CadSetup-Debug*
  (princ "\n[04_Layout-Views.lsp] View navigation, dimensions, and layout tools loaded.")
)
(princ)
