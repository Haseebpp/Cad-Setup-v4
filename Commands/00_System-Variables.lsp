;;; ==========================================================================
;;; 00_System-Variables.lsp - Drafter Environment Configuration & Controls
;;; Standardizes drafting environment, system variables & performance
;;; ==========================================================================

(vl-load-com)

;; Safe system variable assignment helper
(defun SetSysVar (var val)
  (if (getvar var)
    (if (/= (getvar var) val)
      (setvar var val)
    )
  )
)

;;; --------------------------------------------------------------------------
;;; 1. CORE DRAFTING & SELECTION BEHAVIOR
;;; --------------------------------------------------------------------------
(SetSysVar "CMDECHO"          0)    ; Suppress command line echoes during routine execution
(SetSysVar "PICKFIRST"        1)    ; Enable noun/verb selection (select before command)
(SetSysVar "PICKADD"          2)    ; Accumulate selection sets without holding Shift
(SetSysVar "HIGHLIGHT"        1)    ; Highlight selected entities
(SetSysVar "FILEDIA"          1)    ; Force standard file dialog boxes
(SetSysVar "ATTDIA"           1)    ; Force dialog box for block attribute entry
(SetSysVar "MIRRTEXT"         0)    ; Keep text legible when mirroring
(SetSysVar "EDGEMODE"         1)    ; Trim/Extend to implied intersecting edges
(SetSysVar "DYNMODE"          3)    ; Full Dynamic Input (pointer & dimensional input)
(SetSysVar "DRAGMODE"         2)    ; Auto dynamic dragging
(SetSysVar "SNAPSTYLE"        0)    ; Standard 2D rectangular grid

;;; --------------------------------------------------------------------------
;;; 2. CURSOR ERGONOMICS & SIZING
;;; --------------------------------------------------------------------------
(SetSysVar "CURSORSIZE"      25)    ; Full-screen crosshair (25%)
(SetSysVar "PICKBOX"          6)    ; Object selection target box size (6 px)
(SetSysVar "GRIPSIZE"         7)    ; Grip box display size (7 px)
(SetSysVar "APERTURE"        10)    ; Object snap target box aperture (10 px)

;;; --------------------------------------------------------------------------
;;; 3. SNAPS, GRID & DRAFTING TOGGLES
;;; --------------------------------------------------------------------------
(SetSysVar "OSMODE"        2215)    ; Standard 2D drafting snap suite (End, Mid, Cen, Int, Perp, Ext)
(SetSysVar "OSOPTIONS"        7)    ; Suppress OSNAP on hatches and underlays (speeds up snapping)
(SetSysVar "GRIDMODE"         1)    ; Background grid display ON
(SetSysVar "SNAPMODE"         0)    ; Snap to grid OFF
(SetSysVar "ORTHOMODE"        1)    ; Orthographic constraint ON

;;; --------------------------------------------------------------------------
;;; 4. DISPLAY, LINETYPES & ENTITY DEFAULTS
;;; --------------------------------------------------------------------------
(SetSysVar "LWDISPLAY"           0) ; Default lineweight display off for performance
(SetSysVar "TRANSPARENCYDISPLAY" 1) ; Show layer transparency
(SetSysVar "MSLTSCALE"           1) ; Scale linetypes by annotative scale in model
(SetSysVar "PSLTSCALE"           1) ; Scale linetypes uniformly across viewports
(SetSysVar "LTSCALE"           1.0) ; Base linetype factor
(SetSysVar "ANNOALLVISIBLE"      1) ; Keep annotative elements visible during drafting
(SetSysVar "CELWEIGHT"          -1) ; Lock new geometry to ByLayer (-1)
(SetSysVar "CECOLOR"     "BYLAYER") ; Current entity color ByLayer
(SetSysVar "CELTYPE"     "BYLAYER") ; Current entity linetype ByLayer
(SetSysVar "DRAWORDERCTL"        3) ; Enable full drawing order display
(SetSysVar "LAYLOCKFADECTL"     50) ; 50% fade on locked layers for contrast

;;; --------------------------------------------------------------------------
;;; 5. PERFORMANCE & VISUAL OPTIMIZATION
;;; --------------------------------------------------------------------------
(SetSysVar "SELECTIONPREVIEW"    3) ; Preview selection under cursor
(SetSysVar "HPQUICKPREVIEW"      0) ; Disable laggy hatch hover preview on dense drawings

(princ "\n[00_System-Variables.lsp] Drafter environment & system variables optimized.")
(princ)
