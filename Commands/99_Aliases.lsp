;;; ==========================================================================
;;; 99_Aliases.lsp - Final Stage Consolidated Drafting Aliases & Overrides
;;; ==========================================================================
;;; Category : Final-Stage Ergonomic Shortcuts & Command Remappings
;;; Author   : Haseeb
;;; ==========================================================================
;;; NOTE: This file is loaded LAST (Stage 99) so that all foundational commands,
;;; drafting routines, block operations, layer tools, and utilities are already
;;; defined in memory before these aliases are registered.
;;; ==========================================================================

(vl-load-com)

;;; --------------------------------------------------------------------------
;;; 1. PRIMARY HIGH-FREQUENCY DRAFTING ALIASES
;;; --------------------------------------------------------------------------

;; 1. Copy
(defun c:C () (command "_.COPY") (princ))

;; 2. Match Properties
(defun c:Q () (command "_.MATCHPROP") (princ))

;; 3. Measure Geometry (Quick Mode)
(defun c:T () (command "_.MEASUREGEOM" "_QUICK") (princ))

;; 4. Rectangle
(defun c:R () (command "_.RECTANG") (princ))

;; 5. Trim
(defun c:TT () (command "_.TRIM") (princ))

;; 6. Dimension (Aligned)
(defun c:D () (command "_.DIMALIGNED") (princ))

;; 7. Move
(defun c:V () (command "_.MOVE") (princ))

;; 8. Block Editor (In-place Refedit)
(defun c:` () (command "_.REFEDIT") (princ))


;;; --------------------------------------------------------------------------
;;; 2. CORE DRAFTING & GEOMETRY
;;; --------------------------------------------------------------------------

(defun c:CC  () (command "_.CIRCLE")  (princ))
(defun c:RC  () (command "_.RECTANG") (princ))
(defun c:P   () (command "_.PLINE")   (princ))
(defun c:XL  () (command "_.XLINE")   (princ))


;;; --------------------------------------------------------------------------
;;; 3. ERGONOMIC MODIFICATION SHORTCUTS
;;; --------------------------------------------------------------------------

(defun c:XX  () (command "_.EXTEND")  (princ))
(defun c:RR  () (command "_.ROTATE")  (princ))
(defun c:SS  () (command "_.STRETCH") (princ))
(defun c:EE  () (command "_.ERASE")   (princ))
(defun c:AA  () (command "_.ALIGN")   (princ))
(defun c:MM  () (command "_.MIRROR")  (princ))
(defun c:OO  () (command "_.OFFSET")  (princ))
(defun c:WW  () (command "_.WIPEOUT") (princ))
(defun c:SC  () (command "_.SCALE")   (princ))
(defun c:EXP () (command "_.EXPLODE") (princ))
(defun c:PC  () (command "_.pedit" "_m" (ssget) "" "_c" "") (princ))


;;; --------------------------------------------------------------------------
;;; 4. INSTANT CORNER MAKER (Fillet & Chamfer 0)
;;; --------------------------------------------------------------------------

(defun c:FF  () (setvar "FILLETRAD" 0.0) (initcommandversion) (command "_.FILLET") (princ))
(defun c:F0  ()
  (if (vl-symbol-value 'c:FF)
    (c:FF)
    (progn (setvar "FILLETRAD" 0.0) (initcommandversion) (command "_.FILLET"))
  )
  (princ)
)
(defun c:CF0 () (setvar "CHAMFERA" 0.0) (setvar "CHAMFERB" 0.0) (initcommandversion) (command "_.CHAMFER") (princ))


;;; --------------------------------------------------------------------------
;;; 5. ZOOM & VIEW SHORTCUTS
;;; --------------------------------------------------------------------------

(defun c:ZE  () (command "_.ZOOM" "_E") (princ))
(defun c:ZZ  () (command "_.ZOOM" "_E") (princ))
(defun c:ZW  () (command "_.ZOOM" "_W") (princ))
(defun c:ZP  () (command "_.ZOOM" "_P") (princ))
(defun c:ZS  () (command "_.ZOOM" "_O") (princ))
(defun c:RE  () (command "_.REGEN")    (princ))
(defun c:REA () (command "_.REGENALL") (princ))


;;; --------------------------------------------------------------------------
;;; 6. DIMENSIONING & ANNOTATION SHORTCUTS
;;; --------------------------------------------------------------------------

(defun c:DA  () (command "_.DIMALIGNED")  (princ))
(defun c:DD  () (command "_.DIMLINEAR")   (princ))
(defun c:DR  () (command "_.DIMRADIUS")   (princ))
(defun c:DDI () (command "_.DIMDIAMETER") (princ))
(defun c:DAN () (command "_.DIMANGULAR")  (princ))
(defun c:DC  () (command "_.DIMCONTINUE") (princ))
(defun c:DE  () (command "_.DIMEDIT")     (princ))
(defun c:TE  () (command "_.TEXTEDIT")    (princ))


;;; --------------------------------------------------------------------------
;;; 7. OBJECT ISOLATION & VISIBILITY (Independent of Layers)
;;; --------------------------------------------------------------------------

(defun c:IS  () (command "_.ISOLATEOBJECTS")   (princ))
(defun c:HO  () (command "_.HIDEOBJECTS")      (princ))
(defun c:UN  () (command "_.UNISOLATEOBJECTS") (princ))


;;; --------------------------------------------------------------------------
;;; 8. QUICK LAYER CONTROL SHORTCUTS
;;; --------------------------------------------------------------------------

(defun c:11  () (command "_.LAYOFF") (princ))
(defun c:LO  () (command "_.LAYOFF") (princ))
(defun c:44  () (command "_.LAYON")  (princ))
(defun c:LON () (command "_.LAYON")  (princ))
(defun c:55  () (command "_.LAYFRZ") (princ))
(defun c:LF  () (command "_.LAYFRZ") (princ))
(defun c:66  () (command "_.LAYTHW") (princ))
(defun c:LTH () (command "_.LAYTHW") (princ))
(defun c:LAYC () (command "_.LAYCUR") (princ))
(defun c:LM  () (command "_.LAYMCH") (princ))
(defun c:LLK () (command "_.LAYLCK") (princ))
(defun c:LUK () (command "_.LAYULK") (princ))


;;; --------------------------------------------------------------------------
;;; 9. PRODUCTIVITY & FILE UTILITIES
;;; --------------------------------------------------------------------------

(defun c:QS  () (command "_.QSAVE") (princ))
(defun c:CL  () (command "_.CLOSE") (princ))

(princ "\n[99_Aliases.lsp] Consolidated personal aliases loaded successfully.")
(princ)
