;;; ==========================================================================
;;; 05_Selection-Filters.lsp - Smart Entity Filter & Isolation Shortcuts
;;; Layer: Commands (Priority 05)
;;; Author   : Haseeb
;;; ==========================================================================

(vl-load-com)

;;; --------------------------------------------------------------------------
;;; FILTER & ISOLATION COMMANDS
;;; Powered by CadSetup:FilterSelection from Helpers/Help_Selection.lsp
;;; --------------------------------------------------------------------------

;; Dimensions: SR = Isolate, SRI = Exclude
(defun c:SR  () (CadSetup:FilterSelection "SR"  "DIMENSION"          T   "Dimension"))
(defun c:SRI () (CadSetup:FilterSelection "SRI" "DIMENSION"          nil "Dimension"))

;; Leaders (Standard & Multileader): SL = Isolate, SLI = Exclude
(defun c:SL  () (CadSetup:FilterSelection "SL"  "LEADER,MULTILEADER" T   "Leader"))
(defun c:SLI () (CadSetup:FilterSelection "SLI" "LEADER,MULTILEADER" nil "Leader"))

;; Block References: SB = Isolate, SBI = Exclude
(defun c:SB  () (CadSetup:FilterSelection "SB"  "INSERT"             T   "Block"))
(defun c:SBI () (CadSetup:FilterSelection "SBI" "INSERT"             nil "Block"))

;; Hatches: SHT / SH = Isolate, SHTI / SHI = Exclude
(defun c:SHT  () (CadSetup:FilterSelection "SHT"  "HATCH"              T   "Hatch"))
(defun c:SHTI () (CadSetup:FilterSelection "SHTI" "HATCH"              nil "Hatch"))
(defun c:SH   () (c:SHT))
(defun c:SHI  () (c:SHTI))

;; Text (Single-Line & MText): ST = Isolate, STI = Exclude
(defun c:ST   () (CadSetup:FilterSelection "ST"   "TEXT,MTEXT"         T   "Text"))
(defun c:STI  () (CadSetup:FilterSelection "STI"  "TEXT,MTEXT"         nil "Text"))

;; External References (Xrefs): SX = Isolate, SXI = Exclude
(defun c:SX   () (CadSetup:FilterSelection "SX"   'CadSetup:IsXref     T   "Xref"))
(defun c:SXI  () (CadSetup:FilterSelection "SXI"  'CadSetup:IsXref     nil "Xref"))

;; Polylines (2D/3D & Lightweight): SP = Isolate, SPI = Exclude
(defun c:SP   () (CadSetup:FilterSelection "SP"   "*POLYLINE"          T   "Polyline"))
(defun c:SPI  () (CadSetup:FilterSelection "SPI"  "*POLYLINE"          nil "Polyline"))

;; Lines: SLN = Isolate, SLNI = Exclude
(defun c:SLN  () (CadSetup:FilterSelection "SLN"  "LINE"               T   "Line"))
(defun c:SLNI () (CadSetup:FilterSelection "SLNI" "LINE"               nil "Line"))

;; Circles & Arcs: SCIR = Isolate, SCIRI = Exclude
(defun c:SCIR  () (CadSetup:FilterSelection "SCIR"  "CIRCLE,ARC"       T   "Circle/Arc"))
(defun c:SCIRI () (CadSetup:FilterSelection "SCIRI" "CIRCLE,ARC"       nil "Circle/Arc"))

;; Viewports: SV = Isolate, SVI = Exclude
(defun c:SV   () (CadSetup:FilterSelection "SV"   "VIEWPORT"           T   "Viewport"))
(defun c:SVI  () (CadSetup:FilterSelection "SVI"  "VIEWPORT"           nil "Viewport"))

;; Tables: STB = Isolate, STBI = Exclude
(defun c:STB  () (CadSetup:FilterSelection "STB"  "TABLE"              T   "Table"))
(defun c:STBI () (CadSetup:FilterSelection "STBI" "TABLE"              nil "Table"))

;; Wipeouts: SW = Isolate, SWI = Exclude
(defun c:SW   () (CadSetup:FilterSelection "SW"   "WIPEOUT"            T   "Wipeout"))
(defun c:SWI  () (CadSetup:FilterSelection "SWI"  "WIPEOUT"            nil "Wipeout"))

;; All Annotations: SA = Isolate, SAI = Exclude
(defun c:SA   () (CadSetup:FilterSelection "SA"   'CadSetup:IsAnnotation T   "Annotation"))
(defun c:SAI  () (CadSetup:FilterSelection "SAI"  'CadSetup:IsAnnotation nil "Annotation"))

;; Vertical / Taller Objects (Height > Width): SVERT = Isolate, SVERTI = Exclude
(defun c:SVERT  () (CadSetup:FilterSelection "SVERT"  'CadSetup:IsVertical   T   "Vertical/Taller Object"))
(defun c:SVERTI () (CadSetup:FilterSelection "SVERTI" 'CadSetup:IsVertical   nil "Vertical/Taller Object"))

;; Horizontal / Wider Objects (Width > Height): SHOR = Isolate, SHORI = Exclude
(defun c:SHOR   () (CadSetup:FilterSelection "SHOR"   'CadSetup:IsHorizontal T   "Horizontal/Wider Object"))
(defun c:SHORI  () (CadSetup:FilterSelection "SHORI"  'CadSetup:IsHorizontal nil "Horizontal/Wider Object"))

(if *CadSetup-Debug*
  (princ "\n[05_Selection-Filters.lsp] Smart selection filter shortcuts loaded.")
)
(princ)
