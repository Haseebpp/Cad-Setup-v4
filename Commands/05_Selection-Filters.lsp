;;; ==========================================================================
;;; 05_Selection-Filters.lsp - Smart Entity Filter & Isolation Shortcuts
;;; Part of Cad-Setup-v3 Horizontal Layered Architecture
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

;; Hatches: SH = Isolate, SHI = Exclude
(defun c:SH  () (CadSetup:FilterSelection "SH"  "HATCH"              T   "Hatch"))
(defun c:SHI () (CadSetup:FilterSelection "SHI" "HATCH"              nil "Hatch"))

;; All Annotations: SA = Isolate, SAI = Exclude
(defun c:SA  () (CadSetup:FilterSelection "SA"  'CadSetup:IsAnnotation T   "Annotation"))
(defun c:SAI () (CadSetup:FilterSelection "SAI" 'CadSetup:IsAnnotation nil "Annotation"))

;; Wipeouts: SW = Isolate, SWI = Exclude
(defun c:SW  () (CadSetup:FilterSelection "SW"  "WIPEOUT"            T   "Wipeout"))
(defun c:SWI () (CadSetup:FilterSelection "SWI" "WIPEOUT"            nil "Wipeout"))

(princ "\n[05_Selection-Filters.lsp] Smart selection filter shortcuts loaded.")
(princ)
