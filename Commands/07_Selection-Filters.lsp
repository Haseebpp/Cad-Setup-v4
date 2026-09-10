;;; ==========================================================================
;;; SELECTION-FILTERS.LSP - Smart Entity Filter & Isolation Engine
;;; ==========================================================================
;;; Category : Selection & Filtering
;;; Author   : Haseeb
;;; ==========================================================================

(vl-load-com)

;;; --------------------------------------------------------------------------
;;; 1. CORE FILTERING ENGINE
;;; --------------------------------------------------------------------------

(defun filter-selection (cmd criteria isolate label / *error* oldCmd ss newSS i ent match)
  (setq oldCmd (getvar 'cmdecho))

  ;; Localized Error Handler & Variable Restoration
  (defun *error* (msg)
    (if oldCmd (setvar 'cmdecho oldCmd))
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[" cmd "] Error: " msg))
    )
    (princ)
  )

  (setvar 'cmdecho 0)
  (setq ss (ssget "_I"))
  (if (not ss)
    (progn
      (princ (strcat "\nSelect objects to "
                     (if isolate (strcat "isolate " label "(s): ")
                                 (strcat "remove " label "(s) from: "))))
      (setq ss (ssget))
    )
  )
  (if ss
    (progn
      (setq newSS (ssadd)
            i     0)
      (while (< i (sslength ss))
        (setq ent   (ssname ss i)
              match (if (= (type criteria) 'STR)
                      (wcmatch (cdr (assoc 0 (entget ent))) criteria)
                      (apply criteria (list ent))))
        (if (if isolate match (not match))
          (ssadd ent newSS)
        )
        (setq i (1+ i))
      )
      (if (> (sslength newSS) 0)
        (progn
          (sssetfirst nil newSS)
          (princ (strcat "\n[" cmd "] " (itoa (sslength newSS))
                         (if isolate (strcat " " label "(s) selected.") " Object(s) retained.")))
        )
        (progn
          (sssetfirst nil nil)
          (princ (strcat "\n[" cmd "] No "
                         (if isolate (strcat label "(s) found in selection.")
                                     (strcat "objects remaining after removing " label "(s)."))))
        )
      )
    )
    (princ (strcat "\n[" cmd "] No objects selected."))
  )
  (setvar 'cmdecho oldCmd)
  (princ)
)

;;; --------------------------------------------------------------------------
;;; 2. DETECTION PREDICATES
;;; --------------------------------------------------------------------------

;; Revision cloud detection
(defun is-revcloud (ent / el etype xd)
  (setq el    (entget ent '("*"))
        etype (cdr (assoc 0 el)))
  (if (= etype "LWPOLYLINE")
    (and
      (setq xd (assoc -3 el))
      (vl-some
        '(lambda (app)
           (wcmatch (strcase (car app)) "*REVCLOUD*,*CLOUD*,*REVISION*")
         )
        (cdr xd)
      )
    )
  )
)

;; Annotation detection predicate
(defun is-annotation (ent / etype)
  (setq etype (cdr (assoc 0 (entget ent))))
  (or
    ;; Standard annotation entity types
    (wcmatch etype "DIMENSION,LEADER,MULTILEADER,TEXT,MTEXT,TOLERANCE,TABLE")
    ;; Revision clouds
    (is-revcloud ent)
  )
)

;;; --------------------------------------------------------------------------
;;; 3. FILTER & ISOLATION COMMANDS
;;; --------------------------------------------------------------------------

;; Dimensions: SR = Isolate, SRI = Exclude
(defun c:SR  () (filter-selection "SR"  "DIMENSION"          T   "Dimension"))
(defun c:SRI () (filter-selection "SRI" "DIMENSION"          nil "Dimension"))

;; Leaders (Standard & Multileader): SL = Isolate, SLI = Exclude
(defun c:SL  () (filter-selection "SL"  "LEADER,MULTILEADER" T   "Leader"))
(defun c:SLI () (filter-selection "SLI" "LEADER,MULTILEADER" nil "Leader"))

;; Block References: SB = Isolate, SBI = Exclude
(defun c:SB  () (filter-selection "SB"  "INSERT"             T   "Block"))
(defun c:SBI () (filter-selection "SBI" "INSERT"             nil "Block"))

;; Hatches: SH = Isolate, SHI = Exclude
(defun c:SH  () (filter-selection "SH"  "HATCH"              T   "Hatch"))
(defun c:SHI () (filter-selection "SHI" "HATCH"              nil "Hatch"))

;; All Annotations: SA = Isolate, SAI = Exclude
(defun c:SA  () (filter-selection "SA"  'is-annotation       T   "Annotation"))
(defun c:SAI () (filter-selection "SAI" 'is-annotation       nil "Annotation"))

;; Wipeouts: SW = Isolate, SWI = Exclude
(defun c:SW  () (filter-selection "SW"  "WIPEOUT"            T   "Wipeout"))
(defun c:SWI () (filter-selection "SWI" "WIPEOUT"            nil "Wipeout"))

(princ "\n[07_Selection-Filters.lsp] Smart selection filter engine loaded.")
(princ)
