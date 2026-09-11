;;; ==========================================================================
;;; Help_Selection.lsp - Selection Set Utilities & Entity Filter Engine
;;; Layer: Helpers (Level 1)
;;; ==========================================================================

(vl-load-com)

;; ===========================================================================
;; SELECTION SET CONVERSION & ITERATION
;; ===========================================================================

;; CadSetup:SsToList - Converts an AutoCAD selection set into a standard LISP list
(defun CadSetup:SsToList (ss / i lst)
  (if (and ss (= (type ss) 'PICKSET))
    (progn
      (setq i (sslength ss))
      (while (> i 0)
        (setq i (1- i))
        (setq lst (cons (ssname ss i) lst))
      )
      lst
    )
    nil
  )
)

;; CadSetup:IterateSelectionSet - Iterates over every entity in a selection set
(defun CadSetup:IterateSelectionSet (ss func / i ent)
  (if (and ss (= (type ss) 'PICKSET) func)
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (apply func (list ent))
        (setq i (1+ i))
      )
    )
  )
)

;; ===========================================================================
;; DETECTION PREDICATES
;; ===========================================================================

;; CadSetup:IsRevcloud - Checks if an entity is a Revision Cloud polyline
(defun CadSetup:IsRevcloud (ent / el etype xd)
  (if (and ent (= (type ent) 'ENAME))
    (progn
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
    nil
  )
)

;; CadSetup:IsAnnotation - Checks if an entity is an annotation or revision cloud
(defun CadSetup:IsAnnotation (ent / etype)
  (if (and ent (= (type ent) 'ENAME))
    (progn
      (setq etype (cdr (assoc 0 (entget ent))))
      (or
        (wcmatch etype "DIMENSION,LEADER,MULTILEADER,TEXT,MTEXT,TOLERANCE,TABLE")
        (CadSetup:IsRevcloud ent)
      )
    )
    nil
  )
)

;; ===========================================================================
;; GENERALIZED SELECTION FILTER ENGINE
;; ===========================================================================

;; CadSetup:FilterSelection
;; Filters the current pickfirst set or prompts user for selection.
;; Parameters:
;;   cmd      : Command calling identifier string (e.g., "SR", "SL")
;;   criteria : DXF 0 match pattern (string) or predicate function taking ENAME
;;   isolate  : T to keep matching entities, nil to exclude them
;;   label    : Human-readable entity description (e.g., "Dimension", "Block")
(defun CadSetup:FilterSelection (cmd criteria isolate label / *error* oldCmd ss newSS i ent match)
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

(princ "\n[Helpers/Help_Selection.lsp] Selection filter & iteration engine loaded.")
(princ)
