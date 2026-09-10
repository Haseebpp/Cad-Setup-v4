;;; ==========================================================================
;;; DRAFTING-ALIASES.LSP - High-Frequency Ergonomic Drafting Commands & Aliases
;;; ==========================================================================
;;; Category : Core Drafting & Geometry Editing
;;; Author   : Haseeb
;;; ==========================================================================

(vl-load-com)

;;; NOTE: High-frequency single-line aliases and overrides (C, CC, Q, D, RC,
;;; P, XL, V, TT, XX, RR, SS, EE, AA, MM, OO, WW, SC, EXP, PC, FF, F0, CF0)
;;; have been consolidated into Commands/99_Aliases.lsp for easy editing.
;;; --------------------------------------------------------------------------

;; 'JJ' - Instant Polyline Join for connected lines/arcs/polylines
(defun c:JJ (/ *error* doc ss oldCmd)
  (vl-load-com)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq oldCmd (getvar 'cmdecho))

  ;; Localized Error Handler & Undo Stack Safety
  (defun *error* (msg)
    (if oldCmd (setvar 'cmdecho oldCmd))
    (if (and doc (= (type doc) 'VLA-OBJECT))
      (vl-catch-all-apply 'vla-endundomark (list doc))
    )
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[JJ] Error: " msg))
    )
    (princ)
  )

  (setvar 'cmdecho 0)
  (setq ss (ssget "_I"))
  (if (not ss)
    (progn
      (princ "\nSelect connected lines/arcs/polylines to join: ")
      (setq ss (ssget '((0 . "LINE,ARC,LWPOLYLINE,POLYLINE,SPLINE"))))
    )
  )
  (if ss
    (progn
      (vla-startundomark doc)
      (command "._join" ss "")
      (vla-endundomark doc)
      (princ (strcat "\n[JJ] Joined " (itoa (sslength ss)) " entities."))
    )
    (princ "\n[JJ] No objects selected.")
  )
  (setvar 'cmdecho oldCmd)
  (princ)
)

;;; --------------------------------------------------------------------------
;;; 2. BREAK AT POINT (One-Click Single Point Split)
;;; --------------------------------------------------------------------------

(defun c:BB ( / ent pt )
  (princ "\nSelect object to break: ")
  (if (setq ent (entsel))
    (progn
      (setq pt (getpoint "\nSpecify break point: "))
      (if pt
        (command "_.BREAK" ent "_F" pt "@")
      )
    )
  )
  (princ)
)
(defun c:BAP () (c:BB))

;;; --------------------------------------------------------------------------
;;; 3. RAPID FIXED-ANGLE ROTATIONS
;;; --------------------------------------------------------------------------

(defun c:R90 ( / *error* doc ss pt )
  (vl-load-com)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))

  (defun *error* (msg)
    (if (and doc (= (type doc) 'VLA-OBJECT))
      (vl-catch-all-apply 'vla-endundomark (list doc))
    )
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[R90] Error: " msg))
    )
    (princ)
  )

  (prompt "\nSelect objects to rotate 90°: ")
  (if (setq ss (ssget))
    (progn
      (setq pt (getpoint "\nSpecify base point: "))
      (if pt
        (progn
          (vla-startundomark doc)
          (command "_.rotate" ss "" pt 90)
          (vla-endundomark doc)
        )
      )
    )
  )
  (princ)
)

(defun c:R180 ( / *error* doc ss pt )
  (vl-load-com)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))

  (defun *error* (msg)
    (if (and doc (= (type doc) 'VLA-OBJECT))
      (vl-catch-all-apply 'vla-endundomark (list doc))
    )
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[R180] Error: " msg))
    )
    (princ)
  )

  (prompt "\nSelect objects to rotate 180°: ")
  (if (setq ss (ssget))
    (progn
      (setq pt (getpoint "\nSpecify base point: "))
      (if pt
        (progn
          (vla-startundomark doc)
          (command "_.rotate" ss "" pt 180)
          (vla-endundomark doc)
        )
      )
    )
  )
  (princ)
)

(princ "\n[02_Drafting-Aliases.lsp] High-speed drafting shortcuts loaded.")
(princ)
