;;; ==========================================================================
;;; 01_Undo-Manager.lsp - Safe, Re-entrant AutoCAD Undo Transaction Manager
;;; Layer: Core (Level 0)
;;; ==========================================================================

(vl-load-com)

;; Global transaction depth counter
(setq *CadSetup-Undo-Depth* 0)

;; ===========================================================================
;; TRANSACTION START
;; ===========================================================================

;; CadSetup:UndoStart
;; Begins an undo grouping on the active drawing document.
;; Safely handles re-entrant/nested calls without leaving corrupt undo states.
(defun CadSetup:UndoStart ( / acadApp doc )
  (setq acadApp (vlax-get-acad-object))
  (if acadApp
    (setq doc (vla-get-activedocument acadApp))
  )
  (if (and doc (= (type doc) 'VLA-OBJECT))
    (progn
      (if (<= *CadSetup-Undo-Depth* 0)
        (progn
          (vl-catch-all-apply 'vla-startundomark (list doc))
          (setq *CadSetup-Undo-Depth* 1)
        )
        (setq *CadSetup-Undo-Depth* (1+ *CadSetup-Undo-Depth*))
      )
      T
    )
    nil
  )
)

;; ===========================================================================
;; TRANSACTION END
;; ===========================================================================

;; CadSetup:UndoEnd
;; Closes an undo mark. Commits group only when nesting returns to zero.
(defun CadSetup:UndoEnd ( / acadApp doc )
  (setq acadApp (vlax-get-acad-object))
  (if acadApp
    (setq doc (vla-get-activedocument acadApp))
  )
  (if (and doc (= (type doc) 'VLA-OBJECT))
    (progn
      (setq *CadSetup-Undo-Depth* (1- *CadSetup-Undo-Depth*))
      (if (<= *CadSetup-Undo-Depth* 0)
        (progn
          (vl-catch-all-apply 'vla-endundomark (list doc))
          (setq *CadSetup-Undo-Depth* 0)
        )
      )
      T
    )
    nil
  )
)

;; ===========================================================================
;; EMERGENCY RESET (FOR ERROR HANDLERS)
;; ===========================================================================

;; CadSetup:UndoReset
;; Called inside (defun *error* ...) blocks to terminate any open marks.
(defun CadSetup:UndoReset ( / acadApp doc )
  (setq acadApp (vlax-get-acad-object))
  (if acadApp
    (setq doc (vla-get-activedocument acadApp))
  )
  (if (and doc (= (type doc) 'VLA-OBJECT))
    (progn
      (while (> *CadSetup-Undo-Depth* 0)
        (vl-catch-all-apply 'vla-endundomark (list doc))
        (setq *CadSetup-Undo-Depth* (1- *CadSetup-Undo-Depth*))
      )
      ;; Extra guard to guarantee no lingering undo marks
      (vl-catch-all-apply 'vla-endundomark (list doc))
      (setq *CadSetup-Undo-Depth* 0)
    )
  )
  (princ)
)

(if *CadSetup-Debug*
  (princ "\n[Core/01_Undo-Manager.lsp] Re-entrant Undo transaction manager loaded.")
)
(princ)
