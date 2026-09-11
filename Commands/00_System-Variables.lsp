;;; ==========================================================================
;;; 00_System-Variables.lsp - Drafter Environment Configuration & Controls
;;; Standardizes drafting environment, system variables & performance
;;; Layer: Commands (Priority 00)
;;; ==========================================================================

(vl-load-com)

;; ===========================================================================
;; SYSTEM VARIABLE APPLICATION ENGINE
;; Consumes pure data dictionary *CadSetup-SysVars-Data* from Database/Db_SysVars.lsp
;; ===========================================================================

(defun CadSetup:ApplySystemVariables ( / varList count vName vVal vCat vDesc )
  (setq varList (CadSetup:GetRecommendedSysVars)
        count   0)

  (if varList
    (progn
      (foreach item varList
        (setq vName (nth 0 item)
              vVal  (nth 1 item)
              vCat  (nth 2 item)
              vDesc (nth 3 item))

        (if (CadSetup:SafeSetVar vName vVal)
          (setq count (1+ count))
        )
      )
      count
    )
    0
  )
)

;; Automatically apply system variables on load
(CadSetup:ApplySystemVariables)

;; Command to re-apply recommended system variables on demand
(defun c:APPLY-SYSVARS ()
  (setq count (CadSetup:ApplySystemVariables))
  (princ (strcat "\n[APPLY-SYSVARS] Recommended system variables enforced (" (itoa count) " checked/updated)."))
  (princ)
)
(defun c:RSV () (c:APPLY-SYSVARS))

(if *CadSetup-Debug*
  (princ "\n[00_System-Variables.lsp] Drafter environment & system variables optimized.")
)
(princ)
