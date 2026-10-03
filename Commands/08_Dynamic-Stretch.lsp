;;; ==========================================================================
;;; 08_Dynamic-Stretch.lsp - Multi-Boundary Dynamic Stretch Engine
;;; Architecture: Live Viewfinder Preview & Equalized Opposing Pair Engine
;;; Layer: Commands (Priority 08)
;;; Author   : Haseeb
;;; Commands : DYNSTRETCH, DYNAMICSTRETCH (Aliases: DS, DSTRETCH)
;;; ==========================================================================
;;; ARCHITECTURAL NOTE:
;;; Advanced stretching engine featuring 60 FPS live interactive preview:
;;;   1. Multi-boundary box selection with multi-color palette (Green, Cyan,
;;;      Magenta, Yellow, Red, Blue) and architectural HUD viewfinder brackets.
;;;   2. Entity-level native highlight glowing (redraw ent 3) on candidate blocks.
;;;   3. LIVE interactive preview during point picking & direction displacement:
;;;      - Real-time yellow vector arrow tracking from base point to cursor.
;;;      - Opposing pair blocks: Live dynamic green dual-headed arrows &
;;;        real-time deformed ghost boundaries showing active expansion.
;;;      - Single linear blocks: Live dynamic yellow directional arrows &
;;;        stretch ghost boundaries.
;;;      - Fully enclosed blocks: Live magenta translation ghost box tracking
;;;        with cursor motion.
;;;      - Real-time distance and angle HUD in status line (grtext).
;;;      - Integrated Ortho mode, Object Snapping, and Direct Distance Entry.
;;;   4. Equalized Opposing Pair Engine:
;;;      - Parameter vector probing (dstr:probe-vector) in UCS.
;;;      - Pairs opposing vectors (u1 . u2 <= -0.95).
;;;      - Equalizes total length and automatically shifts block insertion point
;;;        along active axis so the opposite end remains completely stationary.
;;;   5. Non-block crossing geometry: executes native AutoCAD STRETCH with
;;;      blocks excluded.
;;;   6. Re-entrant undo integration (CadSetup:UndoStart / CadSetup:UndoEnd)
;;;      and safe environment restoration on cancel or exit.
;;; ==========================================================================

(vl-load-com)

;;; ==========================================================================
;;; 1. VISUALIZATION SYSTEM: MULTI-COLOR PALETTES, HUD BRACKETS & ARROWS
;;; ==========================================================================

;; Helper: Multi-color palette for sequential crossing boxes
;; Palette: 3=Green, 4=Cyan, 6=Magenta, 2=Yellow, 1=Red, 5=Blue, 30=Orange
(defun dstr:get-box-color (idx / colors)
  (setq colors '(3 4 6 2 1 5 30))
  (nth (rem idx (length colors)) colors))

;; Helper: Draw precision HUD boundary box with corner viewfinders & crossing pattern
(defun dstr:draw-box (p1 p2 col idx / x1 x2 y1 y2 dx dy pa pb pc pd
                                      bLen mx my mLen)
  (setq x1 (min (car p1) (car p2))
        x2 (max (car p1) (car p2))
        y1 (min (cadr p1) (cadr p2))
        y2 (max (cadr p1) (cadr p2))
        dx (- x2 x1)
        dy (- y2 y1))
  (setq pa (list x1 y1 0.0)
        pb (list x2 y1 0.0)
        pc (list x2 y2 0.0)
        pd (list x1 y2 0.0))

  ;; 1. Primary outer boundary rectangle (highlighted)
  (grdraw pa pb col 1)
  (grdraw pb pc col 1)
  (grdraw pc pd col 1)
  (grdraw pd pa col 1)

  ;; 2. Crossing diagonals (AutoCAD Crossing Window indicator)
  (if (and (> dx 1e-4) (> dy 1e-4))
    (progn
      (grdraw pa pc col 0)
      (grdraw pb pd col 0)

      ;; 3. Architectural corner bracket viewfinders
      (setq bLen (min (* (min dx dy) 0.15) (* (getvar "VIEWSIZE") 0.035)))
      (if (> bLen 1e-4)
        (progn
          ;; pa corner (bottom-left)
          (grdraw pa (list (+ x1 bLen) y1 0.0) col 1)
          (grdraw pa (list x1 (+ y1 bLen) 0.0) col 1)
          ;; pb corner (bottom-right)
          (grdraw pb (list (- x2 bLen) y1 0.0) col 1)
          (grdraw pb (list x2 (+ y1 bLen) 0.0) col 1)
          ;; pc corner (top-right)
          (grdraw pc (list (- x2 bLen) y2 0.0) col 1)
          (grdraw pc (list x2 (- y2 bLen) 0.0) col 1)
          ;; pd corner (top-left)
          (grdraw pd (list (+ x1 bLen) y2 0.0) col 1)
          (grdraw pd (list x1 (- y2 bLen) 0.0) col 1)

          ;; 4. Midpoint edge notch markers
          (setq mx   (* 0.5 (+ x1 x2))
                my   (* 0.5 (+ y1 y2))
                mLen (* 0.4 bLen))
          (grdraw (list (- mx mLen) y1 0.0) (list (+ mx mLen) y1 0.0) col 1)
          (grdraw (list (- mx mLen) y2 0.0) (list (+ mx mLen) y2 0.0) col 1)
          (grdraw (list x1 (- my mLen) 0.0) (list x1 (+ my mLen) 0.0) col 1)
          (grdraw (list x2 (- my mLen) 0.0) (list x2 (+ my mLen) 0.0) col 1)
        )
      )
    )
  )
)

;; Helper: Redraw all active boundary boxes in multi-color palette
(defun dstr:draw-all-boxes (box-list / i box)
  (setq i 0)
  (foreach box box-list
    (dstr:draw-box (car box) (cadr box) (dstr:get-box-color i) (1+ i))
    (setq i (1+ i))))

;; Helper: Draw 2D Vector Arrow in UCS with closed barb head
(defun dstr:draw-arrow (p1 p2 col / d ang head-len a1 a2 w1 w2)
  (setq d (distance p1 p2))
  (if (> d 1e-4)
    (progn
      (setq ang      (angle p1 p2)
            head-len (min (* d 0.35) (* (getvar "VIEWSIZE") 0.04))
            a1       (+ ang (* 5.0 (/ pi 6.0)))  ; 150 degrees
            a2       (- ang (* 5.0 (/ pi 6.0)))  ; 210 degrees
            w1       (polar p2 a1 head-len)
            w2       (polar p2 a2 head-len))
      (grdraw p1 p2 col 1)      ; Arrow shaft (highlighted)
      (grdraw p2 w1 col 1)      ; Left barb
      (grdraw p2 w2 col 1)      ; Right barb
      (grdraw w1 w2 col 1))))   ; Base closure

;; Helper: Draw solid ghost bounding box translated by (dx, dy)
(defun dstr:draw-ghost-box (pts dx dy col / p1 p2 p3 p4 len)
  (setq p1 (list (+ (car (nth 0 pts)) dx) (+ (cadr (nth 0 pts)) dy) 0.0)
        p2 (list (+ (car (nth 1 pts)) dx) (+ (cadr (nth 1 pts)) dy) 0.0)
        p3 (list (+ (car (nth 2 pts)) dx) (+ (cadr (nth 2 pts)) dy) 0.0)
        p4 (list (+ (car (nth 3 pts)) dx) (+ (cadr (nth 3 pts)) dy) 0.0))
  (grdraw p1 p2 col 1)
  (grdraw p2 p3 col 1)
  (grdraw p3 p4 col 1)
  (grdraw p4 p1 col 1)
  (setq len (* (getvar "VIEWSIZE") 0.015))
  (grdraw (list (- (car p1) len) (cadr p1) 0.0) (list (+ (car p1) len) (cadr p1) 0.0) col 0)
  (grdraw (list (car p1) (- (cadr p1) len) 0.0) (list (car p1) (+ (cadr p1) len) 0.0) col 0)
  (grdraw (list (- (car p3) len) (cadr p3) 0.0) (list (+ (car p3) len) (cadr p3) 0.0) col 0)
  (grdraw (list (car p3) (- (cadr p3) len) 0.0) (list (car p3) (+ (cadr p3) len) 0.0) col 0)
)

;; Helper: Draw deformed ghost rectangle along active expansion axis
(defun dstr:draw-deformed-ghost (corners ux uy delta col / s-list s-mid new-pts p1 p2 p3 p4 pt s)
  (setq s-list (mapcar '(lambda (p) (+ (* (car p) ux) (* (cadr p) uy))) corners))
  (setq s-mid  (* 0.5 (+ (apply 'min s-list) (apply 'max s-list))))
  (setq new-pts
    (mapcar
      '(lambda (p)
         (setq s (+ (* (car p) ux) (* (cadr p) uy)))
         (if (> s s-mid)
           (list (+ (car p) (* delta ux)) (+ (cadr p) (* delta uy)) 0.0)
           (list (car p) (cadr p) 0.0)))
      corners))
  (setq p1 (nth 0 new-pts)
        p2 (nth 1 new-pts)
        p3 (nth 2 new-pts)
        p4 (nth 3 new-pts))
  (grdraw p1 p2 col 1)
  (grdraw p2 p3 col 1)
  (grdraw p3 p4 col 1)
  (grdraw p4 p1 col 1)
  ;; Subtle crossing diagonals on ghost
  (grdraw p1 p3 col 0)
  (grdraw p2 p4 col 0)
)

;;; ==========================================================================
;;; 2. LIVE INTERACTION ENGINE: GRREAD, ORTHO, OSNAP & DIRECT DISTANCE
;;; ==========================================================================

;; Helper: Apply orthogonal constraint if ORTHOMODE is active
(defun dstr:apply-ortho (p1 p2 / dx dy)
  (setq dx (abs (- (car p2) (car p1)))
        dy (abs (- (cadr p2) (cadr p1))))
  (if (>= dx dy)
    (list (car p2) (cadr p1) (caddr p1))
    (list (car p1) (cadr p2) (caddr p1))))

;; Helper: Snap point using active AutoCAD object snap modes
(defun dstr:snap-point (pt / os snap-pt)
  (setq os (getvar "OSMODE"))
  (if (and (> os 0) (< os 16384))
    (progn
      (setq snap-pt (osnap pt "_end,_mid,_cen,_node,_quad,_int,_ins,_perp,_tan,_nea"))
      (if snap-pt snap-pt pt))
    pt))

;; Helper: Render live frame with dynamic block indicators, vectors & deformed ghosts
(defun dstr:render-live-blocks (detected-data box-list dx dy /
                                item mode ent obj corners cx cy d pairs singles
                                pair itemA itemB uA uB uX uY delta
                                resA resB movInA baseInA movInB baseInB
                                s-item s-prop s-val s-u s-cor sux suy
                                s-res s-movIn s-baseIn s-delta vPt1 vPt2 arrow-len)
  (foreach item detected-data
    (setq mode (nth 0 item))
    (cond
      ;; --- MODE MOVE: Fully enclosed block ---
      ((eq mode 'move)
       (setq corners (nth 3 item)
             cx      (nth 4 item)
             cy      (nth 5 item))
       ;; Draw magenta ghost box translated by (dx, dy)
       (dstr:draw-ghost-box corners dx dy 6)
       ;; Draw motion trail vector from center
       (if (> (+ (* dx dx) (* dy dy)) 1e-4)
         (dstr:draw-arrow (list cx cy 0.0) (list (+ cx dx) (+ cy dy) 0.0) 6))
      )

      ;; --- MODE DYNAMIC: Partially crossing dynamic block ---
      ((eq mode 'dynamic)
       (setq corners (nth 3 item)
             cx      (nth 4 item)
             cy      (nth 5 item)
             d       (nth 6 item)
             pairs   (nth 7 item)
             singles (nth 8 item))
       (setq arrow-len (max (* 0.35 d) (* (getvar "VIEWSIZE") 0.05)))

       ;; 1. Opposing pairs (Green indicators & live expansion ghost)
       (foreach pair pairs
         (setq itemA   (car pair)
               itemB   (cadr pair)
               uA      (nth 2 itemA)
               uX      (car uA)
               uY      (cadr uA)
               delta   (+ (* dx uX) (* dy uY))
               resA    (dstr:check-overlap (nth 3 itemA) box-list uX uY)
               resB    (dstr:check-overlap (nth 3 itemB) box-list (car (nth 2 itemB)) (cadr (nth 2 itemB)))
               movInA  (car resA)
               baseInA (cadr resA)
               movInB  (car resB)
               baseInB (cadr resB))

         ;; Live dynamic arrows along pair axis
         (setq vPt1 (list (+ cx (* (+ arrow-len (max 0.0 delta)) uX))
                          (+ cy (* (+ arrow-len (max 0.0 delta)) uY))
                          0.0)
               vPt2 (list (- cx (* arrow-len uX))
                          (- cy (* arrow-len uY))
                          0.0))
         (dstr:draw-arrow (list cx cy 0.0) vPt1 3)
         (dstr:draw-arrow (list cx cy 0.0) vPt2 3)

         ;; Ghost bounding box showing active stretch deformation
         (cond
           ((and movInA (not baseInA))
            (dstr:draw-deformed-ghost corners uX uY delta 3))
           ((and movInB (not baseInB))
            (dstr:draw-deformed-ghost corners (- uX) (- uY) (- delta) 3))
         )
       )

       ;; 2. Single independent parameters (Yellow indicators & stretch ghost)
       (foreach s-item singles
         (setq s-prop  (nth 0 s-item)
               s-val   (nth 1 s-item)
               s-u     (nth 2 s-item)
               s-cor   (nth 3 s-item)
               sux     (car s-u)
               suy     (cadr s-u)
               s-delta (+ (* dx sux) (* dy suy))
               s-res   (dstr:check-overlap s-cor box-list sux suy)
               s-movIn (car s-res)
               s-baseIn(cadr s-res))

         ;; Live single arrow along axis
         (setq vPt1 (list (+ cx (* (+ arrow-len (max 0.0 s-delta)) sux))
                          (+ cy (* (+ arrow-len (max 0.0 s-delta)) suy))
                          0.0))
         (dstr:draw-arrow (list cx cy 0.0) vPt1 2)

         ;; Ghost bounding box showing deformation or base shift
         (cond
           ((and s-movIn (not s-baseIn))
            (dstr:draw-deformed-ghost corners sux suy s-delta 2))
           ((and s-baseIn (not s-movIn))
            (dstr:draw-ghost-box corners dx dy 2))
         )
       )
      )
    )
  )
)

;; Helper: Interactive live displacement loop (grread) with real-time arrow & ghost tracking
(defun dstr:pick-displacement (base-pt box-list detected-data /
                               loop gr code val cur-pt raw-pt dx dy dest-pt
                               num-str typed-dist ang dist-val status-text)
  (setq loop      t
        dest-pt   nil
        num-str   ""
        cur-pt    base-pt)

  (princ "\nSpecify second point [Live Viewfinder Preview Active] (or type distance): ")

  (while loop
    (setq gr (grread t 15 0))
    (setq code (car gr)
          val  (cadr gr))

    (cond
      ;; --- MOUSE MOVE: code = 5 ---
      ((= code 5)
       (setq raw-pt val)
       ;; 1. Check Ortho
       (if (= (getvar "ORTHOMODE") 1)
         (setq raw-pt (dstr:apply-ortho base-pt raw-pt)))
       ;; 2. Check Osnap
       (setq cur-pt (dstr:snap-point raw-pt))
       (setq dx (- (car cur-pt) (car base-pt))
             dy (- (cadr cur-pt) (cadr base-pt)))

       ;; 3. Redraw screen vectors at 60 FPS
       (redraw)
       (dstr:draw-all-boxes box-list)
       (if (> (distance base-pt cur-pt) 1e-4)
         (dstr:draw-arrow (list (car base-pt) (cadr base-pt) 0.0)
                          (list (car cur-pt) (cadr cur-pt) 0.0)
                          2)) ; Yellow displacement arrow
       (dstr:render-live-blocks detected-data box-list dx dy)

       ;; 4. Real-time HUD readout in AutoCAD status line
       (setq dist-val (distance base-pt cur-pt)
             status-text (strcat "DS Vector: " (rtos dist-val 2 2)
                                 " < " (angtos (angle base-pt cur-pt) 0 1)
                                 (if (> (strlen num-str) 0)
                                   (strcat " | Input: " num-str)
                                   " | Live Viewfinder Active")))
       (vl-catch-all-apply 'grtext (list -1 status-text))
      )

      ;; --- MOUSE LEFT CLICK: code = 3 ---
      ((= code 3)
       (setq raw-pt val)
       (if (= (getvar "ORTHOMODE") 1)
         (setq raw-pt (dstr:apply-ortho base-pt raw-pt)))
       (setq cur-pt  (dstr:snap-point raw-pt)
             dest-pt cur-pt
             loop    nil)
      )

      ;; --- KEYBOARD INPUT: code = 2 ---
      ((= code 2)
       (cond
         ;; Enter (13) or Space (32)
         ((or (= val 13) (= val 32))
          (if (> (strlen num-str) 0)
            (progn
              (setq typed-dist (distof num-str))
              (if (and typed-dist (> typed-dist 1e-6))
                (setq ang     (angle base-pt cur-pt)
                      dest-pt (polar base-pt ang typed-dist))
                (setq dest-pt cur-pt)))
            (setq dest-pt cur-pt))
          (setq loop nil))

         ;; Escape (27)
         ((= val 27)
          (setq dest-pt nil
                loop    nil))

         ;; Backspace (8)
         ((= val 8)
          (if (> (strlen num-str) 0)
            (progn
              (setq num-str (substr num-str 1 (1- (strlen num-str))))
              (princ "\b \b"))))

         ;; Numeric input: Digits 0-9 (48-57), Minus (45), Dot (46)
         ((or (and (>= val 48) (<= val 57)) (= val 45) (= val 46))
          (setq num-str (strcat num-str (chr val)))
          (princ (chr val)))
       )
      )

      ;; --- RIGHT CLICK: code = 11 or 25 ---
      ((or (= code 11) (= code 25))
       (setq dest-pt cur-pt
             loop    nil))
    )
  )

  ;; Clear status line and screen vectors
  (vl-catch-all-apply 'grtext (list -1 ""))
  (redraw)
  dest-pt
)

;;; ==========================================================================
;;; 3. HELPER FUNCTIONS: GEOMETRIC PROBING & PARAMETER CONSTRAINTS
;;; ==========================================================================

;; Helper: Safe parameter test delta (supports discrete lists & increments)
(defun dstr:get-safe-test-val (prop current-val / allowed sorted idx)
  (setq allowed (vlax-get prop 'allowedvalues))
  (cond
    ((and allowed (> (length allowed) 1))
     (setq sorted (vl-sort (vlax-safearray->list allowed) '<)
           idx    (vl-position current-val sorted))
     (cond
       ((and idx (< (1+ idx) (length sorted))) (nth (1+ idx) sorted))
       ((and idx (> idx 0))                    (nth (1- idx) sorted))
       (t (+ current-val 1.0))))
    (t (+ current-val 1.0))))

;; Helper: Check if an entity is fully enclosed within UCS 2D boundary boxes
(defun dstr:is-fully-inside (obj box-list / bmin bmax min-pt max-pt pts ucs-pts xs ys
                                            min-x max-x min-y max-y is-in)
  (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list obj 'bmin 'bmax))))
    (progn
      (setq min-pt (vlax-safearray->list bmin)
            max-pt (vlax-safearray->list bmax))
      (setq pts (list (list (car min-pt) (cadr min-pt) 0.0)
                      (list (car max-pt) (cadr min-pt) 0.0)
                      (list (car max-pt) (cadr max-pt) 0.0)
                      (list (car min-pt) (cadr max-pt) 0.0)))
      (setq ucs-pts (mapcar '(lambda (pt) (trans pt 0 1)) pts)
            xs      (mapcar 'car  ucs-pts)
            ys      (mapcar 'cadr ucs-pts)
            min-x   (apply 'min xs)
            max-x   (apply 'max xs)
            min-y   (apply 'min ys)
            max-y   (apply 'max ys))
      (setq is-in nil)
      (foreach box box-list
        (if (and (>= min-x (- (car (car box)) 1e-4))
                 (<= max-x (+ (car (cadr box)) 1e-4))
                 (>= min-y (- (cadr (car box)) 1e-4))
                 (<= max-y (+ (cadr (cadr box)) 1e-4)))
          (setq is-in t)))
      is-in)
    nil))

;; Helper: Probe parameter extension axis vector (ux, uy, ucs-corners)
(defun dstr:probe-vector (obj prop orig-val / pmin pmax nmin nmax p1 p2 corners
                                              test-val delta vx vy vlen rot n1 n2)
  (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list obj 'pmin 'pmax)))
    nil
    (progn
      (setq p1 (trans (vlax-safearray->list pmin) 0 1)
            p2 (trans (vlax-safearray->list pmax) 0 1))
      (setq corners (list (list (car p1) (cadr p1))
                          (list (car p2) (cadr p1))
                          (list (car p2) (cadr p2))
                          (list (car p1) (cadr p2))))

      (setq test-val (dstr:get-safe-test-val prop orig-val)
            delta    (- test-val orig-val))

      (if (equal delta 0.0 1e-4)
        (setq test-val (- orig-val 1.0)
              delta    (- test-val orig-val)))

      (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vlax-put (list prop 'value test-val))))
        (progn
          (vla-update obj)
          (vla-getboundingbox obj 'nmin 'nmax)
          (setq n1 (trans (vlax-safearray->list nmin) 0 1)
                n2 (trans (vlax-safearray->list nmax) 0 1))

          ;; Immediately revert parameter back
          (vl-catch-all-apply 'vlax-put (list prop 'value orig-val))
          (vla-update obj)

          (setq vx (- (+ (car n2)  (car n1))  (+ (car p2)  (car p1)))
                vy (- (+ (cadr n2) (cadr n1)) (+ (cadr p2) (cadr p1))))

          (if (< delta 0.0)
            (setq vx (- vx) vy (- vy)))

          (setq vlen (sqrt (+ (* vx vx) (* vy vy))))
          (if (> vlen 1e-4)
            (list (/ vx vlen) (/ vy vlen) corners)
            (progn
              (setq rot (vla-get-rotation obj))
              (list (cos rot) (sin rot) corners))))
        nil))))

;; Helper: Snap proposed value to dynamic property constraints
(defun dstr:snap-value (prop target-val / allowed sorted min-val max-val)
  (cond
    ((and (setq allowed (vlax-get prop 'allowedvalues)) (> (length allowed) 0))
     (setq sorted (vl-sort (vlax-safearray->list allowed) '<))
     (car (vl-sort sorted '(lambda (a b) (< (abs (- a target-val)) (abs (- b target-val)))))))
    ((and (vlax-property-available-p prop 'minimumvalue)
          (vlax-property-available-p prop 'maximumvalue))
     (setq min-val (vlax-get prop 'minimumvalue)
           max-val (vlax-get prop 'maximumvalue))
     (cond
       ((< target-val min-val) min-val)
       ((> target-val max-val) max-val)
       (t target-val)))
    (t target-val)))

;; Helper: Check 1D projection overlap of block and crossing window
(defun dstr:check-window-overlap (corners win-corners ux uy /
                                  s-list t-list ws-list wt-list
                                  s-min s-max t-min t-max w-min w-max wt-min wt-max)
  (setq s-list  (mapcar '(lambda (pt) (+ (* (car pt) ux) (* (cadr pt) uy))) corners)
        t-list  (mapcar '(lambda (pt) (- (* (cadr pt) ux) (* (car pt) uy))) corners)
        s-min   (apply 'min s-list)
        s-max   (apply 'max s-list)
        t-min   (apply 'min t-list)
        t-max   (apply 'max t-list))
  (setq ws-list (mapcar '(lambda (pt) (+ (* (car pt) ux) (* (cadr pt) uy))) win-corners)
        wt-list (mapcar '(lambda (pt) (- (* (cadr pt) ux) (* (car pt) uy))) win-corners)
        w-min   (apply 'min ws-list)
        w-max   (apply 'max ws-list)
        wt-min  (apply 'min wt-list)
        wt-max  (apply 'max wt-list))

  (if (and (<= wt-min (+ t-max 1e-4))
           (>= wt-max (- t-min 1e-4)))
    (list (and (>= s-max (- w-min 1e-4)) (<= s-max (+ w-max 1e-4)))   ; movIn
          (and (>= s-min (- w-min 1e-4)) (<= s-min (+ w-max 1e-4))))  ; baseIn
    (list nil nil)))

;; Helper: Check overlap against a collection of boundary boxes
(defun dstr:check-overlap (corners box-list ux uy / movIn baseIn res w-corners bmin bmax)
  (setq movIn nil baseIn nil)
  (foreach box box-list
    (setq bmin      (car box)
          bmax      (cadr box)
          w-corners (list (list (car bmin) (cadr bmin))
                          (list (car bmax) (cadr bmin))
                          (list (car bmax) (cadr bmax))
                          (list (car bmin) (cadr bmax))))
    (setq res (dstr:check-window-overlap corners w-corners ux uy))
    (if (car res)  (setq movIn  t))
    (if (cadr res) (setq baseIn t)))
  (list movIn baseIn))

;; Helper: Pair parameters that point in opposite directions (u1 . u2 <= -0.95)
(defun dstr:classify-properties (probed-list / pairs singles item1 rest u1 matched)
  (setq pairs nil singles nil)
  (while probed-list
    (setq item1   (car probed-list)
          rest    (cdr probed-list)
          u1      (nth 2 item1)
          matched nil)
    (foreach item2 rest
      (if (and (not matched)
               (<= (+ (* (car u1) (car (nth 2 item2)))
                      (* (cadr u1) (cadr (nth 2 item2))))
                   -0.95))
        (progn
          (setq pairs       (cons (list item1 item2) pairs)
                probed-list (vl-remove item2 rest)
                matched     t))))
    (if (not matched)
      (setq singles     (cons item1 singles)
            probed-list rest)))
  (list pairs singles))

;; Helper: Apply equalized expansion & update insertion point
(defun dstr:apply-opposite-stretch (obj prop-cap val-cap u-cap prop-opp val-opp dx dy /
                                    ux uy delta total-new half-val new-val shift
                                    ins-ucs new-ins-ucs new-ins-wcs)
  (setq ux    (car u-cap)
        uy    (cadr u-cap)
        delta (+ (* dx ux) (* dy uy)))
  (if (> (abs delta) 1e-4)
    (progn
      ;; Total length = original combined length + delta moved
      (setq total-new (+ val-cap val-opp delta)
            half-val  (/ total-new 2.0)
            ;; Snap equalized half-length to dynamic property step/constraints
            new-val   (dstr:snap-value prop-cap half-val))
      (if (> new-val 1e-4)
        (progn
          ;; Shift needed along u-cap to keep unselected end stationary
          (setq shift       (- new-val val-opp)
                ins-ucs     (trans (vlax-get obj 'InsertionPoint) 0 1)
                new-ins-ucs (list (+ (car ins-ucs) (* shift ux))
                                  (+ (cadr ins-ucs) (* shift uy))
                                  (caddr ins-ucs))
                new-ins-wcs (trans new-ins-ucs 1 0))
          (vla-put-insertionpoint obj (vlax-3d-point new-ins-wcs))

          ;; Assign identical values to both parameters
          (vl-catch-all-apply 'vlax-put (list prop-cap 'value new-val))
          (vl-catch-all-apply 'vlax-put (list prop-opp 'value new-val))
          (vla-update obj)
          t)
        nil))
    nil))

;; Helper: Pre-analyze detected blocks for real-time live preview
(defun dstr:analyze-blocks (ss-all-blocks box-list / blk-data ent obj pmin pmax
                                                     p1 p2 corners cx cy d is-inside
                                                     probed-props props val probe-res
                                                     classified pairs singles)
  (setq blk-data nil)
  (if ss-all-blocks
    (foreach ent ss-all-blocks
      (setq obj (vlax-ename->vla-object ent))
      (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list obj 'pmin 'pmax))))
        (progn
          (setq p1 (trans (vlax-safearray->list pmin) 0 1)
                p2 (trans (vlax-safearray->list pmax) 0 1)
                cx (* 0.5 (+ (car p1) (car p2)))
                cy (* 0.5 (+ (cadr p1) (cadr p2)))
                d  (distance p1 p2))
          (setq corners (list (list (car p1) (cadr p1))
                              (list (car p2) (cadr p1))
                              (list (car p2) (cadr p2))
                              (list (car p1) (cadr p2))))

          (setq is-inside (dstr:is-fully-inside obj box-list))

          (if is-inside
            ;; Fully enclosed block -> mode 'move
            (setq blk-data (cons (list 'move ent obj corners cx cy d) blk-data))
            ;; Partially crossing block
            (if (= (vla-get-isdynamicblock obj) :vlax-true)
              (progn
                (setq probed-props nil
                      props        (vlax-invoke obj 'getdynamicblockproperties))
                (foreach prop props
                  (if (and (= (vla-get-readonly prop) :vlax-false)
                           (not (wcmatch (strcase (vla-get-propertyname prop))
                                         "*VISIBILITY*,*LOOKUP*,*FLIP*,*ANGLE*")))
                    (progn
                      (setq val (vlax-get prop 'value))
                      (if (and (numberp val) (> val 1e-4))
                        (progn
                          (setq probe-res (dstr:probe-vector obj prop val))
                          (if probe-res
                            (setq probed-props
                                  (cons (list prop val (list (nth 0 probe-res) (nth 1 probe-res)) (nth 2 probe-res))
                                        probed-props))))))))
                (setq classified (dstr:classify-properties probed-props)
                      pairs      (car classified)
                      singles    (cadr classified))
                (setq blk-data (cons (list 'dynamic ent obj corners cx cy d pairs singles) blk-data))
              )
            )
          )
        )
      )
    )
  )
  (reverse blk-data)
)

;;; ==========================================================================
;;; 4. MAIN COMMAND: DYNSTRETCH / DYNAMICSTRETCH
;;; ==========================================================================
(defun c:DYNSTRETCH ( / *error* old-echo old-osmode doc pt1 pt2 p-next p-next2
                        ucs-min ucs-max box-list continue base-pt dest-pt
                        dx dy ss-all-blocks ss-box ss-other ss-blocks
                        bmin bmax k ent obj count-blocks highlighted-ents
                        detected-data stretched-list block-moved
                        item mode corners pairs singles pair itemA itemB
                        resA resB movInA baseInA movInB baseInB
                        s-item s-prop s-val s-u s-cor sux suy
                        s-res s-movIn s-baseIn s-delta s-newVal
                        new-ins-ucs new-ins-wcs)

  ;; Error Handler & Stack Reset
  (defun *error* (msg)
    (vl-catch-all-apply 'grtext (list -1 ""))
    (if old-echo   (setvar "CMDECHO" old-echo))
    (if old-osmode (setvar "OSMODE"  old-osmode))
    ;; Unhighlight all glowing candidate entities
    (if highlighted-ents
      (foreach ent highlighted-ents
        (if (entget ent) (redraw ent 4))))
    (redraw) ; Clear all temporary HUD brackets, arrows, and boxes
    (if (boundp 'CadSetup:UndoReset)
      (CadSetup:UndoReset)
      (if doc (vla-endundomark doc)))
    (if (and msg (not (wcmatch (strcase msg t) "*break*,*cancel*,*exit*,*quit*")))
      (princ (strcat "\n[DYNSTRETCH Error]: " msg)))
    (princ))

  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (if (boundp 'CadSetup:UndoStart)
    (CadSetup:UndoStart)
    (if doc (vla-startundomark doc)))

  (setq old-echo   (getvar "CMDECHO"))
  (setq old-osmode (getvar "OSMODE"))
  (setvar "CMDECHO" 0)

  (setq highlighted-ents nil)

  ;;-----------------------------------------------------------------------
  ;; 1. User Prompts: Boundary Box(es) with Multi-Color HUD Viewfinders
  ;;-----------------------------------------------------------------------
  (setq box-list nil)
  (setq pt1 (getpoint "\nSpecify first corner of crossing window (or Enter to finish): "))
  (if pt1
    (progn
      (setq pt2 (getcorner pt1 "\nSpecify opposite corner: "))
      (if pt2
        (progn
          (setq ucs-min (list (min (car pt1) (car pt2)) (min (cadr pt1) (cadr pt2)) 0.0)
                ucs-max (list (max (car pt1) (car pt2)) (max (cadr pt1) (cadr pt2)) 0.0))
          (setq box-list (list (list ucs-min ucs-max)))
          (dstr:draw-all-boxes box-list)

          ;; Prompt for additional crossing windows (multi-boundary mode)
          (setq continue t)
          (while continue
            (setq p-next (getpoint (strcat "\nSpecify first corner of next window ["
                                           (itoa (1+ (length box-list)))
                                           "] (or press Enter/Space to proceed): ")))
            (if p-next
              (progn
                (setq p-next2 (getcorner p-next "\nSpecify opposite corner: "))
                (if p-next2
                  (progn
                    (setq ucs-min (list (min (car p-next) (car p-next2)) (min (cadr p-next) (cadr p-next2)) 0.0)
                          ucs-max (list (max (car p-next) (car p-next2)) (max (cadr p-next) (cadr p-next2)) 0.0))
                    (setq box-list (cons (list ucs-min ucs-max) box-list))
                    (dstr:draw-all-boxes (reverse box-list)))
                  (princ "\nWindow pick cancelled.")))
              (setq continue nil)))
          (setq box-list (reverse box-list))))))

  (if (null box-list)
    (progn
      (princ "\n[DYNSTRETCH] No crossing windows defined. Exiting.")
      (if (boundp 'CadSetup:UndoEnd)
        (CadSetup:UndoEnd)
        (if doc (vla-endundomark doc)))
      (setvar "CMDECHO" old-echo)
      (setvar "OSMODE"  old-osmode)
      (redraw))
    (progn
      ;; Collect all unique blocks across all crossing windows
      (setq ss-all-blocks nil)
      (foreach box box-list
        (setq ss-box (ssget "_C" (car box) (cadr box) '((0 . "INSERT"))))
        (if ss-box
          (repeat (setq k (sslength ss-box))
            (setq ent (ssname ss-box (setq k (1- k))))
            (if (not (member ent ss-all-blocks))
              (setq ss-all-blocks (cons ent ss-all-blocks))))))

      ;; Visual highlight on candidate entities (AutoCAD Shimmer Highlight)
      (setq count-blocks (length ss-all-blocks))
      (if (> count-blocks 0)
        (foreach ent ss-all-blocks
          (redraw ent 3) ; Highlight entity with dotted outline
          (setq highlighted-ents (cons ent highlighted-ents))))

      ;; Pre-analyze blocks for high-performance 60 FPS live preview
      (setq detected-data (dstr:analyze-blocks ss-all-blocks box-list))

      ;; Render static initial HUD overlays while waiting for base point
      (redraw)
      (dstr:draw-all-boxes box-list)
      (dstr:render-live-blocks detected-data box-list 0.0 0.0)

      (princ (strcat "\n[DYNSTRETCH Visualizer]: "
                     (itoa (length box-list)) " region(s) active | "
                     (itoa count-blocks) " block(s) detected and analyzed."))

      ;;-----------------------------------------------------------------------
      ;; 2. User Prompts: Base Point and Live Interactive Displacement
      ;;-----------------------------------------------------------------------
      (setq base-pt (getpoint "\nSpecify base point: "))
      (if (not base-pt)
        (progn
          (princ "\n[DYNSTRETCH] Base point cancelled.")
          (if highlighted-ents
            (foreach ent highlighted-ents (if (entget ent) (redraw ent 4))))
          (if (boundp 'CadSetup:UndoEnd)
            (CadSetup:UndoEnd)
            (if doc (vla-endundomark doc)))
          (setvar "CMDECHO" old-echo)
          (setvar "OSMODE"  old-osmode)
          (redraw))
        (progn
          ;; LIVE 60 FPS DISPLACEMENT PICK WITH DIRECTIONAL ARROWS & GHOSTS
          (setq dest-pt (dstr:pick-displacement base-pt box-list detected-data))

          (cond
            ((not dest-pt)
             (princ "\n[DYNSTRETCH] Second point cancelled.")
             (if highlighted-ents
               (foreach ent highlighted-ents (if (entget ent) (redraw ent 4))))
             (if (boundp 'CadSetup:UndoEnd)
               (CadSetup:UndoEnd)
               (if doc (vla-endundomark doc)))
             (setvar "CMDECHO" old-echo)
             (setvar "OSMODE"  old-osmode)
             (redraw))
            ((< (distance base-pt dest-pt) 1e-6)
             (princ "\n[DYNSTRETCH] Distance is zero. No stretch performed.")
             (if highlighted-ents
               (foreach ent highlighted-ents (if (entget ent) (redraw ent 4))))
             (if (boundp 'CadSetup:UndoEnd)
               (CadSetup:UndoEnd)
               (if doc (vla-endundomark doc)))
             (setvar "CMDECHO" old-echo)
             (setvar "OSMODE"  old-osmode)
             (redraw))
            (t
             (setq dx (- (car dest-pt)  (car base-pt))
                   dy (- (cadr dest-pt) (cadr base-pt)))

             ;;-----------------------------------------------------------------------
             ;; 3. Stretch regular (non-block) geometry natively
             ;;-----------------------------------------------------------------------
             (setvar "OSMODE" 0)
             (foreach box box-list
               (setq bmin (car box)
                     bmax (cadr box))
               (setq ss-other  (ssget "_C" bmin bmax '((0 . "~INSERT")))
                     ss-blocks (ssget "_C" bmin bmax '((0 . "INSERT"))))
               (if ss-other
                 (progn
                   (command "_.stretch" "_C" "_non" bmin "_non" bmax)
                   (if ss-blocks
                     (progn
                       (command "_R")
                       (repeat (setq k (sslength ss-blocks))
                         (command (ssname ss-blocks (setq k (1- k)))))
                       (command ""))
                     (command ""))
                   (command "_non" base-pt "_non" dest-pt))))

             ;;-----------------------------------------------------------------------
             ;; 4. Dynamic block parameter adaptation (Equalized Opposing Pair Engine)
             ;;-----------------------------------------------------------------------
             (setq stretched-list nil)

             (foreach item detected-data
               (setq mode (nth 0 item))
               (cond
                 ;; Case Dynamic
                 ((eq mode 'dynamic)
                  (setq ent     (nth 1 item)
                        obj     (nth 2 item)
                        pairs   (nth 7 item)
                        singles (nth 8 item)
                        block-moved nil)

                  ;; Case A: Opposite Pairs
                  (foreach pair pairs
                    (setq itemA   (car pair)
                          itemB   (cadr pair)
                          resA    (dstr:check-overlap (nth 3 itemA) box-list (car (nth 2 itemA)) (cadr (nth 2 itemA)))
                          resB    (dstr:check-overlap (nth 3 itemB) box-list (car (nth 2 itemB)) (cadr (nth 2 itemB)))
                          movInA  (car resA)
                          baseInA (cadr resA)
                          movInB  (car resB)
                          baseInB (cadr resB))

                    (cond
                      ((and movInA (not baseInA))
                       (if (dstr:apply-opposite-stretch obj (nth 0 itemA) (nth 1 itemA) (nth 2 itemA)
                                                            (nth 0 itemB) (nth 1 itemB) dx dy)
                         (setq stretched-list (cons ent stretched-list)
                               block-moved    t)))

                      ((and movInB (not baseInB))
                       (if (dstr:apply-opposite-stretch obj (nth 0 itemB) (nth 1 itemB) (nth 2 itemB)
                                                            (nth 0 itemA) (nth 1 itemA) dx dy)
                         (setq stretched-list (cons ent stretched-list)
                               block-moved    t)))))

                  ;; Case B: Single Independent Parameters
                  (foreach s-item singles
                    (setq s-prop   (nth 0 s-item)
                          s-val    (nth 1 s-item)
                          s-u      (nth 2 s-item)
                          s-cor    (nth 3 s-item)
                          sux      (car s-u)
                          suy      (cadr s-u)
                          s-res    (dstr:check-overlap s-cor box-list sux suy)
                          s-movIn  (car s-res)
                          s-baseIn (cadr s-res))

                    (if (or s-movIn s-baseIn)
                      (progn
                        (setq s-delta (+ (* dx sux) (* dy suy)))
                        (cond
                          ((and s-movIn (not s-baseIn))
                           (setq s-newVal (dstr:snap-value s-prop (+ s-val s-delta)))
                           (if (and (> s-newVal 1e-4)
                                    (not (vl-catch-all-error-p
                                           (vl-catch-all-apply 'vlax-put (list s-prop 'value s-newVal)))))
                             (progn
                               (vla-update obj)
                               (if (not (member ent stretched-list))
                                 (setq stretched-list (cons ent stretched-list))))))

                          ((and s-baseIn (not s-movIn))
                           (if (> (abs s-delta) 1e-4)
                             (progn
                               (setq s-newVal (dstr:snap-value s-prop (- s-val s-delta)))
                               (if (> s-newVal 1e-4)
                                 (progn
                                   (if (not block-moved)
                                     (progn
                                       (setq new-ins-ucs (list (+ (car (trans (vlax-get obj 'InsertionPoint) 0 1)) dx)
                                                               (+ (cadr (trans (vlax-get obj 'InsertionPoint) 0 1)) dy)
                                                               (caddr (trans (vlax-get obj 'InsertionPoint) 0 1)))
                                             new-ins-wcs (trans new-ins-ucs 1 0))
                                       (vla-put-insertionpoint obj (vlax-3d-point new-ins-wcs))
                                       (setq block-moved t)))
                                   (if (not (vl-catch-all-error-p
                                              (vl-catch-all-apply 'vlax-put (list s-prop 'value s-newVal))))
                                     (progn
                                       (vla-update obj)
                                       (if (not (member ent stretched-list))
                                         (setq stretched-list (cons ent stretched-list)))))))))))))))

                 ;; Case Move: 100% enclosed block
                 ((eq mode 'move)
                  (setq ent (nth 1 item)
                        obj (nth 2 item))
                  (if (not (member ent stretched-list))
                    (vla-move obj
                              (vlax-3d-point (trans base-pt 1 0))
                              (vlax-3d-point (trans dest-pt 1 0)))))
               )
             )

             ;;-----------------------------------------------------------------------
             ;; 5. Clean Up & Finalize
             ;;-----------------------------------------------------------------------
             (setvar "OSMODE"  old-osmode)
             (setvar "CMDECHO" old-echo)

             ;; De-highlight all candidate blocks
             (if highlighted-ents
               (foreach ent highlighted-ents (if (entget ent) (redraw ent 4))))
             (redraw) ; Clear all temporary HUD visual vectors

             (if (boundp 'CadSetup:UndoEnd)
               (CadSetup:UndoEnd)
               (if doc (vla-endundomark doc)))

             (princ (strcat "\n[DYNSTRETCH Complete]: "
                            (itoa (length stretched-list)) " dynamic block(s) adapted, "
                            (itoa (length box-list)) " boundary region(s) processed."))
            ))))))
  (princ))

;;; ==========================================================================
;;; 5. COMMAND ALIASES
;;; ==========================================================================
(defun c:DYNAMICSTRETCH () (c:DYNSTRETCH))
(defun c:DSTRETCH       () (c:DYNSTRETCH))
(defun c:DS             () (c:DYNSTRETCH))

(if *CadSetup-Debug*
  (princ "\n[08_Dynamic-Stretch.lsp] Multi-boundary Live Viewfinder & Opposing Pair Engine loaded."))
(princ "\nDYNAMICSTRETCH loaded (Live Viewfinder & Opposing Pair Engine). Aliases: DYNSTRETCH, DSTRETCH, DS.")
(princ)
