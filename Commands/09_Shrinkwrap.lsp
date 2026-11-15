;;; ==========================================================================
;;; 09_Shrinkwrap.lsp - Concave Hull & Corridor-Snap 2D Shrinkwrap Engine
;;; Layer: Commands (Priority 09)
;;; Author   : Haseeb
;;; Commands : SHRINKWRAPVIS, WRAP
;;; ==========================================================================
;;; ARCHITECTURAL NOTE:
;;; Generates an outer 2D closed LWPOLYLINE boundary around arbitrary selected
;;; AutoCAD geometry in the active UCS plane:
;;;   1. Adaptive geometry sampling (curves, vertices, blocks, solids, points)
;;;      with infinite entity filtering (XLINE/RAY) and strict weak-order
;;;      spatial deduplication.
;;;   2. Local UCS origin-shifted coordinate space to prevent floating-point
;;;      cancellation and 32-bit integer overflow on georeferenced drawings.
;;;   3. Scale-invariant radial concave hull / convex hull perimeter tracking
;;;      with optional real-time streaming animation.
;;;   4. Iterative corridor-projection & spatial-grid inward shrinkwrap pass
;;;      (for Convex [1] mode) with monotonic bridge merging and bowtie
;;;      prevention.
;;;   5. Final nearest-point vertex snap, active-layer lock preservation,
;;;      optional arc fitting (PEDIT _F), re-entrant undo transactions
;;;      (CadSetup:UndoStart / CadSetup:UndoEnd / CadSetup:UndoReset), and
;;;      complete error/Esc cleanup.
;;; ==========================================================================

(vl-load-com)

;;; --------------------------------------------------------------------------
;;; 1. VALIDATION, SANITIZATION & MATH HELPERS
;;; --------------------------------------------------------------------------

;; sw:valid-pt-p - Returns T if pt is a 2D or 3D list of finite numbers
(defun sw:valid-pt-p (pt)
  (and (vl-consp pt)
       (>= (length pt) 2)
       (numberp (car pt))
       (numberp (cadr pt))
       (= (car pt) (car pt))
       (= (cadr pt) (cadr pt))
       (or (< (length pt) 3)
           (and (numberp (caddr pt))
                (= (caddr pt) (caddr pt)))))
)

;; sw:to-2d - Normalizes a valid 2D/3D point into a 2-element float list
(defun sw:to-2d (pt)
  (if (sw:valid-pt-p pt)
    (list (float (car pt)) (float (cadr pt)))
    nil
  )
)

;; sw:flatten-2d - Safely flattens ((x1 y1) (x2 y2) ...) into (x1 y1 x2 y2 ...)
;; Avoids (apply 'append ...) argument-count limits on large boundaries.
(defun sw:flatten-2d (pts / res p)
  (setq res nil)
  (foreach p (reverse pts)
    (if (sw:valid-pt-p p)
      (setq res (cons (float (car p)) (cons (float (cadr p)) res)))
    )
  )
  res
)

;; sw:floor - Safe integer floor guarded against 32-bit overflow
(defun sw:floor (n / clamped fn)
  (setq clamped (max -2000000000.0 (min 2000000000.0 (float n)))
        fn      (fix clamped))
  (if (and (< clamped 0.0) (/= clamped (float fn)))
    (1- fn)
    fn
  )
)

;; sw:coord-to-cell - Maps scalar coordinate val to integer grid cell index
(defun sw:coord-to-cell (val sz)
  (if (> sz 1e-12)
    (sw:floor (/ val sz))
    0
  )
)

;; sw:ccw - Twice the signed 2D area of triangle (p1, p2, p3)
(defun sw:ccw (p1 p2 p3)
  (- (* (- (car p2) (car p1)) (- (cadr p3) (cadr p1)))
     (* (- (cadr p2) (cadr p1)) (- (car p3) (car p1))))
)

;; sw:seg-intersect-p - Scale-invariant proper segment intersection predicate
;; Returns T iff open segments (p1,p2) and (p3,p4) cross strictly in their interiors.
(defun sw:seg-intersect-p (p1 p2 p3 p4 /
                           x1 y1 x2 y2 x3 y3 x4 y4
                           dx12 dy12 dx34 dy34 l12 l34 epTol
                           h1 h2 h3 h4 maxC tol)
  (setq x1 (car p1) y1 (cadr p1)
        x2 (car p2) y2 (cadr p2)
        x3 (car p3) y3 (cadr p3)
        x4 (car p4) y4 (cadr p4))
  (if (or (< (max x1 x2) (min x3 x4))
          (> (min x1 x2) (max x3 x4))
          (< (max y1 y2) (min y3 y4))
          (> (min y1 y2) (max y3 y4)))
    nil
    (progn
      (setq dx12 (- x2 x1) dy12 (- y2 y1)
            dx34 (- x4 x3) dy34 (- y4 y3)
            l12  (sqrt (+ (* dx12 dx12) (* dy12 dy12)))
            l34  (sqrt (+ (* dx34 dx34) (* dy34 dy34))))
      (if (or (< l12 1e-12) (< l34 1e-12))
        nil
        (progn
          (setq epTol (* 1e-9 (min l12 l34)))
          (if (or (equal p1 p3 epTol)
                  (equal p1 p4 epTol)
                  (equal p2 p3 epTol)
                  (equal p2 p4 epTol))
            nil
            (progn
              (setq h1   (/ (sw:ccw p3 p4 p1) l34)
                    h2   (/ (sw:ccw p3 p4 p2) l34)
                    h3   (/ (sw:ccw p1 p2 p3) l12)
                    h4   (/ (sw:ccw p1 p2 p4) l12)
                    maxC (max (abs x1) (abs y1) (abs x2) (abs y2)
                              (abs x3) (abs y3) (abs x4) (abs y4) 1e-12)
                    tol  (max (* 1e-12 maxC) (* 1e-8 (max l12 l34))))
              (and (or (and (> h1 tol) (< h2 (- tol)))
                       (and (< h1 (- tol)) (> h2 tol)))
                   (or (and (> h3 tol) (< h4 (- tol)))
                       (and (< h3 (- tol)) (> h4 tol))))
            )
          )
        )
      )
    )
  )
)

;; sw:edge-collides-p - Returns T if segment (pA, pB) intersects any edge in eList
(defun sw:edge-collides-p (pA pB eList / hit)
  (setq hit nil)
  (while (and eList (not hit))
    (if (sw:seg-intersect-p pA pB (caar eList) (cadar eList))
      (setq hit T)
    )
    (setq eList (cdr eList))
  )
  hit
)

;; sw:make-edges - Converts an open vertex list into consecutive (pA pB) segment pairs
(defun sw:make-edges (vList / res)
  (setq res nil)
  (while (cdr vList)
    (setq res (cons (list (car vList) (cadr vList)) res))
    (setq vList (cdr vList))
  )
  (reverse res)
)

;; sw:polygon-area2 - Twice the signed area of closed polygon (>0 = CCW, <0 = CW)
(defun sw:polygon-area2 (ptList / prev cur acc)
  (if (and ptList (>= (length ptList) 3))
    (progn
      (setq prev (last ptList)
            acc  0.0)
      (foreach cur ptList
        (setq acc  (+ acc (- (* (car prev) (cadr cur))
                             (* (car cur)  (cadr prev))))
              prev cur)
      )
      acc
    )
    0.0
  )
)

;; sw:add-vertex - Prepends pt onto ptList if farther than tol from current head
(defun sw:add-vertex (pt ptList tol)
  (if (and (sw:valid-pt-p pt)
           (or (null ptList) (> (distance pt (car ptList)) tol)))
    (cons pt ptList)
    ptList
  )
)

;; sw:rotate-list - Rotates list so element at startIdx becomes first element
(defun sw:rotate-list (lst startIdx / head tail i)
  (if (or (null lst) (<= startIdx 0) (>= startIdx (length lst)))
    lst
    (progn
      (setq head nil
            tail lst
            i    0)
      (while (< i startIdx)
        (setq head (cons (car tail) head)
              tail (cdr tail)
              i    (1+ i))
      )
      (append tail (reverse head))
    )
  )
)


;;; --------------------------------------------------------------------------
;;; 2. TWO-LEVEL SPATIAL HASH GRID & POINT DEDUPLICATION
;;; --------------------------------------------------------------------------

;; sw:grid-build - Indexes points into a 2-level alist ((gx . ((gy . (pts...)) ...)) ...)
;; Sorting uses strict integer lexicographic ordering (strict weak order).
(defun sw:grid-build (ptList cellSize / tagged sorted curGx curGy curRow curCol res item gx gy pt)
  (if (and ptList (> cellSize 1e-12))
    (progn
      (setq tagged
        (mapcar
          '(lambda (pt)
             (list (sw:coord-to-cell (car pt) cellSize)
                   (sw:coord-to-cell (cadr pt) cellSize)
                   pt))
          ptList
        )
      )
      (setq sorted
        (vl-sort tagged
          '(lambda (a b)
             (if (= (car a) (car b))
               (< (cadr a) (cadr b))
               (< (car a) (car b))))))
      (setq res nil curGx nil curGy nil curRow nil curCol nil)
      (foreach item sorted
        (setq gx (car item)
              gy (cadr item)
              pt (caddr item))
        (cond
          ((null curGx)
           (setq curGx  gx
                 curGy  gy
                 curRow (list pt)))
          ((and (= gx curGx) (= gy curGy))
           (setq curRow (cons pt curRow)))
          ((= gx curGx)
           (setq curCol (cons (cons curGy curRow) curCol)
                 curGy  gy
                 curRow (list pt)))
          (t
           (setq curCol (cons (cons curGy curRow) curCol)
                 res    (cons (cons curGx (reverse curCol)) res)
                 curGx  gx
                 curGy  gy
                 curCol nil
                 curRow (list pt)))
        )
      )
      (if curGx
        (progn
          (setq curCol (cons (cons curGy curRow) curCol)
                res    (cons (cons curGx (reverse curCol)) res))
        )
      )
      (reverse res)
    )
    nil
  )
)

;; sw:grid-lookup - Retrieves list of points inside grid cell (gx, gy)
(defun sw:grid-lookup (grid gx gy / col)
  (if (setq col (cdr (assoc gx grid)))
    (cdr (assoc gy col))
    nil
  )
)

;; sw:grid-has-near-p - Returns T if any indexed point lies within cellSize of pt
(defun sw:grid-has-near-p (pt grid cellSize cellSizeSq / cx cy gx gy ox oy col bucket found dx dy)
  (if (and pt grid (> cellSize 1e-12))
    (progn
      (setq cx (car pt)
            cy (cadr pt)
            gx (sw:coord-to-cell cx cellSize)
            gy (sw:coord-to-cell cy cellSize)
            found nil
            ox -1)
      (while (and (<= ox 1) (not found))
        (if (setq col (cdr (assoc (+ gx ox) grid)))
          (progn
            (setq oy -1)
            (while (and (<= oy 1) (not found))
              (setq bucket (cdr (assoc (+ gy oy) col)))
              (while (and bucket (not found))
                (setq dx (- (caar bucket) cx)
                      dy (- (cadar bucket) cy))
                (if (<= (+ (* dx dx) (* dy dy)) cellSizeSq)
                  (setq found T)
                )
                (setq bucket (cdr bucket))
              )
              (setq oy (1+ oy))
            )
          )
        )
        (setq ox (1+ ox))
      )
      found
    )
    nil
  )
)

;; sw:grid-nearest-pt - Returns point in pts nearest to pt using spatial grid fast-path
(defun sw:grid-nearest-pt (pt grid allPts cellSize / cx cy gx gy ox oy col bucket p d minDist bestPt)
  (setq cx (car pt)
        cy (cadr pt)
        gx (sw:coord-to-cell cx cellSize)
        gy (sw:coord-to-cell cy cellSize)
        minDist 1e30
        bestPt  nil
        ox -1)
  (while (<= ox 1)
    (if (setq col (cdr (assoc (+ gx ox) grid)))
      (progn
        (setq oy -1)
        (while (<= oy 1)
          (setq bucket (cdr (assoc (+ gy oy) col)))
          (while bucket
            (setq p      (car bucket)
                  bucket (cdr bucket)
                  d      (distance pt p))
            (if (< d minDist)
              (setq minDist d
                    bestPt  p)
            )
          )
          (setq oy (1+ oy))
        )
      )
    )
    (setq ox (1+ ox))
  )
  ;; 3x3 neighborhood covers all points within 1.0*cellSize; fall back only if farther
  (if (and bestPt (<= minDist cellSize))
    bestPt
    (progn
      (foreach p allPts
        (setq d (distance pt p))
        (if (< d minDist)
          (setq minDist d
                bestPt  p)
        )
      )
      bestPt
    )
  )
)

;; sw:dedupe-points - Spatial-grid O(N log N) deduplication prioritizing vertexPts
;; Ensures no two retained points lie within minSep of each other.
(defun sw:dedupe-points (vertPts samplePts minSep / minSepSq tagged sorted
                                                    acceptedList acceptedGrid
                                                    curGx curGy cellRaw curColAccepted
                                                    flush-cell check-near
                                                    item gx gy pt)
  (setq minSepSq (* minSep minSep)
        tagged   nil)
  ;; Priority 0 = explicit corners/vertices, Priority 1 = subdivided samples
  (foreach pt vertPts
    (if (sw:valid-pt-p pt)
      (setq tagged (cons (list (sw:coord-to-cell (car pt) minSep)
                               (sw:coord-to-cell (cadr pt) minSep)
                               0
                               pt)
                         tagged))
    )
  )
  (foreach pt samplePts
    (if (sw:valid-pt-p pt)
      (setq tagged (cons (list (sw:coord-to-cell (car pt) minSep)
                               (sw:coord-to-cell (cadr pt) minSep)
                               1
                               pt)
                         tagged))
    )
  )

  (if (null tagged)
    nil
    (progn
      (setq sorted
        (vl-sort tagged
          '(lambda (a b)
             (cond
               ((/= (car a) (car b))   (< (car a) (car b)))
               ((/= (cadr a) (cadr b)) (< (cadr a) (cadr b)))
               (t                      (< (caddr a) (caddr b)))
             )
           )
        )
      )

      (defun check-near (p candList / cx cy hit dx dy q)
        (setq cx (car p) cy (cadr p) hit nil)
        (while (and candList (not hit))
          (setq q  (car candList)
                dx (- (car q) cx)
                dy (- (cadr q) cy))
          (if (<= (+ (* dx dx) (* dy dy)) minSepSq)
            (setq hit T)
          )
          (setq candList (cdr candList))
        )
        hit
      )

      (defun flush-cell (gx gy rawItems / cellAccepted prevCol prevRow1 prevRow2 prevRow3 belowRow entry p)
        (setq prevCol      (cdr (assoc (1- gx) acceptedGrid))
              prevRow1     (if prevCol (cdr (assoc (1- gy) prevCol)) nil)
              prevRow2     (if prevCol (cdr (assoc gy prevCol)) nil)
              prevRow3     (if prevCol (cdr (assoc (1+ gy) prevCol)) nil)
              belowRow     (cdr (assoc (1- gy) curColAccepted))
              cellAccepted nil)
        (foreach entry (reverse rawItems)
          (setq p (cadddr entry))
          (if (and (not (check-near p cellAccepted))
                   (not (check-near p belowRow))
                   (not (check-near p prevRow1))
                   (not (check-near p prevRow2))
                   (not (check-near p prevRow3)))
            (setq cellAccepted (cons p cellAccepted)
                  acceptedList (cons p acceptedList))
          )
        )
        (if cellAccepted
          (setq curColAccepted (cons (cons gy cellAccepted) curColAccepted))
        )
      )

      (setq acceptedList   nil
            acceptedGrid   nil
            curGx          nil
            curGy          nil
            cellRaw        nil
            curColAccepted nil)

      (foreach item sorted
        (setq gx (car item)
              gy (cadr item))
        (cond
          ((null curGx)
           (setq curGx   gx
                 curGy   gy
                 cellRaw (list item)))
          ((and (= gx curGx) (= gy curGy))
           (setq cellRaw (cons item cellRaw)))
          ((= gx curGx)
           (flush-cell curGx curGy cellRaw)
           (setq curGy   gy
                 cellRaw (list item)))
          (t
           (flush-cell curGx curGy cellRaw)
           (if curColAccepted
             (setq acceptedGrid (cons (cons curGx curColAccepted) acceptedGrid))
           )
           (setq curGx          gx
                 curGy          gy
                 curColAccepted nil
                 cellRaw        (list item)))
        )
      )
      (if curGx
        (flush-cell curGx curGy cellRaw)
      )
      acceptedList
    )
  )
)


;;; --------------------------------------------------------------------------
;;; 3. DIRECTIONAL CORRIDOR GRID SNAP
;;; --------------------------------------------------------------------------

;; sw:corridor-grid-snap - Searches spatial grid along inward normal inDir
;; between lateral bounds pL..pR for the shallowest unobstructed object point.
(defun sw:corridor-grid-snap (pL pR inDir grid cellSize maxDepth existEdges /
                              dx dy len ux uy inUx inUy
                              numLat latDelta dStep curDist
                              visitedGrid visitedCol bestPt minDepth
                              w ptOnSlice gx gy bucket p vx vy tVal dIn)
  (setq dx  (- (car pR) (car pL))
        dy  (- (cadr pR) (cadr pL))
        len (sqrt (+ (* dx dx) (* dy dy))))
  (if (> len (* cellSize 1e-4))
    (progn
      (setq ux          (/ dx len)
            uy          (/ dy len)
            inUx        (car inDir)
            inUy        (cadr inDir)
            numLat      (max 1 (fix (+ (/ len (* 0.75 cellSize)) 1.0)))
            latDelta    (/ 1.0 (float numLat))
            dStep       (* 0.75 cellSize)
            curDist     (* 0.25 cellSize)
            visitedGrid nil
            bestPt      nil
            minDepth    1e20)

      ;; Continue slightly past minDepth so closer candidates in adjacent cells are never missed
      (while (and (<= curDist maxDepth) (<= curDist (+ minDepth cellSize)))
        (setq w 0)
        (while (<= w numLat)
          (setq ptOnSlice
            (list (+ (car pL) (* w latDelta dx) (* inUx curDist))
                  (+ (cadr pL) (* w latDelta dy) (* inUy curDist))))
          (setq gx (sw:coord-to-cell (car ptOnSlice) cellSize)
                gy (sw:coord-to-cell (cadr ptOnSlice) cellSize)
                visitedCol (cdr (assoc gx visitedGrid)))
          (if (not (and visitedCol (member gy visitedCol)))
            (progn
              (if visitedCol
                (setq visitedGrid (subst (cons gx (cons gy visitedCol)) (assoc gx visitedGrid) visitedGrid))
                (setq visitedGrid (cons (list gx gy) visitedGrid))
              )
              (setq bucket (sw:grid-lookup grid gx gy))
              (while bucket
                (setq p      (car bucket)
                      bucket (cdr bucket)
                      vx     (- (car p) (car pL))
                      vy     (- (cadr p) (cadr pL))
                      tVal   (+ (* vx ux) (* vy uy))
                      dIn    (+ (* vx inUx) (* vy inUy)))
                ;; 1. Lateral bounds (3% margin avoids re-selecting endpoints)
                ;; 2. Strictly in front of segment along inward vector
                ;; 3. Shallowest inward depth within maxDepth
                ;; 4. Line-of-sight free of edge intersections
                (if (and (> tVal (* 0.03 len))
                         (< tVal (* 0.97 len))
                         (> dIn (* 0.15 cellSize))
                         (<= dIn maxDepth)
                         (< dIn minDepth))
                  (if (and (not (sw:edge-collides-p pL p existEdges))
                           (not (sw:edge-collides-p p pR existEdges)))
                    (setq minDepth dIn
                          bestPt   p)
                  )
                )
              )
            )
          )
          (setq w (1+ w))
        )
        (setq curDist (+ curDist dStep))
      )
      bestPt
    )
    nil
  )
)


;;; --------------------------------------------------------------------------
;;; 4. ENTITY SAMPLING & UCS BOUNDING BOX EXTRACTION
;;; --------------------------------------------------------------------------

;; sw:extract-entity-points - Extracts WCS vertex and sampled points from one entity
;; Returns: 'INFINITE for XLINE/RAY, or (vertPts . samplePts), or nil on error.
(defun sw:extract-entity-points (ent step / ed eType obj sp ep crvLen localStep
                                            curDist idx maxIdx maxSamples
                                            vertPts samplePts pt bbox pMin pMax
                                            x1 y1 z1 x2 y2 z2 qStep code)
  (setq maxSamples 25000)
  (if (and ent (= (type ent) 'ENAME) (setq ed (entget ent)))
    (progn
      (setq eType (cdr (assoc 0 ed)))
      (cond
        ;; Infinite construction rays cannot form a finite shrinkwrap
        ((member eType '("XLINE" "RAY"))
         'INFINITE
        )

        ;; Point entity
        ((= eType "POINT")
         (setq pt (cdr (assoc 10 ed)))
         (if (sw:valid-pt-p pt)
           (cons (list pt) nil)
           nil
         )
        )

        ;; Planar solid / 3D face vertices
        ((member eType '("SOLID" "TRACE" "3DFACE"))
         (setq vertPts nil)
         (foreach code '(10 11 12 13)
           (if (setq pt (cdr (assoc code ed)))
             (progn
               (if (member eType '("SOLID" "TRACE"))
                 (setq pt (trans pt ent 0))
               )
               (if (sw:valid-pt-p pt)
                 (setq vertPts (cons pt vertPts))
               )
             )
           )
         )
         (if vertPts (cons vertPts nil) nil)
        )

        ;; Curve entities via vlax-curve-*
        ((and (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
              (not (vl-catch-all-error-p obj))
              (= (type obj) 'VLA-OBJECT)
              (not (vl-catch-all-error-p
                     (setq ep (vl-catch-all-apply 'vlax-curve-getEndParam (list obj)))))
              (numberp ep))
         (setq vertPts   nil
               samplePts nil
               sp        (vl-catch-all-apply 'vlax-curve-getStartParam (list obj)))

         ;; Explicit vertices for poly-curves so sharp corners are never skipped
         (if (and (member eType '("LWPOLYLINE" "POLYLINE"))
                  (not (vl-catch-all-error-p sp))
                  (numberp sp))
           (progn
             (setq idx    (fix sp)
                   maxIdx (min (fix ep) (+ (fix sp) maxSamples)))
             (while (<= idx maxIdx)
               (setq pt (vl-catch-all-apply 'vlax-curve-getPointAtParam (list obj (float idx))))
               (if (and (not (vl-catch-all-error-p pt)) (sw:valid-pt-p pt))
                 (setq vertPts (cons pt vertPts))
               )
               (setq idx (1+ idx))
             )
           )
         )

         ;; Cardinal quadrants for circles so small circles keep >= 4 points
         (if (= eType "CIRCLE")
           (foreach qStep (list 0.0 (* 0.5 pi) pi (* 1.5 pi))
             (setq pt (vl-catch-all-apply 'vlax-curve-getPointAtParam (list obj qStep)))
             (if (and (not (vl-catch-all-error-p pt)) (sw:valid-pt-p pt))
               (setq vertPts (cons pt vertPts))
             )
           )
         )

         ;; Start & end points for all curves
         (setq pt (vl-catch-all-apply 'vlax-curve-getStartPoint (list obj)))
         (if (and (not (vl-catch-all-error-p pt)) (sw:valid-pt-p pt))
           (setq vertPts (cons pt vertPts))
         )
         (setq pt (vl-catch-all-apply 'vlax-curve-getEndPoint (list obj)))
         (if (and (not (vl-catch-all-error-p pt)) (sw:valid-pt-p pt))
           (setq vertPts (cons pt vertPts))
         )

         ;; Adaptive arc-length sampling
         (setq crvLen (vl-catch-all-apply 'vlax-curve-getDistAtParam (list obj ep)))
         (if (and (not (vl-catch-all-error-p crvLen))
                  (numberp crvLen)
                  (> crvLen 1e-9)
                  (> step 1e-12))
           (progn
             (setq localStep (max step (/ crvLen (float maxSamples)))
                   curDist   localStep)
             (while (< curDist crvLen)
               (setq pt (vl-catch-all-apply 'vlax-curve-getPointAtDist (list obj curDist)))
               (if (and (not (vl-catch-all-error-p pt)) (sw:valid-pt-p pt))
                 (setq samplePts (cons pt samplePts))
               )
               (setq curDist (+ curDist localStep))
             )
           )
         )
         (if (or vertPts samplePts)
           (cons vertPts samplePts)
           nil
         )
        )

        ;; Fallback for blocks, text, hatches, regions, solids, dimensions, etc.
        (t
         (setq bbox (if (boundp 'CadSetup:GetBoundingBox)
                      (CadSetup:GetBoundingBox ent)
                      (progn
                        (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
                        (if (and obj (not (vl-catch-all-error-p obj))
                                 (not (vl-catch-all-error-p
                                        (vl-catch-all-apply 'vla-getboundingbox (list obj 'pMin 'pMax)))))
                          (list (vlax-safearray->list pMin)
                                (vlax-safearray->list pMax))
                          nil
                        )
                      )))
         (if (and bbox (sw:valid-pt-p (car bbox)) (sw:valid-pt-p (cadr bbox)))
           (progn
             (setq pMin (car bbox) pMax (cadr bbox)
                   x1 (car pMin) y1 (cadr pMin) z1 (if (caddr pMin) (caddr pMin) 0.0)
                   x2 (car pMax) y2 (cadr pMax) z2 (if (caddr pMax) (caddr pMax) 0.0)
                   vertPts (list (list x1 y1 z1)
                                 (list x2 y1 z1)
                                 (list x2 y2 z1)
                                 (list x1 y2 z1)))
             (if (> (abs (- z2 z1)) 1e-9)
               (setq vertPts (append vertPts
                                     (list (list x1 y1 z2)
                                           (list x2 y1 z2)
                                           (list x2 y2 z2)
                                           (list x1 y2 z2))))
             )
             (cons vertPts nil)
           )
           nil
         )
        )
      )
    )
    nil
  )
)

;; sw:compute-ucs-bbox - Computes tight 2D UCS bounding box across finite entities
;; Returns (list (minX minY) (maxX maxY) validEnts skippedInfiniteCount)
(defun sw:compute-ucs-bbox (ss / i ent ed eType bbox pMinW pMaxW
                                 z1 z2 ptW ptU minX minY maxX maxY
                                 validEnts skippedInf)
  (setq i 0
        minX 1e30 minY 1e30
        maxX -1e30 maxY -1e30
        validEnts nil
        skippedInf 0)
  (while (< i (sslength ss))
    (setq ent   (ssname ss i)
          ed    (entget ent)
          eType (if ed (cdr (assoc 0 ed)) ""))
    (if (member eType '("XLINE" "RAY"))
      (setq skippedInf (1+ skippedInf))
      (progn
        (setq bbox (if (boundp 'CadSetup:GetBoundingBox)
                     (CadSetup:GetBoundingBox ent)
                     nil))
        (if (and bbox (sw:valid-pt-p (car bbox)) (sw:valid-pt-p (cadr bbox)))
          (progn
            (setq validEnts (cons ent validEnts)
                  pMinW (car bbox)
                  pMaxW (cadr bbox)
                  z1    (if (caddr pMinW) (caddr pMinW) 0.0)
                  z2    (if (caddr pMaxW) (caddr pMaxW) 0.0))
            (foreach ptW (append
                           (list (list (car pMinW) (cadr pMinW) z1)
                                 (list (car pMaxW) (cadr pMinW) z1)
                                 (list (car pMaxW) (cadr pMaxW) z1)
                                 (list (car pMinW) (cadr pMaxW) z1))
                           (if (> (abs (- z2 z1)) 1e-9)
                             (list (list (car pMinW) (cadr pMinW) z2)
                                   (list (car pMaxW) (cadr pMinW) z2)
                                   (list (car pMaxW) (cadr pMaxW) z2)
                                   (list (car pMinW) (cadr pMaxW) z2))
                             nil
                           ))
              (setq ptU  (trans ptW 0 1)
                    minX (min minX (float (car ptU)))
                    minY (min minY (float (cadr ptU)))
                    maxX (max maxX (float (car ptU)))
                    maxY (max maxY (float (cadr ptU))))
            )
          )
        )
      )
    )
    (setq i (1+ i))
  )
  (if (and validEnts (< minX 1e29) (< minY 1e29))
    (list (list minX minY) (list maxX maxY) (reverse validEnts) skippedInf)
    (list nil nil nil skippedInf)
  )
)


;;; --------------------------------------------------------------------------
;;; 5. POLYLINE & PREVIEW ENTITY HELPERS
;;; --------------------------------------------------------------------------

;; sw:get-active-space - Returns PaperSpace when in layout paper, else ModelSpace
(defun sw:get-active-space (doc)
  (if (and (= (getvar "TILEMODE") 0) (= (getvar "CVPORT") 1))
    (if (boundp 'CadSetup:GetPaperSpace)
      (CadSetup:GetPaperSpace)
      (vl-catch-all-apply 'vla-get-PaperSpace (list doc))
    )
    (if (boundp 'CadSetup:GetModelSpace)
      (CadSetup:GetModelSpace)
      (vl-catch-all-apply 'vla-get-ModelSpace (list doc))
    )
  )
)

;; sw:ucs-to-wcs - Converts local shifted 2D UCS point back to 3D WCS
(defun sw:ucs-to-wcs (ptLocal originUcs zElev)
  (trans (list (+ (car ptLocal) (car originUcs))
               (+ (cadr ptLocal) (cadr originUcs))
               zElev)
         1 0)
)

;; sw:pts-to-ocs-coords - Converts local shifted 2D UCS points to flat OCS double list
(defun sw:pts-to-ocs-coords (ptsLocal originUcs zElev ucsNormal isWcsIdentity / res p ptW ptO)
  (if isWcsIdentity
    (progn
      (foreach p (reverse ptsLocal)
        (setq res (cons (float (+ (car p) (car originUcs)))
                        (cons (float (+ (cadr p) (cadr originUcs))) res)))
      )
      res
    )
    (progn
      (foreach p (reverse ptsLocal)
        (setq ptW (sw:ucs-to-wcs p originUcs zElev)
              ptO (trans ptW 0 ucsNormal)
              res (cons (float (car ptO))
                        (cons (float (cadr ptO)) res)))
      )
      res
    )
  )
)

;; sw:create-polyline - Creates a LightWeightPolyline in space aligned to active UCS
(defun sw:create-polyline (space ptsLocal originUcs zElev ucsNormal ocsElev isWcsIdentity
                           col width closedFlag / coordList sa polyObj)
  (setq coordList (sw:pts-to-ocs-coords ptsLocal originUcs zElev ucsNormal isWcsIdentity))
  (if (>= (length coordList) 4)
    (progn
      (setq sa (vlax-make-safearray vlax-vbDouble (cons 0 (1- (length coordList)))))
      (vlax-safearray-fill sa coordList)
      (setq polyObj (vl-catch-all-apply 'vla-AddLightWeightPolyline (list space sa)))
      (if (and polyObj (not (vl-catch-all-error-p polyObj)) (= (type polyObj) 'VLA-OBJECT))
        (progn
          (if (not isWcsIdentity)
            (progn
              (vl-catch-all-apply 'vla-put-Normal (list polyObj (vlax-3d-point ucsNormal)))
              (vl-catch-all-apply 'vla-put-Elevation (list polyObj (float ocsElev)))
            )
          )
          (if col   (vl-catch-all-apply 'vla-put-Color (list polyObj col)))
          (if width (vl-catch-all-apply 'vla-put-ConstantWidth (list polyObj (float width))))
          (vl-catch-all-apply 'vla-put-Closed (list polyObj (if closedFlag :vlax-true :vlax-false)))
          (vl-catch-all-apply 'vla-Update (list polyObj))
          polyObj
        )
        nil
      )
    )
    nil
  )
)

;; sw:update-polyline - Updates polyline coordinates or safely recreates if needed
(defun sw:update-polyline (polyObj space ptsLocal originUcs zElev ucsNormal ocsElev isWcsIdentity
                           col width closedFlag / coordList sa res)
  (setq coordList (sw:pts-to-ocs-coords ptsLocal originUcs zElev ucsNormal isWcsIdentity))
  (if (>= (length coordList) 4)
    (progn
      (setq sa (vlax-make-safearray vlax-vbDouble (cons 0 (1- (length coordList)))))
      (vlax-safearray-fill sa coordList)
      (if (and polyObj (= (type polyObj) 'VLA-OBJECT) (not (vlax-erased-p polyObj)))
        (setq res (vl-catch-all-apply 'vla-put-Coordinates (list polyObj sa)))
        (setq res (vl-catch-all-apply 'error (list "no-poly")))
      )
      (if (vl-catch-all-error-p res)
        (progn
          (if (and polyObj (= (type polyObj) 'VLA-OBJECT) (not (vlax-erased-p polyObj)))
            (vl-catch-all-apply 'vla-Delete (list polyObj))
          )
          (sw:create-polyline space ptsLocal originUcs zElev ucsNormal ocsElev isWcsIdentity
                              col width closedFlag)
        )
        (progn
          (if col   (vl-catch-all-apply 'vla-put-Color (list polyObj col)))
          (if width (vl-catch-all-apply 'vla-put-ConstantWidth (list polyObj (float width))))
          (vl-catch-all-apply 'vla-put-Closed (list polyObj (if closedFlag :vlax-true :vlax-false)))
          (vl-catch-all-apply 'vla-Update (list polyObj))
          polyObj
        )
      )
    )
    polyObj
  )
)

;; sw:make-preview-point - Creates a temporary visualization POINT on layer "0"
(defun sw:make-preview-point (ptLocal originUcs zElev aci / ptW)
  (setq ptW (sw:ucs-to-wcs ptLocal originUcs zElev))
  (entmakex (list '(0 . "POINT")
                  '(8 . "0")
                  (cons 10 ptW)
                  (cons 62 aci)))
)

;; sw:make-preview-line - Creates a temporary visualization LINE on layer "0"
(defun sw:make-preview-line (p1Local p2Local originUcs zElev aci / p1W p2W)
  (setq p1W (sw:ucs-to-wcs p1Local originUcs zElev)
        p2W (sw:ucs-to-wcs p2Local originUcs zElev))
  (entmakex (list '(0 . "LINE")
                  '(8 . "0")
                  (cons 10 p1W)
                  (cons 11 p2W)
                  (cons 62 aci)))
)

;; sw:cleanup-entities - Safely deletes a list of temporary enames
(defun sw:cleanup-entities (entList / e)
  (foreach e entList
    (if (and e (= (type e) 'ENAME) (entget e))
      (vl-catch-all-apply 'entdel (list e))
    )
  )
  nil
)

;; sw:unlock-layer-temp - Unlocks layName if currently locked; returns layObj if modified
(defun sw:unlock-layer-temp (doc layName / layers layObj res)
  (if (and doc layName (= (type layName) 'STR) (tblsearch "LAYER" layName))
    (progn
      (setq layers (vl-catch-all-apply 'vla-get-Layers (list doc)))
      (if (and layers (not (vl-catch-all-error-p layers)))
        (progn
          (setq layObj (vl-catch-all-apply 'vla-Item (list layers layName)))
          (if (and layObj
                   (not (vl-catch-all-error-p layObj))
                   (= (type layObj) 'VLA-OBJECT)
                   (= (vla-get-Lock layObj) :vlax-true))
            (progn
              (setq res (vl-catch-all-apply 'vla-put-Lock (list layObj :vlax-false)))
              (if (not (vl-catch-all-error-p res))
                layObj
                nil
              )
            )
            nil
          )
        )
        nil
      )
    )
    nil
  )
)

;; sw:restore-layer-lock - Re-locks a previously unlocked VLA layer object
(defun sw:restore-layer-lock (layObj)
  (if (and layObj (= (type layObj) 'VLA-OBJECT))
    (vl-catch-all-apply 'vla-put-Lock (list layObj :vlax-true))
  )
  nil
)


;;; --------------------------------------------------------------------------
;;; 6. SANITIZED USER PROMPTS
;;; --------------------------------------------------------------------------

;; sw:prompt-tightness - Prompts for tightness preset; returns (factor . isConvex)
(defun sw:prompt-tightness ( / kw )
  (initget "Convex Loose Medium Tight Ultra-tight")
  (setq kw (getkword "\nSpecify tightness [Convex/Loose/Medium/Tight/Ultra-tight] <Tight>: "))
  (if (null kw) (setq kw "Tight"))
  (setq kw (strcase kw))
  (cond
    ((= kw "CONVEX")      (cons 100.0 T))
    ((= kw "LOOSE")       (cons 5.0   nil))
    ((= kw "MEDIUM")      (cons 3.0   nil))
    ((= kw "ULTRA-TIGHT") (cons 1.1   nil))
    (t                    (cons 1.8   nil))
  )
)

;; sw:prompt-speed - Prompts for animation delay; returns delay in ms (0 = disabled)
(defun sw:prompt-speed ( / kw )
  (initget "None Fast Medium Slow")
  (setq kw (getkword "\nSpecify animation speed [None/Fast/Medium/Slow] <None>: "))
  (if (null kw) (setq kw "None"))
  (setq kw (strcase kw))
  (cond
    ((= kw "FAST")   3)
    ((= kw "MEDIUM") 15)
    ((= kw "SLOW")   45)
    (t               0)
  )
)

;; sw:prompt-yes-no - Sanitized Yes/No keyword prompt with boolean default
(defun sw:prompt-yes-no (msg defaultYes / kw)
  (initget "Yes No")
  (setq kw (getkword msg))
  (if (null kw)
    defaultYes
    (= (strcase kw) "YES")
  )
)


;;; --------------------------------------------------------------------------
;;; 7. CONCAVE HULL ENGINE
;;; --------------------------------------------------------------------------

;; sw:compute-concave-hull - Radial K-nearest / angle-sweep concave hull in local 2D UCS
;; Returns (list closedFlag hullOrderedPts updatedPolyObj)
(defun sw:compute-concave-hull (pts stepDist diag factor delayMs
                                space originUcs zElev ucsNormal ocsElev isWcsIdentity
                                ribWidth polyObj /
                                p0 tieTol ptTol remainingPts hull current prevBack
                                baseRadius searchRadius maxRadius closed iter maxIter
                                candList subCand cx cy p bestCand nextPt existingEdges
                                thetaBack scored sorted candEntry ang diff
                                forceNearest distPairs cnt)
  (setq tieTol       (* diag 1e-9)
        ptTol        (* stepDist 1e-4)
        baseRadius   (* stepDist factor)
        searchRadius baseRadius
        maxRadius    (* diag 1.5))

  ;; Anchor start point at lowest-Y (tie-break lowest-X)
  (setq p0 (car pts))
  (foreach p (cdr pts)
    (if (or (< (cadr p) (- (cadr p0) tieTol))
            (and (<= (abs (- (cadr p) (cadr p0))) tieTol)
                 (< (car p) (car p0))))
      (setq p0 p)
    )
  )

  (setq remainingPts (vl-remove-if '(lambda (pt) (equal pt p0 ptTol)) pts)
        hull         (list p0)
        current      p0
        prevBack     '(0.0 -1.0)
        closed       nil
        forceNearest nil
        iter         0
        maxIter      (* 4 (length pts)))

  (while (and (not closed) (< iter maxIter))
    (setq iter     (1+ iter)
          candList (if (>= (length hull) 3) (cons p0 remainingPts) remainingPts))

    (if (null candList)
      (setq iter maxIter)
      (progn
        (setq cx      (car current)
              cy      (cadr current)
              subCand nil)

        (if (not forceNearest)
          (foreach p candList
            (if (and (<= (abs (- (car p) cx)) searchRadius)
                     (<= (abs (- (cadr p) cy)) searchRadius)
                     (<= (distance current p) searchRadius))
              (setq subCand (cons p subCand))
            )
          )
        )

        ;; Fallback to nearest candidates when search radius has no points or stalled
        (if (or (null subCand) forceNearest)
          (progn
            (setq distPairs (mapcar '(lambda (pt) (cons (distance current pt) pt)) candList)
                  distPairs (vl-sort distPairs '(lambda (a b) (< (car a) (car b))))
                  subCand   nil
                  cnt       0)
            (while (and distPairs (< cnt 15))
              (setq subCand   (cons (cdar distPairs) subCand)
                    distPairs (cdr distPairs)
                    cnt       (1+ cnt))
            )
          )
        )

        ;; Sort candidates by CCW sweep angle from prevBack, tie-break by shorter distance
        (setq thetaBack (atan (cadr prevBack) (car prevBack))
              scored    nil)
        (foreach p subCand
          (setq ang  (atan (- (cadr p) cy) (- (car p) cx))
                diff (- ang thetaBack))
          (while (<= diff 0.0)       (setq diff (+ diff (* 2.0 pi))))
          (while (> diff (* 2.0 pi)) (setq diff (- diff (* 2.0 pi))))
          (setq scored (cons (list diff (distance current p) p) scored))
        )
        (setq sorted
          (vl-sort scored
            '(lambda (a b)
               (if (= (car a) (car b))
                 (< (cadr a) (cadr b))
                 (< (car a) (car b))
               )
             )
          )
        )

        ;; Pick first candidate whose segment does not intersect existing hull edges
        (setq bestCand nil)
        (while (and sorted (null bestCand))
          (setq candEntry (car sorted)
                sorted    (cdr sorted)
                nextPt    (caddr candEntry))
          (if (equal nextPt p0 ptTol)
            (setq existingEdges (sw:make-edges (cdr (reverse (cdr hull)))))
            (setq existingEdges (sw:make-edges (cdr hull)))
          )
          (if (not (sw:edge-collides-p current nextPt existingEdges))
            (setq bestCand nextPt)
          )
        )

        (if bestCand
          (progn
            (setq forceNearest nil)
            (if (equal bestCand p0 ptTol)
              (setq closed T)
              (progn
                (setq hull         (cons bestCand hull)
                      remainingPts (vl-remove bestCand remainingPts)
                      prevBack     (list (- (car current) (car bestCand))
                                         (- (cadr current) (cadr bestCand)))
                      current      bestCand
                      searchRadius baseRadius)
              )
            )
            (if (and (> delayMs 0) (>= (length hull) 2))
              (progn
                (setq polyObj
                  (sw:update-polyline polyObj space (reverse hull)
                                      originUcs zElev ucsNormal ocsElev isWcsIdentity
                                      3 ribWidth nil))
                (command "._delay" delayMs)
              )
            )
          )
          (progn
            (if (>= searchRadius (* baseRadius 4.0))
              (if forceNearest
                (setq searchRadius (* searchRadius 1.5))
                (setq forceNearest T)
              )
              (setq searchRadius (min (* searchRadius 1.35) (* baseRadius 4.0)))
            )
            (if (> searchRadius maxRadius)
              (setq iter maxIter)
            )
          )
        )
      )
    )
  )

  (list closed (reverse hull) polyObj)
)


;;; --------------------------------------------------------------------------
;;; 8. INWARD CORRIDOR SHRINKWRAP REFINEMENT ENGINE
;;; --------------------------------------------------------------------------

;; sw:analyze-polygon-edges - Discretizes edges into stepDist samples & red islands
(defun sw:analyze-polygon-edges (curPolyPts spatialGrid stepDist stepDistSq tol tolSq
                                 originUcs zElev delayMs verifyEnts /
                                 N edgeData i pA pB segLen subSteps stepDelta
                                 sampleList anyRedOnEdge j curPt hasNear
                                 islands curStart item jIdx isR
                                 red0 red1 redEnd redPrevEnd hasInteriorGreen
                                 isRealGeom ePt)
  (setq N        (length curPolyPts)
        edgeData nil
        i        0)
  (while (< i N)
    (setq pA           (nth i curPolyPts)
          pB           (nth (rem (1+ i) N) curPolyPts)
          segLen       (distance pA pB)
          subSteps     (max 1 (fix (+ (/ segLen stepDist) 0.5)))
          stepDelta    (/ 1.0 (float subSteps))
          sampleList   nil
          anyRedOnEdge nil
          j            0)

    (while (<= j subSteps)
      (setq curPt (list (+ (car pA) (* (- (car pB) (car pA)) (* j stepDelta)))
                        (+ (cadr pA) (* (- (cadr pB) (cadr pA)) (* j stepDelta))))
            hasNear (sw:grid-has-near-p curPt spatialGrid stepDist stepDistSq))
      (if (not hasNear)
        (progn
          (if (> delayMs 0)
            (if (setq ePt (sw:make-preview-point curPt originUcs zElev 1))
              (setq verifyEnts (cons ePt verifyEnts))
            )
          )
          (setq sampleList   (cons (list j curPt T) sampleList)
                anyRedOnEdge T)
        )
        (progn
          (if (> delayMs 0)
            (if (setq ePt (sw:make-preview-point curPt originUcs zElev 4))
              (setq verifyEnts (cons ePt verifyEnts))
            )
          )
          (setq sampleList (cons (list j curPt nil) sampleList))
        )
      )
      (setq j (1+ j))
    )
    (setq sampleList (reverse sampleList))

    ;; Identify contiguous red sample islands (sIdx eIdx)
    (setq islands nil curStart nil)
    (foreach item sampleList
      (setq jIdx (car item)
            isR  (caddr item))
      (if isR
        (if (null curStart) (setq curStart jIdx))
        (if curStart
          (progn
            (setq islands  (cons (list curStart (1- jIdx)) islands)
                  curStart nil)
          )
        )
      )
    )
    (if curStart
      (setq islands (cons (list curStart subSteps) islands))
    )
    (setq islands (reverse islands))

    ;; Interior green presence (0 < j < subSteps)
    (setq hasInteriorGreen nil
          j 1)
    (while (and (< j subSteps) (not hasInteriorGreen))
      (if (not (caddr (nth j sampleList)))
        (setq hasInteriorGreen T)
      )
      (setq j (1+ j))
    )

    (setq red0             (caddr (nth 0 sampleList))
          red1             (if (> subSteps 0) (caddr (nth 1 sampleList)) nil)
          redEnd           (caddr (last sampleList))
          redPrevEnd       (if (> subSteps 0) (caddr (nth (1- subSteps) sampleList)) nil)
          isRealGeom       (sw:grid-has-near-p pA spatialGrid tol tolSq))

    ;; Record layout:
    ;; 0:idx 1:pA 2:pB 3:subSteps 4:sampleList 5:anyRedOnEdge
    ;; 6:red0 7:red1 8:redEnd 9:redPrevEnd 10:islands 11:hasInteriorGreen 12:isRealGeom
    (setq edgeData
      (cons (list i pA pB subSteps sampleList anyRedOnEdge
                  red0 red1 redEnd redPrevEnd islands hasInteriorGreen isRealGeom)
            edgeData))
    (setq i (1+ i))
  )
  (list (reverse edgeData) verifyEnts)
)

;; sw:compute-inward-dir - Returns unit inward normal for chord pL -> pR
(defun sw:compute-inward-dir (pL pR fallbackA fallbackB isCCW / dx dy d ux uy)
  (setq dx (- (car pR) (car pL))
        dy (- (cadr pR) (cadr pL))
        d  (sqrt (+ (* dx dx) (* dy dy))))
  (if (and (< d 1e-10) fallbackA fallbackB)
    (setq dx (- (car fallbackB) (car fallbackA))
          dy (- (cadr fallbackB) (cadr fallbackA))
          d  (sqrt (+ (* dx dx) (* dy dy))))
  )
  (if (> d 1e-10)
    (setq ux (/ dx d) uy (/ dy d))
    (setq ux 1.0 uy 0.0)
  )
  (if isCCW
    (list (- uy) ux)
    (list uy (- ux))
  )
)

;; sw:refine-inward - Iterative corridor-projection shrinkwrap loop
;; Returns (list finalPtsLocal convergedFlag genCount updatedPolyObj)
(defun sw:refine-inward (initPtsLocal ptsLocal spatialGrid stepDist diag tol
                         delayMs space originUcs zElev ucsNormal ocsElev isWcsIdentity
                         ribWidth polyObj /
                         curPolyPts gen maxGen anyRed verifyEnts
                         stepDistSq tolSq area2 isCCW N analysisRes edgeData
                         allEdges i prevEdge currEdge vBridgeList isBridge
                         anchorIdx hasAnyGreen newPolyPts newEdges snappedAny
                         emit-pt process-island
                         edgeRec pA pB subSteps currSamples islands
                         endV prevIdx prevEdgeRec prevSamples endEdgeRec endSamples endSubSteps
                         kL kR ptLeft ptRight inDir snapPt eTmp vIdx sIdx eIdx
                         isExistingTail nextBridge ringSamples firstGreenIdx
                         curGreen curRedSpan pNext j item island)

  (setq curPolyPts initPtsLocal
        stepDistSq (* stepDist stepDist)
        tolSq      (* tol tol)
        gen        0
        maxGen     40
        anyRed     T
        verifyEnts nil)

  ;; Emits vertex while maintaining incremental newEdges list for fast collision tests
  (defun emit-pt (pt)
    (if (and (sw:valid-pt-p pt)
             (or (null newPolyPts) (> (distance pt (car newPolyPts)) tol)))
      (progn
        (if newPolyPts
          (setq newEdges (cons (list (car newPolyPts) pt) newEdges))
        )
        (setq newPolyPts (cons pt newPolyPts))
      )
    )
  )

  ;; Snaps one unsupported island (sIdx .. eIdx) along edge pA -> pB
  (defun process-island (pA pB subSteps samples sIdx eIdx /
                         leftIdx rightIdx pL pR midPt dir sPt eTmp)
    (setq leftIdx  (max 0 (1- sIdx))
          rightIdx (min subSteps (1+ eIdx))
          pL       (cadr (nth leftIdx samples))
          pR       (cadr (nth rightIdx samples))
          midPt    (list (* 0.5 (+ (car pL) (car pR)))
                         (* 0.5 (+ (cadr pL) (cadr pR)))))
    (if (> delayMs 0)
      (progn
        (if (setq eTmp (sw:make-preview-point pL originUcs zElev 6)) (setq verifyEnts (cons eTmp verifyEnts)))
        (if (setq eTmp (sw:make-preview-point pR originUcs zElev 6)) (setq verifyEnts (cons eTmp verifyEnts)))
        (if (setq eTmp (sw:make-preview-point midPt originUcs zElev 5)) (setq verifyEnts (cons eTmp verifyEnts)))
      )
    )
    (setq dir (sw:compute-inward-dir pL pR pA pB isCCW)
          sPt (sw:corridor-grid-snap pL pR dir spatialGrid stepDist diag (append allEdges newEdges)))
    (if sPt
      (progn
        (setq snappedAny T)
        (if (> delayMs 0)
          (progn
            (if (setq eTmp (sw:make-preview-point sPt originUcs zElev 3)) (setq verifyEnts (cons eTmp verifyEnts)))
            (if (setq eTmp (sw:make-preview-line midPt sPt originUcs zElev 6)) (setq verifyEnts (cons eTmp verifyEnts)))
          )
        )
        (emit-pt pL)
        (emit-pt sPt)
        (emit-pt pR)
      )
      (progn
        (emit-pt pL)
        (emit-pt pR)
      )
    )
  )

  (while (and anyRed (< gen maxGen))
    (setq verifyEnts (sw:cleanup-entities verifyEnts))

    (if (or (null curPolyPts)
            (< (length curPolyPts) 3)
            (<= (abs (setq area2 (sw:polygon-area2 curPolyPts))) (* diag diag 1e-12)))
      (setq anyRed nil)
      (progn
        (setq gen        (1+ gen)
              anyRed     nil
              snappedAny nil
              newPolyPts nil
              newEdges   nil
              N          (length curPolyPts)
              isCCW      (> area2 0.0))

        (princ (strcat "\n[SHRINKWRAP] Refining generation " (itoa gen) "..."))

        ;; PASS 1: Discretize edges & detect unsupported red islands
        (setq analysisRes (sw:analyze-polygon-edges curPolyPts spatialGrid stepDist stepDistSq
                                                    tol tolSq originUcs zElev delayMs verifyEnts)
              edgeData    (car analysisRes)
              verifyEnts  (cadr analysisRes)
              anyRed      (vl-some '(lambda (e) (nth 5 e)) edgeData))

        (if anyRed
          (progn
            ;; PASS 2: Identify unsupported artificial bridge vertices flanked by red
            (setq i 0 vBridgeList nil)
            (while (< i N)
              (setq prevEdge (nth (rem (+ (1- i) N) N) edgeData)
                    currEdge (nth i edgeData)
                    isBridge (or (nth 6 currEdge)
                                 (and (not (nth 12 currEdge))
                                      (or (nth 8 prevEdge) (nth 9 prevEdge))
                                      (or (nth 6 currEdge) (nth 7 currEdge)))))
              (setq vBridgeList (cons isBridge vBridgeList)
                    i (1+ i))
            )
            (setq vBridgeList (reverse vBridgeList))

            ;; Find a non-bridge anchor vertex so Pass 3 starts cleanly without wrap-around
            (setq anchorIdx nil i 0)
            (while (and (< i N) (null anchorIdx))
              (if (not (nth i vBridgeList)) (setq anchorIdx i))
              (setq i (1+ i))
            )

            (if anchorIdx
              (progn
                (if (> anchorIdx 0)
                  (setq curPolyPts  (sw:rotate-list curPolyPts anchorIdx)
                        edgeData    (sw:rotate-list edgeData anchorIdx)
                        vBridgeList (sw:rotate-list vBridgeList anchorIdx))
                )
                (setq allEdges (sw:make-edges (append curPolyPts (list (car curPolyPts)))))

                ;; PASS 3: Monotonic directional corridor snap across bridged vertices & edge islands
                (setq i 0)
                (while (< i N)
                  (if (not (nth i vBridgeList))
                    (progn
                      (setq edgeRec     (nth i edgeData)
                            pA          (nth 1 edgeRec)
                            pB          (nth 2 edgeRec)
                            subSteps    (nth 3 edgeRec)
                            currSamples (nth 4 edgeRec)
                            islands     (nth 10 edgeRec)
                            nextBridge  (nth (rem (1+ i) N) vBridgeList))
                      (emit-pt (nth i curPolyPts))
                      (foreach island islands
                        (setq sIdx (car island)
                              eIdx (cadr island)
                              isExistingTail
                                (and nextBridge
                                     (or (= eIdx subSteps)
                                         (and (>= subSteps 2)
                                              (= eIdx (1- subSteps))
                                              (not (nth 8 edgeRec))))))
                        (if (not isExistingTail)
                          (process-island pA pB subSteps currSamples sIdx eIdx)
                        )
                      )
                      (setq i (1+ i))
                    )
                    ;; Bridged vertex (or consecutive bridged vertices without interior green anchors)
                    (progn
                      (setq endV i)
                      (while (and (< endV (1- N))
                                  (nth (1+ endV) vBridgeList)
                                  (not (nth 11 (nth endV edgeData))))
                        (setq endV (1+ endV))
                      )

                      ;; Locate last green anchor before bridge span
                      (setq prevIdx     (rem (+ (1- i) N) N)
                            prevEdgeRec (nth prevIdx edgeData)
                            prevSamples (nth 4 prevEdgeRec)
                            kL          (if (nth 8 prevEdgeRec)
                                          (nth 3 prevEdgeRec)
                                          (max 0 (1- (nth 3 prevEdgeRec))))
                            ptLeft      nil)
                      (while (and (>= kL 0) (null ptLeft))
                        (if (not (caddr (nth kL prevSamples)))
                          (setq ptLeft (cadr (nth kL prevSamples)))
                        )
                        (setq kL (1- kL))
                      )
                      (if (null ptLeft) (setq ptLeft (cadr (car prevSamples))))

                      ;; Locate first green anchor after bridge span
                      (setq endEdgeRec  (nth endV edgeData)
                            endSamples  (nth 4 endEdgeRec)
                            endSubSteps (nth 3 endEdgeRec)
                            kR          (if (nth 6 endEdgeRec) 0 (min endSubSteps 1))
                            ptRight     nil)
                      (while (and (<= kR endSubSteps) (null ptRight))
                        (if (not (caddr (nth kR endSamples)))
                          (setq ptRight (cadr (nth kR endSamples)))
                          (setq kR (1+ kR))
                        )
                      )
                      (if (null ptRight)
                        (progn
                          (setq kR      endSubSteps
                                ptRight (cadr (last endSamples)))
                        )
                      )

                      (if (> delayMs 0)
                        (progn
                          (if (setq eTmp (sw:make-preview-point ptLeft originUcs zElev 6)) (setq verifyEnts (cons eTmp verifyEnts)))
                          (if (setq eTmp (sw:make-preview-point ptRight originUcs zElev 6)) (setq verifyEnts (cons eTmp verifyEnts)))
                        )
                      )

                      (setq inDir  (sw:compute-inward-dir ptLeft ptRight nil nil isCCW)
                            snapPt (sw:corridor-grid-snap ptLeft ptRight inDir spatialGrid stepDist diag
                                                          (append allEdges newEdges)))
                      (if snapPt
                        (progn
                          (setq snappedAny T)
                          (if (> delayMs 0)
                            (if (setq eTmp (sw:make-preview-point snapPt originUcs zElev 3))
                              (setq verifyEnts (cons eTmp verifyEnts)))
                          )
                          (emit-pt ptLeft)
                          (emit-pt snapPt)
                          (emit-pt ptRight)
                        )
                        ;; Retain spanned corners if no safe inward snap was found
                        (progn
                          (setq vIdx i)
                          (while (<= vIdx endV)
                            (emit-pt (nth vIdx curPolyPts))
                            (setq vIdx (1+ vIdx))
                          )
                        )
                      )

                      ;; Process any subsequent islands strictly beyond ptRight on endEdgeRec
                      (setq nextBridge (nth (rem (1+ endV) N) vBridgeList))
                      (foreach island (nth 10 endEdgeRec)
                        (setq sIdx (car island)
                              eIdx (cadr island)
                              isExistingTail
                                (and nextBridge
                                     (or (= eIdx endSubSteps)
                                         (and (>= endSubSteps 2)
                                              (= eIdx (1- endSubSteps))
                                              (not (nth 8 endEdgeRec))))))
                        (if (and (> sIdx kR) (not isExistingTail))
                          (process-island (nth 1 endEdgeRec) (nth 2 endEdgeRec)
                                          endSubSteps endSamples sIdx eIdx)
                        )
                      )
                      (setq i (1+ endV))
                    )
                  )
                )
              )
              ;; Fallback if all vertices became unsupported: snap across cyclic green-to-green arcs
              (progn
                (setq allEdges    (sw:make-edges (append curPolyPts (list (car curPolyPts))))
                      ringSamples nil
                      hasAnyGreen nil)
                (foreach edgeRec edgeData
                  (setq j 0
                        currSamples (nth 4 edgeRec)
                        subSteps    (nth 3 edgeRec))
                  (while (< j subSteps)
                    (setq item (nth j currSamples)
                          ringSamples (cons item ringSamples))
                    (if (not (caddr item)) (setq hasAnyGreen T))
                    (setq j (1+ j))
                  )
                )
                (setq ringSamples (reverse ringSamples))
                (if hasAnyGreen
                  (progn
                    (setq firstGreenIdx 0)
                    (while (caddr (nth firstGreenIdx ringSamples))
                      (setq firstGreenIdx (1+ firstGreenIdx))
                    )
                    (setq ringSamples (sw:rotate-list ringSamples firstGreenIdx)
                          curGreen    (cadr (car ringSamples))
                          curRedSpan  nil)
                    (emit-pt curGreen)
                    (foreach item (cdr ringSamples)
                      (if (caddr item)
                        (setq curRedSpan T)
                        (progn
                          (setq pNext (cadr item))
                          (if curRedSpan
                            (progn
                              (setq inDir  (sw:compute-inward-dir curGreen pNext nil nil isCCW)
                                    snapPt (sw:corridor-grid-snap curGreen pNext inDir spatialGrid stepDist diag
                                                                  (append allEdges newEdges)))
                              (if snapPt
                                (progn
                                  (setq snappedAny T)
                                  (emit-pt snapPt)
                                )
                              )
                              (setq curRedSpan nil)
                            )
                          )
                          (emit-pt pNext)
                          (setq curGreen pNext)
                        )
                      )
                    )
                    (if curRedSpan
                      (progn
                        (setq pNext  (cadr (car ringSamples))
                              inDir  (sw:compute-inward-dir curGreen pNext nil nil isCCW)
                              snapPt (sw:corridor-grid-snap curGreen pNext inDir spatialGrid stepDist diag
                                                            (append allEdges newEdges)))
                        (if snapPt
                          (progn
                            (setq snappedAny T)
                            (emit-pt snapPt)
                          )
                        )
                      )
                    )
                  )
                )
              )
            )

            ;; Assemble & validate generation result
            (setq newPolyPts (reverse newPolyPts))
            (if (and (> (length newPolyPts) 1)
                     (equal (car newPolyPts) (last newPolyPts) tol))
              (setq newPolyPts (cdr newPolyPts))
            )

            (if (and (>= (length newPolyPts) 3)
                     (> (abs (sw:polygon-area2 newPolyPts)) (* diag diag 1e-12)))
              (progn
                (setq curPolyPts newPolyPts)
                (setq polyObj
                  (sw:update-polyline polyObj space curPolyPts
                                      originUcs zElev ucsNormal ocsElev isWcsIdentity
                                      3 ribWidth T))
                (if (> delayMs 0)
                  (command "._delay" delayMs)
                )
                (if (not snappedAny)
                  (setq anyRed nil)
                )
              )
              (setq anyRed nil)
            )
          )
        )
      )
    )
  )

  (setq verifyEnts (sw:cleanup-entities verifyEnts))
  (list curPolyPts (not anyRed) gen polyObj)
)


;;; --------------------------------------------------------------------------
;;; 9. MAIN COMMAND: SHRINKWRAPVIS (ALIAS: WRAP)
;;; --------------------------------------------------------------------------

(defun c:SHRINKWRAPVIS ( / *error* acadApp doc space
                           oldCmdecho oldPdMode oldPdSize
                           lockedClayerObj lockedZeroObj
                           tempPtEnts verifyEnts polyObj isComplete
                           ss tightnessPair factor isConvex delayMs
                           bboxRes bbMinUcs bbMaxUcs validEnts skippedInf
                           diag stepDist tol originUcs zElev ucsNormal
                           origWcs origOcs ocsElev isWcsIdentity ribWidth
                           ent res vertPts samplePts ptU ptsLocal
                           hullRes closed hullLocal
                           spatialGrid refineRes converged genCount
                           snappedPts p bestPt polyEnt)

  (setq acadApp        (if (boundp 'CadSetup:GetAcad) (CadSetup:GetAcad) (vlax-get-acad-object))
        doc            (if (boundp 'CadSetup:GetDoc)  (CadSetup:GetDoc)  (if acadApp (vla-get-ActiveDocument acadApp)))
        oldCmdecho     (if (boundp 'CadSetup:SafeGetVar) (CadSetup:SafeGetVar "CMDECHO" 1) (getvar "CMDECHO"))
        oldPdMode      (if (boundp 'CadSetup:SafeGetVar) (CadSetup:SafeGetVar "PDMODE" 0)  (getvar "PDMODE"))
        oldPdSize      (if (boundp 'CadSetup:SafeGetVar) (CadSetup:SafeGetVar "PDSIZE" 0.0)(getvar "PDSIZE"))
        lockedClayerObj nil
        lockedZeroObj   nil
        tempPtEnts      nil
        verifyEnts      nil
        polyObj         nil
        isComplete      nil)

  ;; Localized error & cancellation handler with full entity & sysvar cleanup
  (defun *error* (msg)
    (setq tempPtEnts (sw:cleanup-entities tempPtEnts))
    (setq verifyEnts (sw:cleanup-entities verifyEnts))
    (if (and (not isComplete)
             polyObj
             (= (type polyObj) 'VLA-OBJECT)
             (not (vlax-erased-p polyObj)))
      (vl-catch-all-apply 'vla-Delete (list polyObj))
    )
    (if lockedClayerObj (sw:restore-layer-lock lockedClayerObj))
    (if lockedZeroObj   (sw:restore-layer-lock lockedZeroObj))
    (if oldPdMode  (vl-catch-all-apply 'setvar (list "PDMODE" oldPdMode)))
    (if oldPdSize  (vl-catch-all-apply 'setvar (list "PDSIZE" oldPdSize)))
    (if oldCmdecho (vl-catch-all-apply 'setvar (list "CMDECHO" oldCmdecho)))
    (if (boundp 'CadSetup:UndoReset) (CadSetup:UndoReset))
    (redraw)
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[SHRINKWRAP] Error: " msg))
    )
    (princ)
  )

  (if (or (null doc) (null (setq space (sw:get-active-space doc))) (vl-catch-all-error-p space))
    (princ "\n[SHRINKWRAP] ERR: Active AutoCAD document or space unavailable.")
    (progn
      (setvar "CMDECHO" 0)

      ;; 1. Acquire selection (support noun/verb pre-selection or interactive prompt)
      (setq ss (ssget "_I"))
      (if (and ss (= (type ss) 'PICKSET) (> (sslength ss) 0))
        (sssetfirst nil nil)
        (progn
          (princ "\n[SHRINKWRAP] Select geometry to shrinkwrap: ")
          (setq ss (ssget))
        )
      )

      (if (or (null ss) (/= (type ss) 'PICKSET) (= (sslength ss) 0))
        (princ "\n[SHRINKWRAP] No objects selected.")
        (progn
          ;; 2. Compute UCS bounding extent and validate finite entities
          (setq bboxRes    (sw:compute-ucs-bbox ss)
                bbMinUcs   (car bboxRes)
                bbMaxUcs   (cadr bboxRes)
                validEnts  (caddr bboxRes)
                skippedInf (cadddr bboxRes))

          (if (> skippedInf 0)
            (princ (strcat "\n[SHRINKWRAP] WARN: Skipped " (itoa skippedInf) " infinite XLINE/RAY entity(ies)."))
          )

          (if (or (null validEnts) (null bbMinUcs) (null bbMaxUcs))
            (princ "\n[SHRINKWRAP] ERR: No finite geometry found in selection.")
            (progn
              (setq diag (distance bbMinUcs bbMaxUcs))
              (if (< diag 1e-9)
                (princ "\n[SHRINKWRAP] ERR: Selected geometry has zero bounding extent.")
                (progn
                  ;; 3. Collect sanitized user parameters
                  (setq tightnessPair (sw:prompt-tightness)
                        factor        (car tightnessPair)
                        isConvex      (cdr tightnessPair)
                        delayMs       (sw:prompt-speed)
                        stepDist      (/ diag 100.0)
                        tol           (/ stepDist 4.0)
                        ribWidth      (/ diag 250.0)
                        originUcs     (list (float (car bbMinUcs)) (float (cadr bbMinUcs)))
                        zElev         (float (if (numberp (getvar "ELEVATION")) (getvar "ELEVATION") 0.0))
                        ucsNormal     (trans '(0.0 0.0 1.0) 1 0 T)
                        origWcs       (trans (list 0.0 0.0 zElev) 1 0)
                        origOcs       (trans origWcs 0 ucsNormal)
                        ocsElev       (float (caddr origOcs))
                        isWcsIdentity (and (equal ucsNormal '(0.0 0.0 1.0) 1e-12)
                                           (equal origWcs '(0.0 0.0 0.0) 1e-12)))

                  ;; 4. Sample geometry in WCS, project to UCS, shift to local origin
                  (princ "\n[SHRINKWRAP] Sampling selected entities...")
                  (setq vertPts   nil
                        samplePts nil)
                  (foreach ent validEnts
                    (setq res (sw:extract-entity-points ent stepDist))
                    (if (and res (not (eq res 'INFINITE)))
                      (progn
                        (foreach p (car res)
                          (setq ptU (trans p 0 1))
                          (if (sw:valid-pt-p ptU)
                            (setq vertPts
                              (cons (list (- (float (car ptU)) (car originUcs))
                                          (- (float (cadr ptU)) (cadr originUcs)))
                                    vertPts))
                          )
                        )
                        (foreach p (cdr res)
                          (setq ptU (trans p 0 1))
                          (if (sw:valid-pt-p ptU)
                            (setq samplePts
                              (cons (list (- (float (car ptU)) (car originUcs))
                                          (- (float (cadr ptU)) (cadr originUcs)))
                                    samplePts))
                          )
                        )
                      )
                    )
                  )

                  ;; Deduplicate preserving explicit corners/vertices first
                  (setq ptsLocal (sw:dedupe-points vertPts samplePts (/ stepDist 3.0)))

                  (if (< (length ptsLocal) 3)
                    (princ "\n[SHRINKWRAP] ERR: Insufficient distinct points generated (minimum 3 required).")
                    (progn
                      ;; 5. Begin atomic undo transaction & guard locked layers
                      (if (boundp 'CadSetup:UndoStart) (CadSetup:UndoStart))
                      (setq lockedClayerObj (sw:unlock-layer-temp doc (getvar "CLAYER")))
                      (if (> delayMs 0)
                        (setq lockedZeroObj (sw:unlock-layer-temp doc "0"))
                      )

                      ;; Render visual preview sampling points only when animation is active
                      (if (> delayMs 0)
                        (progn
                          (setvar "PDMODE" 35)
                          (setvar "PDSIZE" (/ diag 120.0))
                          (foreach p ptsLocal
                            (if (setq ent (sw:make-preview-point p originUcs zElev 2))
                              (setq tempPtEnts (cons ent tempPtEnts))
                            )
                          )
                          (setq polyObj
                            (sw:create-polyline space (list (car ptsLocal) (cadr ptsLocal))
                                                originUcs zElev ucsNormal ocsElev isWcsIdentity
                                                3 ribWidth nil))
                          (princ "\n[SHRINKWRAP] Streaming animated boundary tracking...")
                        )
                        (princ "\n[SHRINKWRAP] Calculating boundary...")
                      )

                      ;; 6. Compute outer concave/convex hull
                      (setq hullRes   (sw:compute-concave-hull ptsLocal stepDist diag factor delayMs
                                                               space originUcs zElev ucsNormal ocsElev isWcsIdentity
                                                               ribWidth polyObj)
                            closed    (car hullRes)
                            hullLocal (cadr hullRes)
                            polyObj   (caddr hullRes))

                      (setq tempPtEnts (sw:cleanup-entities tempPtEnts))

                      (if (or (not closed)
                              (< (length hullLocal) 3)
                              (<= (abs (sw:polygon-area2 hullLocal)) (* diag diag 1e-12)))
                        (progn
                          (if (and polyObj (= (type polyObj) 'VLA-OBJECT) (not (vlax-erased-p polyObj)))
                            (vl-catch-all-apply 'vla-Delete (list polyObj))
                          )
                          (setq polyObj nil)
                          (princ "\n[SHRINKWRAP] WARN: Could not close non-degenerate boundary cleanly. Try Medium or Loose tightness.")
                        )
                        (progn
                          ;; Create or finalize hull polyline
                          (if (null polyObj)
                            (setq polyObj
                              (sw:create-polyline space hullLocal originUcs zElev ucsNormal ocsElev isWcsIdentity
                                                  256 0.0 T))
                            (setq polyObj
                              (sw:update-polyline polyObj space hullLocal originUcs zElev ucsNormal ocsElev isWcsIdentity
                                                  256 0.0 T))
                          )

                          (princ (strcat "\n[SHRINKWRAP] OK: Boundary generated with "
                                         (itoa (length hullLocal)) " vertices."))

                          ;; 7. Optional inward corridor shrinkwrap refinement (Convex mode)
                          (if (and isConvex
                                   (sw:prompt-yes-no
                                     "\nConvex boundary completed. Further shrink inward? [Yes/No] <Yes>: "
                                     T))
                            (progn
                              (princ "\n[SHRINKWRAP] Indexing spatial lattice...")
                              (setq spatialGrid (sw:grid-build ptsLocal stepDist)
                                    refineRes   (sw:refine-inward hullLocal ptsLocal spatialGrid stepDist diag tol
                                                                  delayMs space originUcs zElev ucsNormal ocsElev isWcsIdentity
                                                                  ribWidth polyObj)
                                    hullLocal   (car refineRes)
                                    converged   (cadr refineRes)
                                    genCount    (caddr refineRes)
                                    polyObj     (cadddr refineRes))

                              (if converged
                                (princ (strcat "\n[SHRINKWRAP] OK: Converged at generation " (itoa genCount) "."))
                                (princ (strcat "\n[SHRINKWRAP] Reached generation limit (" (itoa genCount) ")."))
                              )

                              ;; Final pass: snap all vertices onto exact nearest sampled object points
                              (princ "\n[SHRINKWRAP] Snapping boundary vertices to nearest object points...")
                              (setq snappedPts nil)
                              (foreach p hullLocal
                                (setq bestPt     (sw:grid-nearest-pt p spatialGrid ptsLocal stepDist)
                                      snappedPts (sw:add-vertex bestPt snappedPts tol))
                              )
                              (setq snappedPts (reverse snappedPts))
                              (if (and (> (length snappedPts) 2)
                                       (equal (car snappedPts) (last snappedPts) tol))
                                (setq snappedPts (cdr snappedPts))
                              )
                              (if (and (>= (length snappedPts) 3)
                                       (> (abs (sw:polygon-area2 snappedPts)) (* diag diag 1e-12)))
                                (setq hullLocal snappedPts)
                              )

                              (setq polyObj
                                (sw:update-polyline polyObj space hullLocal originUcs zElev ucsNormal ocsElev isWcsIdentity
                                                    256 0.0 T))
                            )
                          )

                          ;; 8. Optional smooth arc fitting
                          (if (and polyObj
                                   (= (type polyObj) 'VLA-OBJECT)
                                   (not (vlax-erased-p polyObj))
                                   (sw:prompt-yes-no
                                     "\nFit smooth circular arcs through vertices? [Yes/No] <No>: "
                                     nil))
                            (progn
                              (setq polyEnt (vl-catch-all-apply 'vlax-vla-object->ename (list polyObj)))
                              (if (and polyEnt (not (vl-catch-all-error-p polyEnt)) (entget polyEnt))
                                (progn
                                  (vl-catch-all-apply 'command (list "._PEDIT" polyEnt "_F" ""))
                                  (while (> (getvar "CMDACTIVE") 0)
                                    (command "")
                                  )
                                )
                              )
                            )
                          )

                          ;; Select resulting boundary for immediate follow-up operations
                          (if (and polyObj
                                   (= (type polyObj) 'VLA-OBJECT)
                                   (not (vlax-erased-p polyObj))
                                   (setq polyEnt (vl-catch-all-apply 'vlax-vla-object->ename (list polyObj)))
                                   (not (vl-catch-all-error-p polyEnt)))
                            (sssetfirst nil (ssadd polyEnt))
                          )
                          (setq isComplete T)
                        )
                      )

                      ;; Restore locks & close undo group
                      (if lockedZeroObj
                        (progn (sw:restore-layer-lock lockedZeroObj) (setq lockedZeroObj nil))
                      )
                      (if lockedClayerObj
                        (progn (sw:restore-layer-lock lockedClayerObj) (setq lockedClayerObj nil))
                      )
                      (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  ;; Restore system variables & display
  (if oldPdMode  (vl-catch-all-apply 'setvar (list "PDMODE" oldPdMode)))
  (if oldPdSize  (vl-catch-all-apply 'setvar (list "PDSIZE" oldPdSize)))
  (if oldCmdecho (vl-catch-all-apply 'setvar (list "CMDECHO" oldCmdecho)))
  (redraw)
  (princ)
)

;;; ==========================================================================
;;; COMMAND ALIAS
;;; ==========================================================================
(defun c:WRAP () (c:SHRINKWRAPVIS))

(if *CadSetup-Debug*
  (princ "\n[09_Shrinkwrap.lsp] Concave hull & corridor-snap shrinkwrap loaded.")
)
(princ)
