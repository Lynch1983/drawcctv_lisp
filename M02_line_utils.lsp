;;;===============================================================
;;;  M02 - Line Utilities Module
;;;  Utility functions for LINE entity operations
;;;===============================================================
;;;  ENCODING: ANSI (ASCII only, no Chinese characters)
;;;  DEPENDENCIES: None
;;;===============================================================

;;;---------------------------------------------------------------
;;;  line-get-endpoints
;;;  Get start and end points of a LINE entity
;;;  Args: ent - entity name (ename)
;;;  Returns: (start-point end-point) or nil
;;;---------------------------------------------------------------
(defun line-get-endpoints (ent / elist etype)
  (setq elist (entget ent))
  (setq etype (cdr (assoc 0 elist)))
  (if (= etype "LINE")
    (list (cdr (assoc 10 elist)) (cdr (assoc 11 elist)))
    nil
  )
)

;;;---------------------------------------------------------------
;;;  line-get-startpoint
;;;  Get start point of a LINE entity
;;;  Args: ent - entity name
;;;  Returns: point or nil
;;;---------------------------------------------------------------
(defun line-get-startpoint (ent / elist)
  (setq elist (entget ent))
  (cdr (assoc 10 elist))
)

;;;---------------------------------------------------------------
;;;  line-get-endpoint
;;;  Get end point of a LINE entity
;;;  Args: ent - entity name
;;;  Returns: point or nil
;;;---------------------------------------------------------------
(defun line-get-endpoint (ent / elist)
  (setq elist (entget ent))
  (cdr (assoc 11 elist))
)

;;;---------------------------------------------------------------
;;;  line-get-length
;;;  Get the length of a LINE entity
;;;  Args: ent - entity name
;;;  Returns: float length, or nil if not a line
;;;---------------------------------------------------------------
(defun line-get-length (ent / pts)
  (setq pts (line-get-endpoints ent))
  (if pts
    (distance (car pts) (cadr pts))
    nil
  )
)

;;;---------------------------------------------------------------
;;;  line-get-length-vl
;;;  Get length using vlax-curve (works for any curve entity)
;;;  Args: ent - entity name
;;;  Returns: float length
;;;---------------------------------------------------------------
(defun line-get-length-vl (ent)
  (vl-load-com)
  (- (vlax-curve-getDistAtParam ent (vlax-curve-getEndParam ent))
     (vlax-curve-getDistAtParam ent (vlax-curve-getStartParam ent)))
)

;;;---------------------------------------------------------------
;;;  line-get-midpoint
;;;  Get midpoint of a LINE entity
;;;  Args: ent - entity name
;;;  Returns: point or nil
;;;---------------------------------------------------------------
(defun line-get-midpoint (ent / pts)
  (setq pts (line-get-endpoints ent))
  (if pts
    (list (/ (+ (car (car pts)) (car (cadr pts))) 2.0)
          (/ (+ (cadr (car pts)) (cadr (cadr pts))) 2.0)
          0.0)
    nil
  )
)

;;;---------------------------------------------------------------
;;;  line-get-closest-point
;;;  Get the closest point on a line entity to a given point
;;;  Args: ent - entity name
;;;         pt  - point list
;;;  Returns: point on line, or nil
;;;---------------------------------------------------------------
(defun line-get-closest-point (ent pt)
  (vl-load-com)
  (vlax-curve-getClosestPointTo ent pt)
)

;;;---------------------------------------------------------------
;;;  line-get-closest-point-with-dist
;;;  Get closest point and distance from a line to a given point
;;;  Args: ent - entity name
;;;         pt  - point list
;;;  Returns: (closest-point distance) or nil
;;;---------------------------------------------------------------
(defun line-get-closest-point-with-dist (ent pt / cpt d)
  (setq cpt (line-get-closest-point ent pt))
  (if cpt
    (list cpt (distance pt cpt))
    nil
  )
)

;;;---------------------------------------------------------------
;;;  line-point-at-distance
;;;  Get point at specified distance from start of line
;;;  Args: ent      - entity name
;;;         dist     - distance from start
;;;  Returns: point or nil
;;;---------------------------------------------------------------
(defun line-point-at-distance (ent dist / param)
  (vl-load-com)
  (setq param (vlax-curve-getParamAtDist ent dist))
  (if param
    (vlax-curve-getPointAtParam ent param)
    nil
  )
)

;;;---------------------------------------------------------------
;;;  line-distance-at-point
;;;  Get distance from line start to a point on the line
;;;  Args: ent - entity name
;;;         pt  - point on or near the line
;;;  Returns: float distance or nil
;;;---------------------------------------------------------------
(defun line-distance-at-point (ent pt / cpt)
  (setq cpt (line-get-closest-point ent pt))
  (if cpt
    (vlax-curve-getDistAtPoint ent cpt)
    nil
  )
)

;;;---------------------------------------------------------------
;;;  lines-get-intersection
;;;  Get intersection point(s) of two line entities
;;;  Args: ent1, ent2 - entity names
;;;  Returns: list of intersection points, or nil
;;;---------------------------------------------------------------
(defun lines-get-intersection (ent1 ent2 / obj1 obj2 int-arr n pts k pt)
  (vl-load-com)
  (setq obj1 (vlax-ename->vla-object ent1))
  (setq obj2 (vlax-ename->vla-object ent2))
  (if (and obj1 obj2)
    (progn
      (setq int-arr (vl-catch-all-apply
                      'vlax-safearray->list
                      (list (vlax-variant-value
                              (vla-intersectwith obj1 obj2 acExtendNone)))))
      (if (and int-arr (not (vl-catch-all-error-p int-arr)))
        (progn
          (setq n (/ (length int-arr) 3))
          (setq pts nil k 0)
          (repeat n
            (setq pt (list (nth (* k 3) int-arr)
                           (nth (1+ (* k 3)) int-arr)
                           0.0))
            (setq pts (cons pt pts))
            (setq k (1+ k))
          )
          (reverse pts)
        )
        nil
      )
    )
    nil
  )
)

;;;---------------------------------------------------------------
;;;  line-is-horizontal-p
;;;  Check if a LINE entity is horizontal (within tolerance)
;;;  Args: ent       - entity name
;;;         tolerance - angle tolerance in radians (default 0.001)
;;;  Returns: T or nil
;;;---------------------------------------------------------------
(defun line-is-horizontal-p (ent tolerance / pts dx dy angle)
  (if (null tolerance) (setq tolerance 0.001))
  (setq pts (line-get-endpoints ent))
  (if pts
    (progn
      (setq dx (- (car (cadr pts)) (car (car pts))))
      (setq dy (- (cadr (cadr pts)) (cadr (car pts))))
      (setq angle (abs (atan dy dx)))
      (or (< angle tolerance) (< (abs (- angle pi)) tolerance))
    )
    nil
  )
)

;;;---------------------------------------------------------------
;;;  line-is-vertical-p
;;;  Check if a LINE entity is vertical (within tolerance)
;;;  Args: ent       - entity name
;;;         tolerance - angle tolerance in radians (default 0.001)
;;;  Returns: T or nil
;;;---------------------------------------------------------------
(defun line-is-vertical-p (ent tolerance / pts dx dy angle)
  (if (null tolerance) (setq tolerance 0.001))
  (setq pts (line-get-endpoints ent))
  (if pts
    (progn
      (setq dx (- (car (cadr pts)) (car (car pts))))
      (setq dy (- (cadr (cadr pts)) (cadr (car pts))))
      (setq angle (abs (atan dx dy)))
      (or (< angle tolerance) (< (abs (- angle pi)) tolerance))
    )
    nil
  )
)

;;;---------------------------------------------------------------
;;;  line-get-angle
;;;  Get the angle (radians) of a LINE entity
;;;  Args: ent - entity name
;;;  Returns: float angle in radians
;;;---------------------------------------------------------------
(defun line-get-angle (ent / pts)
  (setq pts (line-get-endpoints ent))
  (if pts
    (angle (car pts) (cadr pts))
    0.0
  )
)

;;;---------------------------------------------------------------
;;;  line-get-slope-intercept
;;;  Get slope and Y-intercept of a LINE entity
;;;  Args: ent - entity name
;;;  Returns: (slope intercept) where slope=nil for vertical lines
;;;---------------------------------------------------------------
(defun line-get-slope-intercept (ent / pts x1 y1 x2 y2 dx dy)
  (setq pts (line-get-endpoints ent))
  (if pts
    (progn
      (setq x1 (car (car pts)) y1 (cadr (car pts)))
      (setq x2 (car (cadr pts)) y2 (cadr (cadr pts)))
      (setq dx (- x2 x1))
      (if (< (abs dx) 1e-10)
        ;; vertical line: slope = nil, intercept = x value
        (list nil x1)
        ;; normal line
        (progn
          (setq k (/ (- y2 y1) dx))
          (list k (- y1 (* k x1)))
        )
      )
    )
    nil
  )
)

;;;---------------------------------------------------------------
;;;  line-point-on-segment-p
;;;  Check if a point lies on a line segment (within tolerance)
;;;  Args: pt        - point to test
;;;         line-start - line start point
;;;         line-end   - line end point
;;;         tolerance  - distance tolerance (default 1.0)
;;;  Returns: T or nil
;;;---------------------------------------------------------------
(defun line-point-on-segment-p (pt line-start line-end tolerance / cp)
  (if (null tolerance) (setq tolerance 1.0))
  (setq cp (line-get-closest-point-to-segment pt line-start line-end))
  (if (< (distance pt cp) tolerance) T nil)
)

;;;---------------------------------------------------------------
;;;  line-get-closest-point-to-segment
;;;  Get closest point on a line segment (not infinite line)
;;;  Args: pt   - test point
;;;         p1   - segment start
;;;         p2   - segment end
;;;  Returns: closest point on segment
;;;---------------------------------------------------------------
(defun line-get-closest-point-to-segment (pt p1 p2 / dx dy len2 t-param proj)
  (setq dx (- (car p2) (car p1)))
  (setq dy (- (cadr p2) (cadr p1)))
  (setq len2 (+ (* dx dx) (* dy dy)))
  (if (< len2 1e-20)
    p1  ; degenerate segment
    (progn
      (setq t-param (/ (+ (* (- (car pt) (car p1)) dx)
                          (* (- (cadr pt) (cadr p1)) dy))
                       len2))
      (cond
        ((<= t-param 0.0) p1)
        ((>= t-param 1.0) p2)
        (T
         (list (+ (car p1) (* t-param dx))
               (+ (cadr p1) (* t-param dy))
               0.0)
        )
      )
    )
  )
)

;;;---------------------------------------------------------------
;;;  line-get-layer
;;;  Get the layer name of a LINE entity
;;;  Args: ent - entity name
;;;  Returns: string layer name
;;;---------------------------------------------------------------
(defun line-get-layer (ent / elist)
  (setq elist (entget ent))
  (cdr (assoc 8 elist))
)

;;;---------------------------------------------------------------
;;;  line-set-layer
;;;  Change the layer of a LINE entity
;;;  Args: ent  - entity name
;;;         name - layer name string
;;;  Returns: modified entity (from entmod)
;;;---------------------------------------------------------------
(defun line-set-layer (ent name / elist)
  (setq elist (entget ent))
  (entmod (subst (cons 8 name) (assoc 8 elist) elist))
)

;;;---------------------------------------------------------------
;;;  line-create
;;;  Create a LINE entity between two points
;;;  Args: p1, p2 - point lists
;;;  Returns: entity name of new line
;;;---------------------------------------------------------------
(defun line-create (p1 p2 / old-cmdecho ent)
  (setq old-cmdecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (command-s "_.line" "_non" (trans p1 0 1) "_non" (trans p2 0 1) "")
  (setq ent (entlast))
  (setvar "cmdecho" old-cmdecho)
  ent
)

;;;---------------------------------------------------------------
;;;  line-create-on-layer
;;;  Create a LINE on a specific layer
;;;  Args: p1, p2 - point lists
;;;         layer - layer name string
;;;  Returns: entity name
;;;---------------------------------------------------------------
(defun line-create-on-layer (p1 p2 layer / old-layer ent)
  (setq old-layer (getvar "clayer"))
  (setvar "clayer" layer)
  (setq ent (line-create p1 p2))
  (setvar "clayer" old-layer)
  ent
)

;;;---------------------------------------------------------------
;;;  line-ss->list
;;;  Convert a selection set of lines to a list of enames
;;;  Args: ss - selection set
;;;  Returns: list of entity names
;;;---------------------------------------------------------------
(defun line-ss->list (ss / i ent result)
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq result (cons ent result))
        (setq i (1+ i))
      )
      (reverse result)
    )
    nil
  )
)

;;;---------------------------------------------------------------
;;;  line-get-all-on-layer
;;;  Get all LINE entities on a specific layer
;;;  Args: layer-name - layer name string
;;;  Returns: selection set or nil
;;;---------------------------------------------------------------
(defun line-get-all-on-layer (layer-name)
  (ssget "x" (list (cons 0 "LINE") (cons 8 layer-name)))
)

;;;===============================================================
;;;  TEST FUNCTIONS
;;;===============================================================

(defun test-M02-line-utils (/ passed failed old-cmdecho
                              ent1 ent2 ent3 pts cp d mid slope si
                              int-pts h-p v-p ang)
  (setq passed 0 failed 0)
  (setq old-cmdecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)

  (princ "\n\n=== M02 Line Utils Tests ===")

  (command-s "_.undo" "_be")

  ;; create test lines
  (command-s "_.line" "0,0" "1000,0" "")       ; horizontal
  (setq ent1 (entlast))
  (command-s "_.line" "1000,0" "1000,1000" "")  ; vertical
  (setq ent2 (entlast))
  (command-s "_.line" "0,0" "1000,1000" "")     ; diagonal
  (setq ent3 (entlast))

  ;; Test 1: line-get-endpoints
  (princ "\n[Test 1] line-get-endpoints...")
  (setq pts (line-get-endpoints ent1))
  (if (and pts
           (< (abs (- (car (car pts)) 0.0)) 0.001)
           (< (abs (- (cadr (car pts)) 0.0)) 0.001)
           (< (abs (- (car (cadr pts)) 1000.0)) 0.001)
           (< (abs (- (cadr (cadr pts)) 0.0)) 0.001))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 2: line-get-length
  (princ "\n[Test 2] line-get-length...")
  (setq d (line-get-length ent1))
  (if (< (abs (- d 1000.0)) 0.01)
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 3: line-get-midpoint
  (princ "\n[Test 3] line-get-midpoint...")
  (setq mid (line-get-midpoint ent1))
  (if (and mid
           (< (abs (- (car mid) 500.0)) 0.001)
           (< (abs (- (cadr mid) 0.0)) 0.001))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 4: line-get-closest-point
  (princ "\n[Test 4] line-get-closest-point...")
  (setq cp (line-get-closest-point ent1 (list 500.0 200.0 0.0)))
  (if (and cp
           (< (abs (- (car cp) 500.0)) 0.001)
           (< (abs (- (cadr cp) 0.0)) 0.001))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 5: line-get-closest-point-with-dist
  (princ "\n[Test 5] line-get-closest-point-with-dist...")
  (setq cpd (line-get-closest-point-with-dist ent1 (list 500.0 200.0 0.0)))
  (if (and cpd
           (= (length cpd) 2)
           (< (abs (- (cadr cpd) 200.0)) 0.001))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 6: lines-get-intersection
  (princ "\n[Test 6] lines-get-intersection...")
  (setq int-pts (lines-get-intersection ent1 ent2))
  (if (and int-pts
           (= (length int-pts) 1)
           (< (abs (- (car (car int-pts)) 1000.0)) 0.001)
           (< (abs (- (cadr (car int-pts)) 0.0)) 0.001))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 7: line-is-horizontal-p
  (princ "\n[Test 7] line-is-horizontal-p...")
  (setq h-p (line-is-horizontal-p ent1))
  (if h-p
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 8: line-is-vertical-p
  (princ "\n[Test 8] line-is-vertical-p...")
  (setq v-p (line-is-vertical-p ent2))
  (if v-p
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 9: line-get-angle
  (princ "\n[Test 9] line-get-angle...")
  (setq ang (line-get-angle ent1))
  (if (< (abs ang) 0.001)
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 10: line-get-slope-intercept
  (princ "\n[Test 10] line-get-slope-intercept...")
  (setq si (line-get-slope-intercept ent1))
  (if (and si
           (< (abs (- (car si) 0.0)) 0.001)   ; slope = 0
           (< (abs (- (cadr si) 0.0)) 0.001))   ; intercept = 0
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 11: line-get-slope-intercept (vertical)
  (princ "\n[Test 11] line-get-slope-intercept (vertical)...")
  (setq si (line-get-slope-intercept ent2))
  (if (and si
           (null (car si))                       ; slope = nil
           (< (abs (- (cadr si) 1000.0)) 0.001)) ; intercept = x
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 12: line-create
  (princ "\n[Test 12] line-create...")
  (setq new-ent (line-create (list 0.0 500.0 0.0) (list 500.0 500.0 0.0)))
  (if (and new-ent
           (= (cdr (assoc 0 (entget new-ent))) "LINE")
           (< (abs (- (line-get-length new-ent) 500.0)) 0.01))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 13: line-point-on-segment-p
  (princ "\n[Test 13] line-point-on-segment-p...")
  (if (and (line-point-on-segment-p (list 500.0 0.0 0.0)
                                     (list 0.0 0.0 0.0)
                                     (list 1000.0 0.0 0.0)
                                     1.0)
           (null (line-point-on-segment-p (list 500.0 500.0 0.0)
                                          (list 0.0 0.0 0.0)
                                          (list 1000.0 0.0 0.0)
                                          1.0)))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 14: line-ss->list
  (princ "\n[Test 14] line-ss->list...")
  (setq ss (ssget "x" '((0 . "LINE") (8 . "0"))))
  (setq lst (line-ss->list ss))
  (if (and lst (= (length lst) 4))  ; 3 original + 1 created
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  (command-s "_.undo" "_e")

  ;; Summary
  (princ (strcat "\n\n=== M02 Test Summary: "
                 (itoa passed) " passed, "
                 (itoa failed) " failed ===\n"))

  (setvar "cmdecho" old-cmdecho)
  (list passed failed)
)

;;;---------------------------------------------------------------
;;;  Provide module info
;;;---------------------------------------------------------------
(princ "\n[M02] line_utils.lsp loaded.")
(princ (strcat "  Functions: line-get-endpoints, line-get-length, "
               "line-get-closest-point, lines-get-intersection, "
               "line-create, line-get-slope-intercept, ..."))
(princ "\n  Test: (test-M02-line-utils)")
(princ)
