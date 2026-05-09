;;;===============================================================
;;;  M00 - Spatial Index and Geometry Utilities
;;;  Shared spatial hashing, bounding box, and point utilities
;;;  Used by: M04 (duplicate_remover), M05 (break_lines)
;;;===============================================================
;;;  ENCODING: ANSI (ASCII only, no Chinese characters)
;;;  DEPENDENCIES: M02 (line_utils.lsp) - for sp-box-from-line
;;;===============================================================

;;;---------------------------------------------------------------
;;;  sp-spatial-hash
;;;  Compute spatial hash cell keys for a bounding box
;;;  Args: box - (min-x min-y max-x max-y)
;;;         cell-size - spatial grid cell size
;;;  Returns: list of string cell keys like "cx_cy"
;;;---------------------------------------------------------------
(defun sp-spatial-hash (box cell-size / min-cx min-cy max-cx max-cy cx cy cells)
  (setq min-cx (fix (/ (nth 0 box) cell-size)))
  (setq min-cy (fix (/ (nth 1 box) cell-size)))
  (setq max-cx (fix (/ (nth 2 box) cell-size)))
  (setq max-cy (fix (/ (nth 3 box) cell-size)))
  (setq cells nil)
  (setq cx min-cx)
  (while (<= cx max-cx)
    (setq cy min-cy)
    (while (<= cy max-cy)
      (setq cells (cons (strcat (itoa cx) "_" (itoa cy)) cells))
      (setq cy (1+ cy))
    )
    (setq cx (1+ cx))
  )
  cells
)

;;;---------------------------------------------------------------
;;;  sp-build-index
;;;  Build a spatial hash index from a list of records
;;;  Each record must have a bounding box at a specified position
;;;  Args: records - list of records (any structure)
;;;         box-fn  - function that extracts box from a record
;;;         cell-size - spatial grid cell size
;;;  Returns: assoc list of (cell-key . (record-index ...))
;;;---------------------------------------------------------------
(defun sp-build-index (records box-fn cell-size / idx rec box cells cell-key i)
  (setq idx nil)
  (setq i 0)
  (foreach rec records
    (setq box (apply box-fn (list rec)))
    (if box
      (progn
        (setq cells (sp-spatial-hash box cell-size))
        (foreach ck cells
          (setq cell-key (assoc ck idx))
          (if cell-key
            (setq idx (subst (cons ck (cons i (cdr cell-key))) cell-key idx))
            (setq idx (cons (list ck i) idx))
          )
        )
      )
    )
    (setq i (1+ i))
  )
  idx
)

;;;---------------------------------------------------------------
;;;  sp-get-candidates
;;;  Get candidate record indices from spatial index for a given box
;;;  Args: box - bounding box
;;;         idx - spatial index from sp-build-index
;;;         cell-size - grid cell size
;;;  Returns: sorted list of unique candidate indices
;;;---------------------------------------------------------------
(defun sp-get-candidates (box idx cell-size / cells candidates cell-records)
  (setq cells (sp-spatial-hash box cell-size))
  (setq candidates nil)
  (foreach ck cells
    (setq cell-records (assoc ck idx))
    (if cell-records
      (foreach ri (cdr cell-records)
        (if (not (member ri candidates))
          (setq candidates (cons ri candidates))
        )
      )
    )
  )
  (vl-sort-i candidates '<)
)

;;;---------------------------------------------------------------
;;;  sp-box-from-pts
;;;  Get bounding box from two endpoints
;;;  Args: pts - (pt1 pt2)
;;;  Returns: (min-x min-y max-x max-y) or nil
;;;---------------------------------------------------------------
(defun sp-box-from-pts (pts / x1 y1 x2 y2)
  (if (and pts (= (length pts) 2))
    (progn
      (setq x1 (car (car pts)) y1 (cadr (car pts)))
      (setq x2 (car (cadr pts)) y2 (cadr (cadr pts)))
      (list (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2))
    )
    nil
  )
)

;;;---------------------------------------------------------------
;;;  sp-box-from-line
;;;  Get bounding box from a LINE entity
;;;  Args: ent - entity name
;;;  Returns: (min-x min-y max-x max-y) or nil
;;;---------------------------------------------------------------
(defun sp-box-from-line (ent / pts)
  (setq pts (line-get-endpoints ent))
  (sp-box-from-pts pts)
)

;;;---------------------------------------------------------------
;;;  sp-boxes-overlap-p
;;;  Check if two bounding boxes overlap (with optional tolerance)
;;;  Args: box1, box2 - bounding boxes
;;;         tol - tolerance (nil = 0.0)
;;;  Returns: T or nil
;;;---------------------------------------------------------------
(defun sp-boxes-overlap-p (box1 box2 tol / t)
  (if (null tol) (setq tol 0.0))
  (if (and box1 box2)
    (not (or (< (nth 2 box1) (- (nth 0 box2) tol))
             (< (nth 2 box2) (- (nth 0 box1) tol))
             (< (nth 3 box1) (- (nth 1 box2) tol))
             (< (nth 3 box2) (- (nth 1 box1) tol))))
    nil
  )
)

;;;---------------------------------------------------------------
;;;  sp-point-in-list-p
;;;  Check if a point is already in a list (within tolerance)
;;;  Args: pt      - point to check
;;;         pt-list - list of points
;;;         tol     - distance tolerance
;;;  Returns: T or nil
;;;---------------------------------------------------------------
(defun sp-point-in-list-p (pt pt-list tol / found)
  (setq found nil)
  (foreach p pt-list
    (if (< (distance pt p) tol)
      (setq found T)
    )
  )
  found
)

;;;---------------------------------------------------------------
;;;  sp-remove-duplicate-points
;;;  Remove duplicate points from a list
;;;  Args: pt-list - list of points
;;;         tol     - distance tolerance
;;;  Returns: list with duplicates removed
;;;---------------------------------------------------------------
(defun sp-remove-duplicate-points (pt-list tol / result)
  (setq result nil)
  (foreach pt pt-list
    (if (not (sp-point-in-list-p pt result tol))
      (setq result (cons pt result))
    )
  )
  (reverse result)
)

;;;---------------------------------------------------------------
;;;  sp-sort-points-by-distance
;;;  Sort points by distance from a reference point
;;;  Args: ref-pt  - reference point
;;;         pt-list - list of points to sort
;;;  Returns: sorted list of points
;;;---------------------------------------------------------------
(defun sp-sort-points-by-distance (ref-pt pt-list / dist-list)
  (setq dist-list nil)
  (foreach pt pt-list
    (setq dist-list (cons (cons (distance ref-pt pt) pt) dist-list))
  )
  (setq dist-list (vl-sort dist-list '(lambda (a b) (< (car a) (car b)))))
  (mapcar 'cdr dist-list)
)

;;;---------------------------------------------------------------
;;;  sp-default-cell-size
;;;  Get/Set the default spatial hash cell size
;;;---------------------------------------------------------------
(setq *sp-default-cell-size* 5000.0)

(defun sp-set-cell-size (val)
  (setq *sp-default-cell-size* val)
)

(defun sp-get-cell-size ()
  *sp-default-cell-size*
)

;;;===============================================================
;;;  TEST FUNCTIONS
;;;===============================================================

(defun test-M00-spatial-index (/ passed failed box cells idx records
                                 candidates box1 box2 pts sorted-pts)
  (setq passed 0 failed 0)

  (princ "\n\n=== M00 Spatial Index Tests ===")

  (princ "\n[Test 1] sp-spatial-hash...")
  (setq box (list 0.0 0.0 5000.0 5000.0))
  (setq cells (sp-spatial-hash box 5000.0))
  (if (and cells (= (length cells) 1)
           (stringp (car cells)))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  (princ "\n[Test 2] sp-spatial-hash (multi-cell)...")
  (setq box (list 0.0 0.0 10000.0 5000.0))
  (setq cells (sp-spatial-hash box 5000.0))
  (if (and cells (= (length cells) 2))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  (princ "\n[Test 3] sp-box-from-pts...")
  (setq pts (list (list 100.0 200.0 0.0) (list 300.0 50.0 0.0)))
  (setq box (sp-box-from-pts pts))
  (if (and box
           (< (abs (- (nth 0 box) 100.0)) 0.001)
           (< (abs (- (nth 1 box) 50.0)) 0.001)
           (< (abs (- (nth 2 box) 300.0)) 0.001)
           (< (abs (- (nth 3 box) 200.0)) 0.001))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  (princ "\n[Test 4] sp-boxes-overlap-p (overlapping)...")
  (setq box1 (list 0.0 0.0 1000.0 1000.0))
  (setq box2 (list 500.0 500.0 1500.0 1500.0))
  (if (sp-boxes-overlap-p box1 box2 nil)
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  (princ "\n[Test 5] sp-boxes-overlap-p (non-overlapping)...")
  (setq box1 (list 0.0 0.0 100.0 100.0))
  (setq box2 (list 200.0 200.0 300.0 300.0))
  (if (null (sp-boxes-overlap-p box1 box2 nil))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  (princ "\n[Test 6] sp-boxes-overlap-p (with tolerance)...")
  (setq box1 (list 0.0 0.0 100.0 100.0))
  (setq box2 (list 101.0 101.0 200.0 200.0))
  (if (and (null (sp-boxes-overlap-p box1 box2 nil))
           (sp-boxes-overlap-p box1 box2 2.0))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  (princ "\n[Test 7] sp-point-in-list-p...")
  (setq pt-list (list (list 100.0 100.0 0.0) (list 200.0 200.0 0.0)))
  (if (and (sp-point-in-list-p (list 100.0 100.0 0.0) pt-list 0.01)
           (sp-point-in-list-p (list 100.001 100.0 0.0) pt-list 0.01)
           (null (sp-point-in-list-p (list 300.0 300.0 0.0) pt-list 0.01)))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  (princ "\n[Test 8] sp-remove-duplicate-points...")
  (setq pt-list (list (list 100.0 100.0 0.0)
                      (list 100.001 100.0 0.0)
                      (list 200.0 200.0 0.0)
                      (list 100.0 100.0 0.0)))
  (setq unique (sp-remove-duplicate-points pt-list 0.01))
  (if (= (length unique) 2)
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  (princ "\n[Test 9] sp-sort-points-by-distance...")
  (setq ref (list 0.0 0.0 0.0))
  (setq pt-list (list (list 800.0 0.0 0.0)
                      (list 200.0 0.0 0.0)
                      (list 500.0 0.0 0.0)))
  (setq sorted (sp-sort-points-by-distance ref pt-list))
  (if (and sorted
           (= (length sorted) 3)
           (< (abs (- (car (car sorted)) 200.0)) 0.001)
           (< (abs (- (car (cadr sorted)) 500.0)) 0.001)
           (< (abs (- (car (caddr sorted)) 800.0)) 0.001))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  (princ "\n[Test 10] sp-build-index and sp-get-candidates...")
  (setq records (list (list nil nil (list 0.0 0.0 1000.0 1000.0))
                      (list nil nil (list 5000.0 5000.0 6000.0 6000.0))
                      (list nil nil (list 500.0 500.0 1500.0 1500.0))))
  (setq idx (sp-build-index records '(lambda (r) (caddr r)) 5000.0))
  (setq candidates (sp-get-candidates (list 0.0 0.0 1000.0 1000.0) idx 5000.0))
  (if (and idx (> (length candidates) 0))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  (princ "\n[Test 11] cell size get/set...")
  (setq old-cs (sp-get-cell-size))
  (sp-set-cell-size 10000.0)
  (if (= (sp-get-cell-size) 10000.0)
    (progn
      (princ " PASS")
      (setq passed (1+ passed))
      (sp-set-cell-size old-cs)
    )
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  (princ (strcat "\n\n=== M00 Test Summary: "
                 (itoa passed) " passed, "
                 (itoa failed) " failed ===\n"))

  (list passed failed)
)

;;;---------------------------------------------------------------
(princ "\n[M00] spatial_index.lsp loaded.")
(princ "  Functions: sp-spatial-hash, sp-build-index, sp-get-candidates,")
(princ "  sp-box-from-pts, sp-box-from-line, sp-boxes-overlap-p,")
(princ "  sp-point-in-list-p, sp-remove-duplicate-points, sp-sort-points-by-distance")
(princ "\n  Test: (test-M00-spatial-index)")
(princ)
