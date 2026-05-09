;;;===============================================================
;;;  M05 - Break Lines Module (Optimized)
;;;  Break LINE entities at intersection points
;;;  Optimized: spatial index, batch processing, entmake
;;;  NOTE: Only handles LINE entities, not ARC/CIRCLE/SPLINE
;;;===============================================================
;;;  ENCODING: ANSI (ASCII only, no Chinese characters)
;;;  DEPENDENCIES: M00 (spatial_index.lsp), M02 (line_utils.lsp)
;;;===============================================================

;;;---------------------------------------------------------------
;;;  Global Parameters
;;;---------------------------------------------------------------
(setq *break-tolerance* 0.001)

;;;---------------------------------------------------------------
;;;  break-build-spatial-index
;;;  Build spatial hash index from line entities
;;;  Uses sp-build-index from M00 for index construction
;;;  Args: line-ss - selection set of LINE entities
;;;         cell-size - grid cell size
;;;  Returns: (index line-data) where
;;;    index = assoc list of (cell-key . (line-idx ...))
;;;    line-data = list of (ent pts box)
;;;---------------------------------------------------------------
(defun break-build-spatial-index (line-ss cell-size / line-data i ent pts box idx)
  (setq line-data nil)
  (setq i 0)
  (if line-ss
    (progn
      (repeat (sslength line-ss)
        (setq ent (ssname line-ss i))
        (if ent
          (progn
            (setq pts (line-get-endpoints ent))
            (if pts
              (setq line-data (cons (list ent pts (sp-box-from-pts pts)) line-data))
            )
          )
        )
        (setq i (1+ i))
      )
      (setq line-data (reverse line-data))
      (setq idx (sp-build-index line-data '(lambda (r) (caddr r)) cell-size))
      (list idx line-data)
    )
    (list nil nil)
  )
)

;;;---------------------------------------------------------------
;;;  break-create-segments-entmake
;;;  Create line segments from break points using entmake
;;;  Args: start-pt - original line start
;;;         end-pt   - original line end
;;;         pt-list  - sorted list of break points (excluding endpoints)
;;;         layer    - layer for new lines
;;;  Returns: list of created entity names
;;;---------------------------------------------------------------
(defun break-create-segments-entmake (start-pt end-pt pt-list layer / prev-pt new-ent result)
  (setq result nil)
  (setq prev-pt start-pt)
  (foreach pt pt-list
    (if (> (distance prev-pt pt) *break-tolerance*)
      (progn
        (setq new-ent
          (entmakex
            (list
              (cons 0 "LINE")
              (cons 8 layer)
              (cons 10 prev-pt)
              (cons 11 pt)
            )
          )
        )
        (if new-ent (setq result (cons new-ent result)))
        (setq prev-pt pt)
      )
    )
  )
  (if (> (distance prev-pt end-pt) *break-tolerance*)
    (progn
      (setq new-ent
        (entmakex
          (list
            (cons 0 "LINE")
            (cons 8 layer)
            (cons 10 prev-pt)
            (cons 11 end-pt)
          )
        )
      )
      (if new-ent (setq result (cons new-ent result)))
    )
  )
  (reverse result)
)

;;;---------------------------------------------------------------
;;;  break-line-at-points
;;;  Break a single line at specified points using entmake
;;;  Args: ent     - entity to break
;;;         pt-list - list of break points
;;;  Returns: list of new segment entity names or nil
;;;---------------------------------------------------------------
(defun break-line-at-points (ent pt-list / pts start-pt end-pt layer sorted-pts new-ents)
  (setq pts (line-get-endpoints ent))
  (if (and pts (> (length pt-list) 0))
    (progn
      (setq start-pt (car pts))
      (setq end-pt (cadr pts))
      (setq layer (line-get-layer ent))
      (setq sorted-pts (sp-sort-points-by-distance start-pt pt-list))
      (setq sorted-pts
        (vl-remove-if
          '(lambda (p)
             (or (< (distance p start-pt) *break-tolerance*)
                 (< (distance p end-pt) *break-tolerance*)))
          sorted-pts))
      (setq new-ents (break-create-segments-entmake start-pt end-pt sorted-pts layer))
      (entdel ent)
      new-ents
    )
    nil
  )
)

;;;---------------------------------------------------------------
;;;  break-collect-intersections-indexed
;;;  Collect all intersection points using spatial index
;;;  Only checks lines in nearby spatial cells
;;;  Args: line-idx - index of current line in line-data
;;;         line-data - list of (ent pts box)
;;;         idx - spatial index
;;;         cell-size - grid cell size
;;;  Returns: list of intersection points
;;;---------------------------------------------------------------
(defun break-collect-intersections-indexed (line-idx line-data idx cell-size /
                                              rec ent1 box nearby-indices pts i rec2 ent2 int-pts)
  (setq rec (nth line-idx line-data))
  (setq ent1 (car rec))
  (setq box (caddr rec))
  (setq nearby-indices (sp-get-candidates box idx cell-size))
  (setq pts nil)
  (foreach ni nearby-indices
    (if (/= ni line-idx)
      (progn
        (setq rec2 (nth ni line-data))
        (setq ent2 (car rec2))
        (if (and ent2 (sp-boxes-overlap-p box (caddr rec2) nil))
          (progn
            (setq int-pts (lines-get-intersection ent1 ent2))
            (if int-pts
              (foreach pt int-pts
                (setq pts (cons pt pts))
              )
            )
          )
        )
      )
    )
  )
  (sp-remove-duplicate-points pts *break-tolerance*)
)

;;;---------------------------------------------------------------
;;;  break-collect-intersections
;;;  Collect all intersection points for a line with other lines
;;;  Args: target-ent - the line to check
;;;         line-ss    - selection set of lines to intersect with
;;;  Returns: list of intersection points
;;;---------------------------------------------------------------
(defun break-collect-intersections (target-ent line-ss / pts i ent int-pts)
  (setq pts nil)
  (if (and target-ent line-ss)
    (progn
      (setq i 0)
      (repeat (sslength line-ss)
        (setq ent (ssname line-ss i))
        (if (and ent (/= ent target-ent))
          (progn
            (setq int-pts (lines-get-intersection target-ent ent))
            (if int-pts
              (foreach pt int-pts
                (setq pts (cons pt pts))
              )
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )
  (sp-remove-duplicate-points pts *break-tolerance*)
)

;;;---------------------------------------------------------------
;;;  break-line-at-intersections
;;;  Break a single line at all intersection points with other lines
;;;  Args: ent     - entity to break
;;;         line-ss - selection set of lines to intersect with
;;;  Returns: list of new segment entity names or nil
;;;---------------------------------------------------------------
(defun break-line-at-intersections (ent line-ss / int-pts)
  (setq int-pts (break-collect-intersections ent line-ss))
  (if (> (length int-pts) 0)
    (break-line-at-points ent int-pts)
    nil
  )
)

;;;---------------------------------------------------------------
;;;  break-lines-in-set
;;;  Break all lines in a selection set at their intersections
;;;  Uses spatial index for O(n*k) instead of O(n^2)
;;;  Args: line-ss - selection set of lines
;;;  Returns: selection set of all resulting lines
;;;---------------------------------------------------------------
(defun break-lines-in-set (line-ss / spatial-result idx line-data i n
                                      int-pts new-ents all-ents ent)
  (setq all-ents (ssadd))
  (if line-ss
    (progn
      (setq n (sslength line-ss))
      (princ (strcat "\n[break] Breaking " (itoa n) " lines..."))

      (if (> n 50)
        (progn
          (setq spatial-result (break-build-spatial-index line-ss (sp-get-cell-size)))
          (setq idx (car spatial-result))
          (setq line-data (cadr spatial-result))
          (setq i 0)
          (repeat n
            (setq int-pts (break-collect-intersections-indexed i line-data idx (sp-get-cell-size)))
            (setq ent (car (nth i line-data)))
            (if (and ent (entget ent))
              (progn
                (if (> (length int-pts) 0)
                  (progn
                    (setq new-ents (break-line-at-points ent int-pts))
                    (if new-ents
                      (foreach ne new-ents
                        (setq all-ents (ssadd ne all-ents))
                      )
                    )
                  )
                  (setq all-ents (ssadd ent all-ents))
                )
              )
            )
            (setq i (1+ i))
          )
        )
        (progn
          (setq i 0)
          (repeat n
            (setq ent (ssname line-ss i))
            (if ent
              (progn
                (setq int-pts (break-collect-intersections ent line-ss))
                (if (> (length int-pts) 0)
                  (progn
                    (setq new-ents (break-line-at-points ent int-pts))
                    (if new-ents
                      (foreach ne new-ents
                        (setq all-ents (ssadd ne all-ents))
                      )
                    )
                  )
                  (setq all-ents (ssadd ent all-ents))
                )
              )
            )
            (setq i (1+ i))
          )
        )
      )
      (princ (strcat "\n[break] Result: " (itoa (sslength all-ents)) " segments"))
    )
  )
  all-ents
)

;;;---------------------------------------------------------------
;;;  break-lines-all
;;;  Main function: Break all lines on specified layer(s)
;;;  Args: layer-list - list of layer names (nil = all layers)
;;;  Returns: selection set of all resulting lines
;;;---------------------------------------------------------------
(defun break-lines-all (layer-list / filter-list line-ss)
  (princ "\n[break] Breaking lines at intersections...")

  (setq filter-list (list (cons 0 "LINE")))
  (if layer-list
    (if (stringp layer-list)
      (setq filter-list (cons (cons 8 layer-list) filter-list))
      (setq filter-list (cons (cons 8 (apply 'strcat (mapcar '(lambda (x) (strcat x ",")) layer-list))) filter-list))
    )
  )

  (setq line-ss (ssget "x" filter-list))

  (if (null line-ss)
    (progn
      (princ "\n[break] No lines found.")
      nil
    )
    (break-lines-in-set line-ss)
  )
)

;;;---------------------------------------------------------------
;;;  break-set-tolerance
;;;  Set the break tolerance
;;;---------------------------------------------------------------
(defun break-set-tolerance (val)
  (setq *break-tolerance* val)
)

;;;---------------------------------------------------------------
;;;  break-get-tolerance
;;;  Get the current break tolerance
;;;---------------------------------------------------------------
(defun break-get-tolerance ()
  *break-tolerance*
)

;;;===============================================================
;;;  TEST FUNCTIONS
;;;===============================================================

(defun test-M05-break-lines (/ passed failed old-cmdecho
                               ent1 ent2 ent3 ent4 ss result)
  (setq passed 0 failed 0)
  (setq old-cmdecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)

  (princ "\n\n=== M05 Break Lines Tests ===")

  (command-s "_.undo" "_be")

  (princ "\n[Test 1] break-collect-intersections...")
  (command-s "_.line" "0,0" "1000,0" "")
  (setq ent1 (entlast))
  (command-s "_.line" "500,-500" "500,500" "")
  (setq ent2 (entlast))
  (setq ss (ssadd))
  (setq ss (ssadd ent1 ss))
  (setq ss (ssadd ent2 ss))
  (setq pts (break-collect-intersections ent1 ss))
  (if (and pts (= (length pts) 1)
           (< (abs (- (car (car pts)) 500.0)) 0.001)
           (< (abs (- (cadr (car pts)) 0.0)) 0.001))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  (command-s "_.erase" ent1 "")
  (command-s "_.erase" ent2 "")

  (princ "\n[Test 2] break-sort-points-by-distance...")
  (setq start (list 0.0 0.0 0.0))
  (setq pt-list (list (list 800.0 0.0 0.0)
                      (list 200.0 0.0 0.0)
                      (list 500.0 0.0 0.0)))
  (setq sorted (sp-sort-points-by-distance start pt-list))
  (if (and sorted
           (= (length sorted) 3)
           (< (abs (- (car (car sorted)) 200.0)) 0.001)
           (< (abs (- (car (cadr sorted)) 500.0)) 0.001)
           (< (abs (- (car (caddr sorted)) 800.0)) 0.001))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  (princ "\n[Test 3] break-remove-duplicate-points...")
  (setq pt-list (list (list 100.0 100.0 0.0)
                      (list 100.001 100.0 0.0)
                      (list 200.0 200.0 0.0)
                      (list 100.0 100.0 0.0)))
  (setq unique (sp-remove-duplicate-points pt-list 0.01))
  (if (= (length unique) 2)
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  (princ "\n[Test 4] break-line-at-points...")
  (command-s "_.line" "0,0" "1000,0" "")
  (setq ent1 (entlast))
  (setq break-pts (list (list 300.0 0.0 0.0)
                        (list 700.0 0.0 0.0)))
  (setq result (break-line-at-points ent1 break-pts))
  (if (and result (= (length result) 3))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  (if result
    (foreach ne result
      (if (entget ne) (command-s "_.erase" ne ""))
    )
  )

  (princ "\n[Test 5] break-line-at-intersections (T-junction)...")
  (command-s "_.line" "0,0" "1000,0" "")
  (setq ent1 (entlast))
  (command-s "_.line" "500,0" "500,500" "")
  (setq ent2 (entlast))
  (setq ss (ssadd))
  (setq ss (ssadd ent1 ss))
  (setq ss (ssadd ent2 ss))
  (setq result (break-line-at-intersections ent1 ss))
  (if (and result (= (length result) 2))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  (if result
    (foreach ne result
      (if (entget ne) (command-s "_.erase" ne ""))
    )
  )
  (command-s "_.erase" ent2 "")

  (princ "\n[Test 6] break-line-at-intersections (X-junction)...")
  (command-s "_.line" "0,0" "1000,1000" "")
  (setq ent1 (entlast))
  (command-s "_.line" "0,1000" "1000,0" "")
  (setq ent2 (entlast))
  (setq ss (ssadd))
  (setq ss (ssadd ent1 ss))
  (setq ss (ssadd ent2 ss))
  (setq result (break-line-at-intersections ent1 ss))
  (if (and result (= (length result) 2))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  (if result
    (foreach ne result
      (if (entget ne) (command-s "_.erase" ne ""))
    )
  )
  (command-s "_.erase" ent2 "")

  (princ "\n[Test 7] break-lines-in-set (grid pattern)...")
  (command-s "_.line" "0,0" "1000,0" "")
  (setq ent1 (entlast))
  (command-s "_.line" "0,500" "1000,500" "")
  (setq ent2 (entlast))
  (command-s "_.line" "250,-100" "250,600" "")
  (setq ent3 (entlast))
  (command-s "_.line" "750,-100" "750,600" "")
  (setq ent4 (entlast))
  (setq ss (ssadd))
  (setq ss (ssadd ent1 ss))
  (setq ss (ssadd ent2 ss))
  (setq ss (ssadd ent3 ss))
  (setq ss (ssadd ent4 ss))
  (setq result (break-lines-in-set ss))
  (if (and result (= (sslength result) 12))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn
      (princ (strcat " FAIL (got " (itoa (sslength result)) " segments, expected 12)"))
      (setq failed (1+ failed))
    )
  )
  (if result
    (progn
      (setq i 0)
      (repeat (sslength result)
        (command-s "_.erase" (ssname result i) "")
        (setq i (1+ i))
      )
    )
  )

  (princ "\n[Test 8] break-lines-all (integration)...")
  (command-s "_.line" "0,0" "1000,0" "")
  (setq ent1 (entlast))
  (command-s "_.line" "500,-500" "500,500" "")
  (setq ent2 (entlast))
  (setq result (break-lines-all "0"))
  (if (and result (> (sslength result) 0))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  (if result
    (progn
      (setq i 0)
      (repeat (sslength result)
        (command-s "_.erase" (ssname result i) "")
        (setq i (1+ i))
      )
    )
  )

  (princ "\n[Test 9] tolerance get/set...")
  (setq old-tol (break-get-tolerance))
  (break-set-tolerance 0.01)
  (if (= (break-get-tolerance) 0.01)
    (progn
      (princ " PASS")
      (setq passed (1+ passed))
      (break-set-tolerance old-tol)
    )
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  (princ "\n[Test 10] spatial hash cell size get/set...")
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

  (princ "\n[Test 11] break-boxes-overlap-p...")
  (if (and (sp-boxes-overlap-p (list 0.0 0.0 1000.0 1000.0)
                                (list 500.0 500.0 1500.0 1500.0) nil)
           (null (sp-boxes-overlap-p (list 0.0 0.0 100.0 100.0)
                                     (list 200.0 200.0 300.0 300.0) nil)))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  (command-s "_.undo" "_e")

  (princ (strcat "\n\n=== M05 Test Summary: "
                 (itoa passed) " passed, "
                 (itoa failed) " failed ===\n"))

  (setvar "cmdecho" old-cmdecho)
  (list passed failed)
)

;;;---------------------------------------------------------------
;;;  Provide module info
;;;---------------------------------------------------------------
(princ "\n[M05] break_lines.lsp loaded.")
(princ (strcat "  Functions: break-lines-in-set, break-lines-all, "
               "break-line-at-intersections, break-line-at-points"))
(princ (strcat "\n  Default tolerance: " (rtos *break-tolerance* 2 4)))
(princ (strcat "\n  Spatial cell size: " (rtos *sp-default-cell-size* 2 0)))
(princ "\n  Dependencies: M00 (spatial_index.lsp), M02 (line_utils.lsp)")
(princ "\n  Test: (test-M05-break-lines)")
(princ)
