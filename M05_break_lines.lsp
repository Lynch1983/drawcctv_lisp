;;;===============================================================
;;;  M05 - Break Lines Module (Simplified)
;;;  Break LINE entities at intersection points
;;;  NOTE: Only handles LINE entities, not ARC/CIRCLE/SPLINE
;;;===============================================================
;;;  ENCODING: ANSI (ASCII only, no Chinese characters)
;;;  DEPENDENCIES: M02 (line_utils.lsp)
;;;===============================================================

;;;---------------------------------------------------------------
;;;  Global Parameters
;;;---------------------------------------------------------------
;;;  *break-tolerance* - Distance tolerance for point comparison
(setq *break-tolerance* 0.001)

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
  ;; Remove duplicate points
  (break-remove-duplicate-points pts *break-tolerance*)
)

;;;---------------------------------------------------------------
;;;  break-remove-duplicate-points
;;;  Remove duplicate points from a list
;;;  Args: pt-list - list of points
;;;         tol     - distance tolerance
;;;  Returns: list with duplicates removed
;;;---------------------------------------------------------------
(defun break-remove-duplicate-points (pt-list tol / result)
  (setq result nil)
  (foreach pt pt-list
    (if (not (break-point-in-list-p pt result tol))
      (setq result (cons pt result))
    )
  )
  (reverse result)
)

;;;---------------------------------------------------------------
;;;  break-point-in-list-p
;;;  Check if a point is already in a list (within tolerance)
;;;  Args: pt      - point to check
;;;         pt-list - list of points
;;;         tol     - distance tolerance
;;;  Returns: T or nil
;;;---------------------------------------------------------------
(defun break-point-in-list-p (pt pt-list tol / found)
  (setq found nil)
  (foreach p pt-list
    (if (< (distance pt p) tol)
      (setq found T)
    )
  )
  found
)

;;;---------------------------------------------------------------
;;;  break-sort-points-by-distance
;;;  Sort intersection points by distance from line start
;;;  Args: start-pt - line start point
;;;         pt-list  - list of points to sort
;;;  Returns: sorted list of points
;;;---------------------------------------------------------------
(defun break-sort-points-by-distance (start-pt pt-list / dist-list)
  (setq dist-list nil)
  (foreach pt pt-list
    (setq dist-list (cons (cons (distance start-pt pt) pt) dist-list))
  )
  ;; Sort by distance
  (setq dist-list (vl-sort dist-list '(lambda (a b) (< (car a) (car b)))))
  ;; Extract points
  (mapcar 'cdr dist-list)
)

;;;---------------------------------------------------------------
;;;  break-create-segments
;;;  Create line segments from break points
;;;  Args: start-pt - original line start
;;;         end-pt   - original line end
;;;         pt-list  - sorted list of break points (excluding endpoints)
;;;         layer    - layer for new lines
;;;  Returns: selection set of created segments
;;;---------------------------------------------------------------
(defun break-create-segments (start-pt end-pt pt-list layer / ss prev-pt new-ent)
  (setq ss (ssadd))
  (setq prev-pt start-pt)
  (foreach pt pt-list
    (if (> (distance prev-pt pt) *break-tolerance*)
      (progn
        (setq new-ent (line-create-on-layer prev-pt pt layer))
        (if new-ent (setq ss (ssadd new-ent ss)))
        (setq prev-pt pt)
      )
    )
  )
  ;; Create final segment to end point
  (if (> (distance prev-pt end-pt) *break-tolerance*)
    (progn
      (setq new-ent (line-create-on-layer prev-pt end-pt layer))
      (if new-ent (setq ss (ssadd new-ent ss)))
    )
  )
  ss
)

;;;---------------------------------------------------------------
;;;  break-line-at-points
;;;  Break a single line at specified points
;;;  Args: ent     - entity to break
;;;         pt-list - list of break points
;;;  Returns: selection set of new segments or nil
;;;---------------------------------------------------------------
(defun break-line-at-points (ent pt-list / pts start-pt end-pt layer sorted-pts ss)
  (setq pts (line-get-endpoints ent))
  (if (and pts (> (length pt-list) 0))
    (progn
      (setq start-pt (car pts))
      (setq end-pt (cadr pts))
      (setq layer (line-get-layer ent))
      ;; Sort points by distance from start
      (setq sorted-pts (break-sort-points-by-distance start-pt pt-list))
      ;; Filter out endpoints
      (setq sorted-pts
        (vl-remove-if
          '(lambda (p)
             (or (< (distance p start-pt) *break-tolerance*)
                 (< (distance p end-pt) *break-tolerance*)))
          sorted-pts))
      ;; Create segments
      (setq ss (break-create-segments start-pt end-pt sorted-pts layer))
      ;; Delete original
      (entdel ent)
      ss
    )
    nil
  )
)

;;;---------------------------------------------------------------
;;;  break-line-at-intersections
;;;  Break a single line at all intersection points with other lines
;;;  Args: ent     - entity to break
;;;         line-ss - selection set of lines to intersect with
;;;  Returns: selection set of new segments or nil
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
;;;  Args: line-ss - selection set of lines
;;;  Returns: selection set of all resulting lines
;;;---------------------------------------------------------------
(defun break-lines-in-set (line-ss / i ent all-lines int-pts new-segments)
  (setq all-lines (ssadd))
  (if line-ss
    (progn
      (princ (strcat "\n[break] Breaking " (itoa (sslength line-ss)) " lines..."))
      ;; First pass: collect all intersection points for each line
      (setq i 0)
      (repeat (sslength line-ss)
        (setq ent (ssname line-ss i))
        (if ent
          (progn
            (setq int-pts (break-collect-intersections ent line-ss))
            (if (> (length int-pts) 0)
              (progn
                (setq new-segments (break-line-at-points ent int-pts))
                (if new-segments
                  (progn
                    (setq j 0)
                    (repeat (sslength new-segments)
                      (setq all-lines (ssadd (ssname new-segments j) all-lines))
                      (setq j (1+ j))
                    )
                  )
                )
              )
              ;; No intersections, keep original
              (setq all-lines (ssadd ent all-lines))
            )
          )
        )
        (setq i (1+ i))
      )
      (princ (strcat "\n[break] Result: " (itoa (sslength all-lines)) " segments"))
    )
  )
  all-lines
)

;;;---------------------------------------------------------------
;;;  break-lines-all
;;;  Main function: Break all lines on specified layer(s)
;;;  Args: layer-list - list of layer names (nil = all layers)
;;;  Returns: selection set of all resulting lines
;;;---------------------------------------------------------------
(defun break-lines-all (layer-list / filter-list line-ss)
  (princ "\n[break] Breaking lines at intersections...")
  
  ;; Build filter
  (setq filter-list (list (cons 0 "LINE")))
  (if layer-list
    (if (stringp layer-list)
      (setq filter-list (cons (cons 8 layer-list) filter-list))
      (setq filter-list (cons (cons 8 (apply 'strcat (mapcar '(lambda (x) (strcat x ",")) layer-list))) filter-list))
    )
  )
  
  ;; Get lines
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

  ;; Test 1: break-collect-intersections
  (princ "\n[Test 1] break-collect-intersections...")
  (command-s "_.line" "0,0" "1000,0" "")    ; horizontal
  (setq ent1 (entlast))
  (command-s "_.line" "500,-500" "500,500" "") ; vertical, crosses at (500,0)
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

  ;; Test 2: break-sort-points-by-distance
  (princ "\n[Test 2] break-sort-points-by-distance...")
  (setq start (list 0.0 0.0 0.0))
  (setq pt-list (list (list 800.0 0.0 0.0)
                      (list 200.0 0.0 0.0)
                      (list 500.0 0.0 0.0)))
  (setq sorted (break-sort-points-by-distance start pt-list))
  (if (and sorted
           (= (length sorted) 3)
           (< (abs (- (car (car sorted)) 200.0)) 0.001)
           (< (abs (- (car (cadr sorted)) 500.0)) 0.001)
           (< (abs (- (car (caddr sorted)) 800.0)) 0.001))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 3: break-remove-duplicate-points
  (princ "\n[Test 3] break-remove-duplicate-points...")
  (setq pt-list (list (list 100.0 100.0 0.0)
                      (list 100.001 100.0 0.0)  ; near duplicate
                      (list 200.0 200.0 0.0)
                      (list 100.0 100.0 0.0)))   ; exact duplicate
  (setq unique (break-remove-duplicate-points pt-list 0.01))
  (if (= (length unique) 2)
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 4: break-line-at-points
  (princ "\n[Test 4] break-line-at-points...")
  (command-s "_.line" "0,0" "1000,0" "")
  (setq ent1 (entlast))
  (setq break-pts (list (list 300.0 0.0 0.0)
                        (list 700.0 0.0 0.0)))
  (setq result (break-line-at-points ent1 break-pts))
  (if (and result (= (sslength result) 3))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  ;; Cleanup
  (if result
    (progn
      (setq i 0)
      (repeat (sslength result)
        (command-s "_.erase" (ssname result i) "")
        (setq i (1+ i))
      )
    )
  )

  ;; Test 5: break-line-at-intersections (T-junction)
  (princ "\n[Test 5] break-line-at-intersections (T-junction)...")
  (command-s "_.line" "0,0" "1000,0" "")    ; main line
  (setq ent1 (entlast))
  (command-s "_.line" "500,0" "500,500" "") ; T-junction at (500,0)
  (setq ent2 (entlast))
  (setq ss (ssadd))
  (setq ss (ssadd ent1 ss))
  (setq ss (ssadd ent2 ss))
  (setq result (break-line-at-intersections ent1 ss))
  ;; Main line should be split into 2 segments
  (if (and result (= (sslength result) 2))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  ;; Cleanup
  (if result
    (progn
      (setq i 0)
      (repeat (sslength result)
        (command-s "_.erase" (ssname result i) "")
        (setq i (1+ i))
      )
    )
  )
  (command-s "_.erase" ent2 "")

  ;; Test 6: break-line-at-intersections (X-junction)
  (princ "\n[Test 6] break-line-at-intersections (X-junction)...")
  (command-s "_.line" "0,0" "1000,1000" "") ; diagonal
  (setq ent1 (entlast))
  (command-s "_.line" "0,1000" "1000,0" "") ; crossing diagonal
  (setq ent2 (entlast))
  (setq ss (ssadd))
  (setq ss (ssadd ent1 ss))
  (setq ss (ssadd ent2 ss))
  (setq result (break-line-at-intersections ent1 ss))
  ;; Each line should be split into 2 segments
  (if (and result (= (sslength result) 2))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  ;; Cleanup
  (if result
    (progn
      (setq i 0)
      (repeat (sslength result)
        (command-s "_.erase" (ssname result i) "")
        (setq i (1+ i))
      )
    )
  )
  (command-s "_.erase" ent2 "")

  ;; Test 7: break-lines-in-set (grid pattern)
  (princ "\n[Test 7] break-lines-in-set (grid pattern)...")
  ;; Create a simple grid
  (command-s "_.line" "0,0" "1000,0" "")    ; horizontal
  (setq ent1 (entlast))
  (command-s "_.line" "0,500" "1000,500" "") ; horizontal
  (setq ent2 (entlast))
  (command-s "_.line" "250,-100" "250,600" "") ; vertical
  (setq ent3 (entlast))
  (command-s "_.line" "750,-100" "750,600" "") ; vertical
  (setq ent4 (entlast))
  (setq ss (ssadd))
  (setq ss (ssadd ent1 ss))
  (setq ss (ssadd ent2 ss))
  (setq ss (ssadd ent3 ss))
  (setq ss (ssadd ent4 ss))
  (setq result (break-lines-in-set ss))
  ;; 4 lines, 4 intersection points, each line split into 3 segments
  ;; Total: 12 segments
  (if (and result (= (sslength result) 12))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn 
      (princ (strcat " FAIL (got " (itoa (sslength result)) " segments, expected 12)"))
      (setq failed (1+ failed))
    )
  )
  ;; Cleanup
  (if result
    (progn
      (setq i 0)
      (repeat (sslength result)
        (command-s "_.erase" (ssname result i) "")
        (setq i (1+ i))
      )
    )
  )

  ;; Test 8: break-lines-all (integration)
  (princ "\n[Test 8] break-lines-all...")
  (command-s "_.line" "0,0" "1000,0" "")
  (setq ent1 (entlast))
  (command-s "_.line" "500,-500" "500,500" "")
  (setq ent2 (entlast))
  (setq result (break-lines-all "0"))
  (if (and result (> (sslength result) 0))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  ;; Cleanup
  (if result
    (progn
      (setq i 0)
      (repeat (sslength result)
        (command-s "_.erase" (ssname result i) "")
        (setq i (1+ i))
      )
    )
  )

  ;; Test 9: tolerance get/set
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

  (command-s "_.undo" "_e")

  ;; Summary
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
(princ "\n  Test: (test-M05-break-lines)")
(princ)
