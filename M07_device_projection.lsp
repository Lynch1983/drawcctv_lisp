;;;===============================================================
;;;  M07 - Device Projection Module
;;;  Project device points to graph network
;;;===============================================================
;;;  ENCODING: ANSI (ASCII only, no Chinese characters)
;;;  DEPENDENCIES: M01, M02, M06
;;;===============================================================

;;;---------------------------------------------------------------
;;;  Global Parameters
;;;---------------------------------------------------------------
(setq *device-search-radius* 1500.0)
(setq *device-pipe-layer* nil)  ; Optional pipe layer for routing

;;;---------------------------------------------------------------
;;;  device-find-nearest-line
;;;  Find the nearest LINE entity to a point
;;;  Args: pt       - point to project
;;;         line-ss  - selection set of lines (nil = use filter)
;;;         gllst    - optional DXF filter
;;;  Returns: (entity closest-point distance) or nil
;;;---------------------------------------------------------------
(defun device-find-nearest-line (pt line-ss gllst / i ent min-ent min-pt min-dist cp d)
  (if (null line-ss)
    (progn
      (if gllst
        (setq line-ss (ssget "x" (append '((0 . "LINE")) gllst)))
        (setq line-ss (ssget "x" '((0 . "LINE"))))
      )
    )
  )
  (if line-ss
    (progn
      (setq min-ent nil min-pt nil min-dist *device-search-radius*)
      (setq i 0)
      (repeat (sslength line-ss)
        (setq ent (ssname line-ss i))
        (if ent
          (progn
            (setq cp (line-get-closest-point ent pt))
            (if cp
              (progn
                (setq d (distance pt cp))
                (if (< d min-dist)
                  (progn
                    (setq min-dist d)
                    (setq min-pt cp)
                    (setq min-ent ent)
                  )
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )
      (if min-ent (list min-ent min-pt min-dist) nil)
    )
    nil
  )
)

;;;---------------------------------------------------------------
;;;  device-find-nearest-pipe
;;;  Find the nearest pipe entity for routing
;;;  Args: pt        - point to project
;;;         pipe-ss   - selection set of pipes
;;;  Returns: (entity closest-point distance) or nil
;;;---------------------------------------------------------------
(defun device-find-nearest-pipe (pt pipe-ss / i ent min-ent min-pt min-dist cp d)
  (if pipe-ss
    (progn
      (setq min-ent nil min-pt nil min-dist *device-search-radius*)
      (setq i 0)
      (repeat (sslength pipe-ss)
        (setq ent (ssname pipe-ss i))
        (if ent
          (progn
            (setq cp (line-get-closest-point ent pt))
            (if cp
              (progn
                (setq d (distance pt cp))
                (if (< d min-dist)
                  (progn
                    (setq min-dist d)
                    (setq min-pt cp)
                    (setq min-ent ent)
                  )
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )
      (if min-ent (list min-ent min-pt min-dist) nil)
    )
    nil
  )
)

;;;---------------------------------------------------------------
;;;  device-project-to-graph
;;;  Project a device point onto the graph network
;;;  Args: pt        - device point
;;;         line-ss   - selection set of lines
;;;         pipe-ss   - optional pipe selection set for routing
;;;  Returns: (graph-node-index projected-point distance) or nil
;;;---------------------------------------------------------------
(defun device-project-to-graph (pt line-ss pipe-ss / nearest pipe-nearest)
  ;; First try direct projection to lines
  (setq nearest (device-find-nearest-line pt line-ss nil))
  (if nearest
    (progn
      ;; Add projection point to graph
      (setq node-idx (graph-add-node (cadr nearest)))
      (list node-idx (cadr nearest) (caddr nearest))
    )
    ;; Try pipe routing if available
    (if pipe-ss
      (progn
        (setq pipe-nearest (device-find-nearest-pipe pt pipe-ss))
        (if pipe-nearest
          (progn
            (setq node-idx (graph-add-node (cadr pipe-nearest)))
            (list node-idx (cadr pipe-nearest) (caddr pipe-nearest))
          )
          nil
        )
      )
      nil
    )
  )
)

;;;---------------------------------------------------------------
;;;  device-calculate-distance
;;;  Calculate total distance from device to target through graph
;;;  Args: device-pt   - device point
;;;         target-pt   - target point (e.g., junction box)
;;;         cable-coef  - cable length coefficient
;;;         junction-bias - junction box distance bias
;;;  Returns: total weighted distance or nil
;;;---------------------------------------------------------------
(defun device-calculate-distance (device-pt target-pt cable-coef junction-bias / dev-info tgt-info graph-dist total-dist line-ss)
  (if (null *graph-floyd-done*)
    (progn (princ "\n[device] Error: Floyd-Warshall not computed.") nil)
    (progn
      ;; Get lines from temp layer only (not entire drawing)
      (setq line-ss (ssget "x" (list (cons 0 "LINE") (cons 8 *main-temp-layer*))))
      ;; Project device to graph
      (setq dev-info (graph-project-point device-pt line-ss nil))
      ;; Project target to graph
      (setq tgt-info (graph-project-point target-pt line-ss nil))
      (if (and dev-info tgt-info)
        (progn
          (setq graph-dist (graph-get-distance (car dev-info) (car tgt-info)))
          (if graph-dist
            (progn
              (setq total-dist (+ (* (+ graph-dist (caddr dev-info) (caddr tgt-info)) cable-coef)
                                  junction-bias))
              total-dist
            )
            nil
          )
        )
        nil
      )
    )
  )
)

;;;---------------------------------------------------------------
;;;  device-find-best-junction
;;;  Find the best junction box for a device
;;;  Args: device-pt     - device point
;;;         junction-list - list of junction box points
;;;         cable-coef    - cable coefficient
;;;         junction-bias - junction bias
;;;  Returns: (junction-pt junction-node total-distance) or nil
;;;---------------------------------------------------------------
(defun device-find-best-junction (device-pt junction-list cable-coef junction-bias / best-jnx best-dist dist)
  (setq best-jnx nil)
  (setq best-dist 1e30)
  (foreach jnx-pt junction-list
    (setq dist (device-calculate-distance device-pt jnx-pt cable-coef junction-bias))
    (if (and dist (< dist best-dist))
      (progn
        (setq best-dist dist)
        (setq best-jnx (list jnx-pt dist))
      )
    )
  )
  best-jnx
)

;;;---------------------------------------------------------------
;;;  device-process-all
;;;  Process all devices and find their best junction connections
;;;  Args: device-blocks   - selection set of device blocks
;;;         junction-blocks - selection set of junction blocks
;;;         cable-coef      - cable coefficient
;;;         junction-bias   - junction bias
;;;  Returns: list of (device-pt junction-pt distance device-name)
;;;---------------------------------------------------------------
(defun device-process-all (device-blocks junction-blocks cable-coef junction-bias / device-list junction-list result i ent pt name best)
  (princ "\n[device] Processing devices...")
  
  ;; Build device list
  (setq device-list nil)
  (if device-blocks
    (progn
      (setq i 0)
      (repeat (sslength device-blocks)
        (setq ent (ssname device-blocks i))
        (if ent
          (progn
            (setq pt (block-get-base-point ent))
            (setq name (block-get-name-from-text ent nil))
            (if pt
              (setq device-list (cons (list pt ent name) device-list))
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )
  (princ (strcat "\n[device] Found " (itoa (length device-list)) " devices."))
  
  ;; Build junction list
  (setq junction-list nil)
  (if junction-blocks
    (progn
      (setq i 0)
      (repeat (sslength junction-blocks)
        (setq ent (ssname junction-blocks i))
        (if ent
          (progn
            (setq pt (block-get-base-point ent))
            (if pt
              (setq junction-list (cons pt junction-list))
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )
  (princ (strcat "\n[device] Found " (itoa (length junction-list)) " junctions."))
  
  ;; Find best junction for each device
  (setq result nil)
  (foreach dev device-list
    (setq best (device-find-best-junction (car dev) junction-list cable-coef junction-bias))
    (if best
      (setq result (cons (list (car dev) (car best) (cadr best) (caddr dev)) result))
    )
  )
  
  (princ (strcat "\n[device] Connected " (itoa (length result)) " devices."))
  result
)

;;;---------------------------------------------------------------
;;;  device-set-search-radius
;;;  Set the search radius for finding nearest lines
;;;---------------------------------------------------------------
(defun device-set-search-radius (val)
  (setq *device-search-radius* val)
)

;;;---------------------------------------------------------------
;;;  device-get-search-radius
;;;  Get the current search radius
;;;---------------------------------------------------------------
(defun device-get-search-radius ()
  *device-search-radius*
)

;;;---------------------------------------------------------------
;;;  device-set-pipe-layer
;;;  Set the pipe layer name for routing
;;;---------------------------------------------------------------
(defun device-set-pipe-layer (layer)
  (setq *device-pipe-layer* layer)
)

;;;---------------------------------------------------------------
;;;  device-get-pipe-layer
;;;  Get the current pipe layer name
;;;---------------------------------------------------------------
(defun device-get-pipe-layer ()
  *device-pipe-layer*
)

;;;===============================================================
;;;  TEST FUNCTIONS
;;;===============================================================

(defun test-M07-device-projection (/ passed failed old-cmdecho result line-ent line-ss
                                          old-rad pt1 pt2 pt3 node1 node2 graph-dist)
  (setq passed 0 failed 0)
  (setq old-cmdecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)

  (princ "\n\n=== M07 Device Projection Tests ===")

  (command-s "_.undo" "_be")

  ;; Test 1: device-find-nearest-line - basic
  (princ "\n[Test 1] device-find-nearest-line (basic)...")
  (command-s "_.line" "0,0" "1000,0" "")
  (setq line-ent (entlast))
  (setq result (device-find-nearest-line (list 500.0 100.0 0.0) nil nil))
  (if (and result
           (= (length result) 3)
           (< (abs (- (car (cadr result)) 500.0)) 0.001)
           (< (abs (- (cadr (cadr result)) 0.0)) 0.001))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  (command-s "_.erase" line-ent "")

  ;; Test 2: device-find-nearest-line - with selection set
  (princ "\n[Test 2] device-find-nearest-line (with ss)...")
  (command-s "_.line" "0,0" "1000,0" "")
  (setq line-ent (entlast))
  (command-s "_.line" "0,1000" "1000,1000" "")  ; Another line far away
  (setq line-ent2 (entlast))
  (setq line-ss (ssadd))
  (setq line-ss (ssadd line-ent line-ss))
  (setq line-ss (ssadd line-ent2 line-ss))
  (setq result (device-find-nearest-line (list 500.0 50.0 0.0) line-ss nil))
  (if (and result
           (< (caddr result) 100.0))  ; Should find the closer line
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  (command-s "_.erase" line-ent line-ent2 "")

  ;; Test 3: device-find-nearest-line - no line in range
  (princ "\n[Test 3] device-find-nearest-line (out of range)...")
  (setq old-rad (device-get-search-radius))
  (device-set-search-radius 100.0)  ; Very small radius
  (command-s "_.line" "0,0" "1000,0" "")
  (setq line-ent (entlast))
  (setq result (device-find-nearest-line (list 500.0 500.0 0.0) nil nil))
  (if (null result)  ; Should return nil (too far)
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  (command-s "_.erase" line-ent "")
  (device-set-search-radius old-rad)

  ;; Test 4: device-project-to-graph
  (princ "\n[Test 4] device-project-to-graph...")
  (command-s "_.line" "0,0" "1000,0" "")
  (setq line-ent (entlast))
  (setq line-ss (ssadd))
  (setq line-ss (ssadd line-ent line-ss))
  (graph-init)
  (setq result (device-project-to-graph (list 500.0 100.0 0.0) line-ss nil))
  (if (and result
           (= (length result) 3)
           (numberp (car result)))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  (command-s "_.erase" line-ent "")

  ;; Test 5: device-find-nearest-pipe
  (princ "\n[Test 5] device-find-nearest-pipe...")
  (command-s "_.line" "0,0" "0,1000" "")  ; Vertical line as "pipe"
  (setq pipe-ent (entlast))
  (setq pipe-ss (ssadd))
  (setq pipe-ss (ssadd pipe-ent pipe-ss))
  (setq result (device-find-nearest-pipe (list 50.0 500.0 0.0) pipe-ss))
  (if (and result
           (= (length result) 3)
           (< (caddr result) 100.0))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  (command-s "_.erase" pipe-ent "")

  ;; Test 6: radius get/set
  (princ "\n[Test 6] radius get/set...")
  (setq old-rad (device-get-search-radius))
  (device-set-search-radius 2500.0)
  (if (= (device-get-search-radius) 2500.0)
    (progn
      (princ " PASS")
      (setq passed (1+ passed))
      (device-set-search-radius old-rad)
    )
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 7: pipe layer get/set
  (princ "\n[Test 7] pipe layer get/set...")
  (setq old-layer (device-get-pipe-layer))
  (device-set-pipe-layer "TEST_PIPE")
  (if (= (device-get-pipe-layer) "TEST_PIPE")
    (progn
      (princ " PASS")
      (setq passed (1+ passed))
      (device-set-pipe-layer old-layer)
    )
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 8: device-calculate-distance (requires graph with Floyd computed)
  (princ "\n[Test 8] device-calculate-distance...")
  ;; Create a simple graph: line from (0,0) to (1000,0)
  (command-s "_.line" "0,0" "1000,0" "")
  (setq line-ent (entlast))
  ;; Build graph
  (graph-init)
  (setq line-ss (ssadd))
  (setq line-ss (ssadd line-ent line-ss))
  (graph-build-from-lines line-ss nil)
  (graph-floyd-compute)
  ;; Calculate distance from device at (500,100) to target at (500,0)
  (setq result (device-calculate-distance (list 500.0 100.0 0.0) (list 500.0 0.0 0.0) 1.0 0.0))
  (if (and result
           (> result 0)
           (< result 200.0))  ; Should be ~100 + small graph distance
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ (strcat " FAIL (got " (if result (rtos result 2 1) "nil") ")")) (setq failed (1+ failed)))
  )
  (command-s "_.erase" line-ent "")

  ;; Test 9: device-find-best-junction
  (princ "\n[Test 9] device-find-best-junction...")
  ;; Create graph with two lines forming a path
  (command-s "_.line" "0,0" "1000,0" "")
  (setq line-ent (entlast))
  (command-s "_.line" "1000,0" "2000,0" "")
  (setq line-ent2 (entlast))
  ;; Build graph
  (graph-init)
  (setq line-ss (ssadd))
  (setq line-ss (ssadd line-ent line-ss))
  (setq line-ss (ssadd line-ent2 line-ss))
  (graph-build-from-lines line-ss nil)
  (graph-floyd-compute)
  ;; Junction at (1500, 0), device at (100, 50)
  (setq junction-list (list (list 1500.0 0.0 0.0)))
  (setq result (device-find-best-junction (list 100.0 50.0 0.0) junction-list 1.0 0.0))
  (if (and result
           (= (length result) 2)
           (> (cadr result) 0))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  (command-s "_.erase" line-ent line-ent2 "")

  ;; Test 10: device-find-best-junction - multiple junctions
  (princ "\n[Test 10] device-find-best-junction (multiple)...")
  ;; Create a simple line
  (command-s "_.line" "0,0" "2000,0" "")
  (setq line-ent (entlast))
  (graph-init)
  (setq line-ss (ssadd))
  (setq line-ss (ssadd line-ent line-ss))
  (graph-build-from-lines line-ss nil)
  (graph-floyd-compute)
  ;; Device at (1000, 50), junctions at (100, 0) and (1900, 0)
  ;; Should pick the closer one (100, 0)
  (setq junction-list (list (list 100.0 0.0 0.0) (list 1900.0 0.0 0.0)))
  (setq result (device-find-best-junction (list 1000.0 50.0 0.0) junction-list 1.0 0.0))
  (if (and result
           (< (car (car result)) 200.0))  ; Should pick junction at x=100
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  (command-s "_.erase" line-ent "")

  ;; Test 11: device-find-best-junction - empty list
  (princ "\n[Test 11] device-find-best-junction (empty list)...")
  (setq result (device-find-best-junction (list 0.0 0.0 0.0) nil 1.0 0.0))
  (if (null result)
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  (command-s "_.undo" "_e")

  ;; Summary
  (princ (strcat "\n\n=== M07 Test Summary: "
                 (itoa passed) " passed, "
                 (itoa failed) " failed ===\n"))

  (setvar "cmdecho" old-cmdecho)
  (list passed failed)
)

;;;---------------------------------------------------------------
;;;  Provide module info
;;;---------------------------------------------------------------
(princ "\n[M07] device_projection.lsp loaded.")
(princ (strcat "  Functions: device-project-to-graph, device-calculate-distance, "
               "device-find-best-junction, device-process-all"))
(princ (strcat "\n  Default search radius: " (rtos *device-search-radius* 2 0)))
(princ "\n  Test: (test-M07-device-projection) - 11 test cases")
(princ)
