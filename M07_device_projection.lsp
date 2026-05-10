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
(setq *device-pipe-layer* nil)
(setq *pipe-nodes* nil)
(setq *pipe-edges* nil)
(setq *pipe-exits* nil)

;;;---------------------------------------------------------------
;;;  device-find-nearest-entity
;;;  Internal helper: find nearest entity in a selection set
;;;  Args: pt         - point to project
;;;         entity-ss  - selection set of entities
;;;         max-radius - maximum search radius
;;;  Returns: (entity closest-point distance) or nil
;;;---------------------------------------------------------------
(defun device-find-nearest-entity (pt entity-ss max-radius / i ent min-ent min-pt min-dist cp d)
  (if entity-ss
    (progn
      (setq min-ent nil min-pt nil min-dist max-radius)
      (setq i 0)
      (repeat (sslength entity-ss)
        (setq ent (ssname entity-ss i))
        (if ent
          (progn
            (setq cp (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list ent pt)))
            (if (and cp (not (vl-catch-all-error-p cp)))
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
;;;  device-find-nearest-line
;;;  Find the nearest LINE entity to a point
;;;  Args: pt       - point to project
;;;         line-ss  - selection set of lines (nil = use filter)
;;;         gllst    - optional DXF filter
;;;  Returns: (entity closest-point distance) or nil
;;;---------------------------------------------------------------
(defun device-find-nearest-line (pt line-ss gllst / ss)
  (if (null line-ss)
    (progn
      (if gllst
        (setq ss (ssget "x" (append '((0 . "LINE")) gllst)))
        (setq ss (ssget "x" '((0 . "LINE"))))
      )
      (if ss (device-find-nearest-entity pt ss *device-search-radius*) nil)
    )
    (device-find-nearest-entity pt line-ss *device-search-radius*)
  )
)

;;;---------------------------------------------------------------
;;;  device-find-nearest-pipe
;;;  Find the nearest pipe entity for routing
;;;  Args: pt        - point to project
;;;         pipe-ss   - selection set of pipes
;;;  Returns: (entity closest-point distance) or nil
;;;---------------------------------------------------------------
(defun device-find-nearest-pipe (pt pipe-ss)
  (device-find-nearest-entity pt pipe-ss *device-search-radius*)
)

;;;---------------------------------------------------------------
;;;  device-project-to-graph
;;;  Project a device point onto the graph network
;;;  Args: pt        - device point
;;;         line-ss   - selection set of lines
;;;         pipe-ss   - optional pipe selection set for routing
;;;  Returns: (graph-node-index projected-point distance) or nil
;;;---------------------------------------------------------------
(defun device-project-to-graph (pt line-ss pipe-ss / nearest pipe-nearest node-idx)
  (setq nearest (device-find-nearest-line pt line-ss nil))
  (if nearest
    (progn
      (setq node-idx (graph-add-node (cadr nearest)))
      (list node-idx (cadr nearest) (caddr nearest))
    )
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
      (setq dev-info (graph-project-point device-pt nil nil))
      (setq tgt-info (graph-project-point target-pt nil nil))
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
  (if (or (null device-blocks) (null junction-blocks))
    (progn
      (princ "\n[device] Warning: No device or junction blocks provided.")
      nil
    )
    (progn
      (princ "\n[device] Processing devices...")

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
  )
)

;;;---------------------------------------------------------------
;;;  pipe-add-node
;;;  Add a node to pipe graph, return index (dedup by coordinate)
;;;---------------------------------------------------------------
(defun pipe-add-node (pt / i found)
  (setq i 0)
  (setq found nil)
  (foreach n *pipe-nodes*
    (if (and (null found) (< (distance pt n) 0.001))
      (setq found i)
    )
    (setq i (1+ i))
  )
  (if found
    found
    (progn
      (setq i (length *pipe-nodes*))
      (setq *pipe-nodes* (append *pipe-nodes* (list pt)))
      i
    )
  )
)

;;;---------------------------------------------------------------
;;;  pipe-list-set
;;;  Replace element at index in a list
;;;---------------------------------------------------------------
(defun pipe-list-set (lst idx val / i)
  (setq i 0)
  (mapcar
    '(lambda (x)
      (if (= i idx)
        (progn (setq i (1+ i)) val)
        (progn (setq i (1+ i)) x)
      )
    )
    lst
  )
)

;;;---------------------------------------------------------------
;;;  pipe-build-adj-list
;;;  Build adjacency list from *pipe-edges*
;;;  Returns: list of ((neighbor-idx . dist) ...) per node
;;;---------------------------------------------------------------
(defun pipe-build-adj-list (n / adj i idx1 idx2 d)
  (setq adj nil)
  (setq i 0)
  (repeat n
    (setq adj (cons nil adj))
    (setq i (1+ i))
  )
  (setq adj (reverse adj))
  (foreach edge *pipe-edges*
    (setq idx1 (car edge))
    (setq idx2 (cadr edge))
    (setq d (caddr edge))
    (setq adj (pipe-list-set adj idx1 (cons (cons idx2 d) (nth idx1 adj))))
    (setq adj (pipe-list-set adj idx2 (cons (cons idx1 d) (nth idx2 adj))))
  )
  adj
)

;;;---------------------------------------------------------------
;;;  pipe-dijkstra
;;;  Dijkstra shortest path from start-idx on pipe graph
;;;  Returns: list of distances (one per node), 1e30 = unreachable
;;;---------------------------------------------------------------
(defun pipe-dijkstra (start-idx / n dist visited adj i j
                               cur-d cur-idx new-d nb-idx nb-d)
  (setq n (length *pipe-nodes*))
  (if (= n 0)
    nil
    (progn
      (setq dist nil)
      (setq i 0)
      (repeat n
        (setq dist (cons (if (= i start-idx) 0.0 1e30) dist))
        (setq i (1+ i))
      )
      (setq dist (reverse dist))
      (setq visited nil)
      (setq i 0)
      (repeat n
        (setq visited (cons nil visited))
        (setq i (1+ i))
      )
      (setq visited (reverse visited))
      (setq adj (pipe-build-adj-list n))
      (setq i 0)
      (repeat n
        (setq cur-d 1e30)
        (setq cur-idx nil)
        (setq j 0)
        (repeat n
          (if (and (null (nth j visited)) (< (nth j dist) cur-d))
            (progn
              (setq cur-d (nth j dist))
              (setq cur-idx j)
            )
          )
          (setq j (1+ j))
        )
        (if cur-idx
          (progn
            (setq visited (pipe-list-set visited cur-idx T))
            (foreach nb (nth cur-idx adj)
              (setq nb-idx (car nb))
              (setq nb-d (cdr nb))
              (setq new-d (+ cur-d nb-d))
              (if (< new-d (nth nb-idx dist))
                (setq dist (pipe-list-set dist nb-idx new-d))
              )
            )
          )
        )
        (setq i (1+ i))
      )
      dist
    )
  )
)

;;;---------------------------------------------------------------
;;;  device-build-pipe-graph
;;;  Build pipe sub-graph from LINEs on pipe layer.
;;;  Must be called AFTER graph-floyd-compute (needs *graph-nodes*).
;;;  Finds exit nodes: pipe nodes within 1500mm of cable tray nodes.
;;;  Returns: T on success, nil on failure
;;;---------------------------------------------------------------
(defun device-build-pipe-graph (pipe-layer / ss i ent endpts p1 p2 idx1 idx2 d
                                           pt tray-idx exit-info node-count)
  (setq *pipe-nodes* nil)
  (setq *pipe-edges* nil)
  (setq *pipe-exits* nil)
  (if (null pipe-layer)
    nil
    (progn
      (setq ss (ssget "x" (list (cons 0 "LINE") (cons 8 pipe-layer))))
      (if (null ss)
        nil
        (progn
          (setq i 0)
          (repeat (sslength ss)
            (setq ent (ssname ss i))
            (setq endpts (line-get-endpoints ent))
            (if endpts
              (progn
                (setq p1 (car endpts))
                (setq p2 (cadr endpts))
                (setq idx1 (pipe-add-node p1))
                (setq idx2 (pipe-add-node p2))
                (setq d (distance p1 p2))
                (if (/= idx1 idx2)
                  (setq *pipe-edges* (cons (list idx1 idx2 d) *pipe-edges*))
                )
              )
            )
            (setq i (1+ i))
          )
          (setq i 0)
          (repeat (length *pipe-nodes*)
            (setq pt (nth i *pipe-nodes*))
            (setq tray-idx (graph-get-node-index pt))
            (if tray-idx
              (setq *pipe-exits* (cons (list i tray-idx 0.0) *pipe-exits*))
              (progn
                (setq exit-info (pipe-find-nearest-tray-node pt))
                (if exit-info
                  (setq *pipe-exits* (cons (list i (car exit-info) (cadr exit-info)) *pipe-exits*))
                )
              )
            )
            (setq i (1+ i))
          )
          (princ (strcat "\n[pipe] Graph: " (itoa (length *pipe-nodes*)) " nodes, "
                         (itoa (length *pipe-edges*)) " edges, "
                         (itoa (length *pipe-exits*)) " exits."))
          T
        )
      )
    )
  )
)

;;;---------------------------------------------------------------
;;;  pipe-find-nearest-tray-node
;;;  Find nearest cable tray graph node within 1500mm
;;;  Returns: (tray-node-idx distance) or nil
;;;---------------------------------------------------------------
(defun pipe-find-nearest-tray-node (pt / i node-pt d best-idx best-d node-count)
  (setq best-idx nil)
  (setq best-d *device-search-radius*)
  (setq node-count (graph-get-node-count))
  (setq i 0)
  (repeat node-count
    (setq node-pt (graph-get-node-coord i))
    (if node-pt
      (progn
        (setq d (distance pt node-pt))
        (if (< d best-d)
          (progn
            (setq best-d d)
            (setq best-idx i)
          )
        )
      )
    )
    (setq i (1+ i))
  )
  (if best-idx
    (list best-idx best-d)
    nil
  )
)

;;;---------------------------------------------------------------
;;;  device-find-nearest-on-layer
;;;  Find the nearest LINE entity on a specific layer
;;;  Args: pt         - point to project
;;;         layer-name - layer name to search on
;;;  Returns: (entity closest-point distance) or nil
;;;---------------------------------------------------------------
(defun device-find-nearest-on-layer (pt layer-name / ss)
  (if (null layer-name)
    nil
    (progn
      (setq ss (ssget "x" (list (cons 0 "LINE") (cons 8 layer-name))))
      (if ss
        (device-find-nearest-entity pt ss *device-search-radius*)
        nil
      )
    )
  )
)

;;;---------------------------------------------------------------
;;;  device-connect-plan-a
;;;  Plan A: Project device point directly to cable tray lines
;;;  Args: base-pt   - device base point
;;;         tray-ss   - selection set of cable tray LINEs (nil=all)
;;;  Returns: (proj-pt proj-dist nearest-ent) or nil
;;;---------------------------------------------------------------
(defun device-connect-plan-a (base-pt tray-ss / result)
  (setq result (device-find-nearest-line base-pt tray-ss nil))
  (if result
    (list (cadr result) (caddr result) (car result))
    nil
  )
)

;;;---------------------------------------------------------------
;;;  device-connect-plan-b
;;;  Plan B: Route device through pipe network to cable tray.
;;;          Pipe graph is pre-built by device-build-pipe-graph.
;;;          Uses Dijkstra on pipe sub-graph, then exits to
;;;          cable tray graph via exit nodes.
;;;  Args: base-pt     - device base point
;;;         pipe-layer  - pipe layer name
;;;  Returns: list of (tray-node-idx . pipe-total-dist) for each
;;;           reachable exit, or nil
;;;           pipe-total-dist = cam_to_pipe + pipe_path + exit_to_tray
;;;---------------------------------------------------------------
(defun device-connect-plan-b (base-pt pipe-layer /
                                       pipe-result pipe-ent pipe-proj-pt pipe-proj-dist
                                       endpts ep1 ep2 ep1-idx ep2-idx
                                       dist1 dist2 exit-list exit
                                       exit-pipe-idx tray-idx exit-dist
                                       d1 d2 pipe-path-dist total-pipe-dist)
  (if (or (null pipe-layer) (null *pipe-nodes*) (null *pipe-exits*))
    nil
    (progn
      (setq pipe-result (device-find-nearest-on-layer base-pt pipe-layer))
      (if (null pipe-result)
        nil
        (progn
          (setq pipe-ent (car pipe-result))
          (setq pipe-proj-pt (cadr pipe-result))
          (setq pipe-proj-dist (caddr pipe-result))
          (setq endpts (line-get-endpoints pipe-ent))
          (if (null endpts)
            nil
            (progn
              (setq ep1 (car endpts))
              (setq ep2 (cadr endpts))
              (setq ep1-idx (pipe-add-node ep1))
              (setq ep2-idx (pipe-add-node ep2))
              (setq dist1 (pipe-dijkstra ep1-idx))
              (setq dist2 (pipe-dijkstra ep2-idx))
              (setq exit-list nil)
              (foreach exit *pipe-exits*
                (setq exit-pipe-idx (car exit))
                (setq tray-idx (cadr exit))
                (setq exit-dist (caddr exit))
                (setq pipe-path-dist nil)
                (if (and dist1 (nth exit-pipe-idx dist1) (< (nth exit-pipe-idx dist1) 1e29))
                  (progn
                    (setq d1 (+ pipe-proj-dist (distance pipe-proj-pt ep1)
                                (nth exit-pipe-idx dist1) exit-dist))
                    (setq pipe-path-dist d1)
                  )
                )
                (if (and dist2 (nth exit-pipe-idx dist2) (< (nth exit-pipe-idx dist2) 1e29))
                  (progn
                    (setq d2 (+ pipe-proj-dist (distance pipe-proj-pt ep2)
                                (nth exit-pipe-idx dist2) exit-dist))
                    (if (or (null pipe-path-dist) (< d2 pipe-path-dist))
                      (setq pipe-path-dist d2)
                    )
                  )
                )
                (if pipe-path-dist
                  (setq exit-list (cons (cons tray-idx pipe-path-dist) exit-list))
                )
              )
              (if exit-list exit-list nil)
            )
          )
        )
      )
    )
  )
)

;;;---------------------------------------------------------------
;;;  device-connect-plan-c
;;;  Plan C: Direct line to nearest junction/room entry point
;;;          No graph network dependency
;;;  Args: base-pt      - device base point
;;;         gjx-list     - list of (proj-pt . proj-dist)
;;;         gjx-name-list - list of junction names
;;;  Returns: (best-jnx-pt best-jnx-dist best-jnx-name) or nil
;;;---------------------------------------------------------------
(defun device-connect-plan-c (base-pt gjx-list gjx-name-list /
                                       m_n tmp-jnx tmp-jnx-pt tmp-jnx-dist
                                       tmp-jnx-name tmp-dis best-jnx best-dist best-name)
  (setq best-jnx nil)
  (setq best-dist 1e30)
  (setq best-name nil)
  (setq m_n 0)
  (repeat (length gjx-list)
    (setq tmp-jnx (nth m_n gjx-list))
    (setq tmp-jnx-pt (car tmp-jnx))
    (setq tmp-jnx-dist (cdr tmp-jnx))
    (setq tmp-jnx-name (nth m_n gjx-name-list))
    (setq tmp-dis (distance base-pt tmp-jnx-pt))
    (if (< tmp-dis best-dist)
      (progn
        (setq best-dist tmp-dis)
        (setq best-jnx tmp-jnx)
        (setq best-name tmp-jnx-name)
      )
    )
    (setq m_n (1+ m_n))
  )
  (if best-jnx
    (list (car best-jnx) best-dist best-name)
    nil
  )
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
