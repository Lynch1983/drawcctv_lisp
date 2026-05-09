;;;===============================================================
;;;  CCTV All-In-One Module
;;;  Combined from 13 module files
;;;  Modules: M01-M13
;;;===============================================================
;;;  ENCODING: ANSI (ASCII only, no Chinese characters)
;;;===============================================================

;;;===============================================================
;;;  M01 - Graph Algorithm Module
;;;  Core graph data structure and Floyd-Warshall shortest path
;;;  Based on graph_algorithm_module.lsp, refactored and optimized
;;;===============================================================
;;;  ENCODING: ANSI (ASCII only, no Chinese characters)
;;;  DEPENDENCIES: None
;;;===============================================================

;;;---------------------------------------------------------------
;;;  Global Variables
;;;---------------------------------------------------------------
;;;  *graph-nodes*       - assoc list: ((key . (index point)) ...)
;;;  *graph-edges*       - assoc list: (((nodeA . nodeB) . weight) ...)
;;;  *graph-adj*         - adjacency list: ((node . ((nbr . wt) ...)) ...)
;;;  *graph-dist*        - distance matrix after Floyd-Warshall
;;;  *graph-node-count*  - integer, number of nodes
;;;  *graph-floyd-done*  - T if Floyd-Warshall has been computed

(setq *graph-nodes* nil)
(setq *graph-edges* nil)
(setq *graph-adj* nil)
(setq *graph-dist* nil)
(setq *graph-node-count* 0)
(setq *graph-floyd-done* nil)

;;;---------------------------------------------------------------
;;;  graph-init
;;;  Reset all graph data to empty state
;;;---------------------------------------------------------------
(defun graph-init ()
  (setq *graph-nodes* nil)
  (setq *graph-edges* nil)
  (setq *graph-adj* nil)
  (setq *graph-dist* nil)
  (setq *graph-node-count* 0)
  (setq *graph-floyd-done* nil)
  T
)

;;;---------------------------------------------------------------
;;;  graph-coord->key
;;;  Convert 2D/3D point to string key "x,y"
;;;  Args: pt - point list (x y [z])
;;;  Returns: string key
;;;---------------------------------------------------------------
(defun graph-coord->key (pt / x y)
  (setq x (rtos (car pt) 2 6))
  (setq y (rtos (cadr pt) 2 6))
  (strcat x "," y)
)

;;;---------------------------------------------------------------
;;;  graph-key->coord
;;;  Convert string key back to point
;;;  Args: key - string "x,y"
;;;  Returns: point list (x y 0.0) or nil
;;;---------------------------------------------------------------
(defun graph-key->coord (key / pos x y)
  (setq pos (vl-string-search "," key))
  (if pos
    (progn
      (setq x (atof (substr key 1 pos)))
      (setq y (atof (substr key (+ pos 2))))
      (list x y 0.0)
    )
    nil
  )
)

;;;---------------------------------------------------------------
;;;  graph-add-node
;;;  Add a node to the graph. If node exists, return existing index.
;;;  Args: pt - point list (x y [z])
;;;  Returns: integer node index
;;;---------------------------------------------------------------
(defun graph-add-node (pt / key idx entry)
  (setq key (graph-coord->key pt))
  (setq entry (assoc key *graph-nodes*))
  (if (null entry)
    (progn
      (setq idx *graph-node-count*)
      (setq *graph-nodes* (cons (cons key (list idx pt)) *graph-nodes*))
      (setq *graph-node-count* (1+ *graph-node-count*))
      idx
    )
    (cadr entry)
  )
)

;;;---------------------------------------------------------------
;;;  graph-get-node-index
;;;  Get node index by point coordinate
;;;  Args: pt - point list
;;;  Returns: integer index or nil
;;;---------------------------------------------------------------
(defun graph-get-node-index (pt / key entry)
  (setq key (graph-coord->key pt))
  (setq entry (assoc key *graph-nodes*))
  (if entry (cadr entry) nil)
)

;;;---------------------------------------------------------------
;;;  graph-get-node-coord
;;;  Get node coordinate by index
;;;  Args: idx - integer node index
;;;  Returns: point list or nil
;;;---------------------------------------------------------------
(defun graph-get-node-coord (idx / node)
  (foreach n *graph-nodes*
    (if (= (cadr n) idx)
      (setq node (caddr n))
    )
  )
  node
)

;;;---------------------------------------------------------------
;;;  graph-node-exists
;;;  Check if a node exists at given point
;;;  Args: pt - point list
;;;  Returns: T or nil
;;;---------------------------------------------------------------
(defun graph-node-exists (pt / key)
  (setq key (graph-coord->key pt))
  (if (assoc key *graph-nodes*) T nil)
)

;;;---------------------------------------------------------------
;;;  graph-add-edge
;;;  Add an undirected edge between two nodes
;;;  Args: nodeA, nodeB - integer node indices
;;;         weight - edge weight (distance)
;;;  Returns: T if added, nil if duplicate
;;;---------------------------------------------------------------
(defun graph-add-edge (nodeA nodeB weight / key adjA adjB)
  ;; check duplicate
  (setq key (cons (min nodeA nodeB) (max nodeA nodeB)))
  (if (assoc key *graph-edges*)
    nil  ; duplicate edge
    (progn
      (setq *graph-edges* (cons (cons key weight) *graph-edges*))
      ;; update adjacency list for nodeA
      (setq adjA (assoc nodeA *graph-adj*))
      (if adjA
        (setq *graph-adj*
          (subst (cons nodeA (cons (cons nodeB weight) (cdr adjA))) adjA *graph-adj*)
        )
        (setq *graph-adj* (cons (list nodeA (cons nodeB weight)) *graph-adj*))
      )
      ;; update adjacency list for nodeB
      (setq adjB (assoc nodeB *graph-adj*))
      (if adjB
        (setq *graph-adj*
          (subst (cons nodeB (cons (cons nodeA weight) (cdr adjB))) adjB *graph-adj*)
        )
        (setq *graph-adj* (cons (list nodeB (cons nodeA weight)) *graph-adj*))
      )
      T
    )
  )
)

;;;---------------------------------------------------------------
;;;  graph-get-adjacent
;;;  Get adjacent nodes of a given node
;;;  Args: node - integer node index
;;;  Returns: list of (neighbor . weight) or nil
;;;---------------------------------------------------------------
(defun graph-get-adjacent (node / entry)
  (setq entry (assoc node *graph-adj*))
  (if entry (cdr entry) nil)
)

;;;---------------------------------------------------------------
;;;  graph-get-edge-weight
;;;  Get weight of edge between two nodes
;;;  Args: nodeA, nodeB - integer node indices
;;;  Returns: float weight or nil
;;;---------------------------------------------------------------
(defun graph-get-edge-weight (nodeA nodeB / key edge)
  (setq key (cons (min nodeA nodeB) (max nodeA nodeB)))
  (setq edge (assoc key *graph-edges*))
  (if edge (cdr edge) nil)
)

;;;---------------------------------------------------------------
;;;  graph-get-line-points
;;;  Extract endpoint(s) from a line/polyline/arc entity
;;;  Args: ent - entity name
;;;  Returns: list of point(s), nil if unsupported type
;;;---------------------------------------------------------------
(defun graph-get-line-points (ent / elist etype)
  (setq elist (entget ent))
  (setq etype (cdr (assoc 0 elist)))
  (cond
    ((= etype "LINE")
     (list (cdr (assoc 10 elist)) (cdr (assoc 11 elist)))
    )
    ((or (= etype "LWPOLYLINE") (= etype "POLYLINE"))
     (graph-get-polyline-points ent)
    )
    ((= etype "ARC")
     (graph-get-arc-points ent)
    )
    (T nil)
  )
)

;;;---------------------------------------------------------------
;;;  graph-get-polyline-points
;;;  Get all vertex points from a polyline
;;;  Args: ent - entity name
;;;  Returns: list of points
;;;---------------------------------------------------------------
(defun graph-get-polyline-points (ent / obj pts n i pt)
  (vl-load-com)
  (setq obj (vlax-ename->vla-object ent))
  (setq pts nil)
  (if (= (vla-get-ObjectName obj) "AcDbPolyline")
    (progn
      (setq n (fix (vlax-get obj 'NumberOfVertices)))
      (setq i 0)
      (repeat n
        (setq pt (vlax-get obj (strcat "Coordinate" (itoa i))))
        (if pt
          (setq pts (cons (list (car pt) (cadr pt) 0.0) pts))
        )
        (setq i (1+ i))
      )
      (reverse pts)
    )
    nil
  )
)

;;;---------------------------------------------------------------
;;;  graph-get-arc-points
;;;  Get start and end points from an arc
;;;  Args: ent - entity name
;;;  Returns: list of 2 points (start, end)
;;;---------------------------------------------------------------
(defun graph-get-arc-points (ent / obj center radius sa ea)
  (vl-load-com)
  (setq obj (vlax-ename->vla-object ent))
  (setq center (vlax-get obj 'Center))
  (setq radius (vlax-get obj 'Radius))
  (setq sa (vlax-get obj 'StartAngle))
  (setq ea (vlax-get obj 'EndAngle))
  (list
    (list (+ (car center) (* radius (cos sa)))
          (+ (cadr center) (* radius (sin sa))) 0.0)
    (list (+ (car center) (* radius (cos ea)))
          (+ (cadr center) (* radius (sin ea))) 0.0)
  )
)

;;;---------------------------------------------------------------
;;;  graph-build-from-lines
;;;  Build graph from a selection set of line entities
;;;  Args: line-ss - selection set (nil = use gllst filter)
;;;         gllst  - optional DXF filter list for ssget
;;;  Returns: T on success, nil on failure
;;;---------------------------------------------------------------
(defun graph-build-from-lines (line-ss gllst / i ent pts pt1 pt2 n1 n2 elen)
  (graph-init)
  (if (null line-ss)
    (progn
      (if gllst
        (setq line-ss (ssget "x" gllst))
        (setq line-ss (ssget "x" '((0 . "LINE"))))
      )
    )
  )
  (if (null line-ss)
    (progn (princ "\n[graph-build] No lines found.") nil)
    (progn
      (princ (strcat "\n[graph-build] Building from " (itoa (sslength line-ss)) " lines..."))
      (setq i 0)
      (repeat (sslength line-ss)
        (setq ent (ssname line-ss i))
        (if ent
          (progn
            (setq pts (graph-get-line-points ent))
            (if (= (length pts) 2)
              (progn
                (setq pt1 (car pts))
                (setq pt2 (cadr pts))
                (setq n1 (graph-add-node pt1))
                (setq n2 (graph-add-node pt2))
                (setq elen (distance pt1 pt2))
                (if (> elen 0.001)
                  (graph-add-edge n1 n2 elen)
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )
      (princ (strcat "\n[graph-build] Done: "
                     (itoa *graph-node-count*) " nodes, "
                     (itoa (length *graph-edges*)) " edges"))
      T
    )
  )
)

;;;---------------------------------------------------------------
;;;  graph-floyd-compute
;;;  Run Floyd-Warshall all-pairs shortest path algorithm
;;;  Fills *graph-dist* matrix
;;;  Returns: T on success
;;;---------------------------------------------------------------
(defun graph-floyd-compute (/ n i j k row dist_ik dist_kj new_dist dist_ij ew)
  (princ (strcat "\n[graph-floyd] Floyd-Warshall with "
                 (itoa *graph-node-count*) " nodes"))
  (if (< *graph-node-count* 2)
    (progn (princ "\n[graph-floyd] Not enough nodes.") nil)
    (progn
      (setq n *graph-node-count*)
      (setq *graph-dist* nil)

      ;; build initial distance matrix
      (setq i 0)
      (repeat n
        (setq row nil)
        (setq j 0)
        (repeat n
          (if (= i j)
            (setq row (cons 0.0 row))
            (progn
              (setq ew (graph-get-edge-weight i j))
              (if ew
                (setq row (cons ew row))
                (setq row (cons 1e30 row))  ; infinity
              )
            )
          )
          (setq j (1+ j))
        )
        (setq *graph-dist* (cons (reverse row) *graph-dist*))
        (setq i (1+ i))
      )
      (setq *graph-dist* (reverse *graph-dist*))

      (princ "\n[graph-floyd] Running optimization...")

      ;; Floyd-Warshall triple loop
      (setq k 0)
      (repeat n
        (if (= (rem k 50) 0)
          (princ (strcat "\r[graph-floyd] Progress: "
                         (itoa k) "/" (itoa n)))
        )
        (setq i 0)
        (repeat n
          (setq dist_ik (nth k (nth i *graph-dist*)))
          (if (< dist_ik 1e29)
            (progn
              (setq j 0)
              (repeat n
                (setq dist_kj (nth j (nth k *graph-dist*)))
                (if (< dist_kj 1e29)
                  (progn
                    (setq new_dist (+ dist_ik dist_kj))
                    (setq dist_ij (nth j (nth i *graph-dist*)))
                    (if (< new_dist dist_ij)
                      (graph-update-matrix i j new_dist)
                    )
                  )
                )
                (setq j (1+ j))
              )
            )
          )
          (setq i (1+ i))
        )
        (setq k (1+ k))
      )

      (setq *graph-floyd-done* T)
      (princ (strcat "\n[graph-floyd] Done."))
      T
    )
  )
)

;;;---------------------------------------------------------------
;;;  graph-update-matrix (internal)
;;;  Update a single cell in the distance matrix
;;;---------------------------------------------------------------
(defun graph-update-matrix (row col val / i j result row-data)
  (setq result nil)
  (setq i 0)
  (foreach r *graph-dist*
    (if (= i row)
      (progn
        (setq row-data nil)
        (setq j 0)
        (foreach c r
          (if (= j col)
            (setq row-data (cons val row-data))
            (setq row-data (cons c row-data))
          )
          (setq j (1+ j))
        )
        (setq result (cons (reverse row-data) result))
      )
      (setq result (cons r result))
    )
    (setq i (1+ i))
  )
  (setq *graph-dist* (reverse result))
)

;;;---------------------------------------------------------------
;;;  graph-get-distance
;;;  Get shortest distance between two nodes
;;;  Args: nodeA, nodeB - integer node indices
;;;  Returns: float distance or nil if unreachable
;;;---------------------------------------------------------------
(defun graph-get-distance (nodeA nodeB / row)
  (if (null *graph-floyd-done*)
    (progn (princ "\n[graph] Error: Floyd-Warshall not computed.") nil)
    (if (and (< nodeA *graph-node-count*)
             (< nodeB *graph-node-count*)
             *graph-dist*)
      (progn
        (setq row (nth nodeA *graph-dist*))
        (if (and row (< nodeB (length row)))
          (progn
            (setq d (nth nodeB row))
            (if (< d 1e29) d nil)
          )
          nil
        )
      )
      nil
    )
  )
)

;;;---------------------------------------------------------------
;;;  graph-get-shortest-path-distance
;;;  Get shortest path distance between two points
;;;  Args: ptA, ptB - point lists
;;;  Returns: float distance or nil
;;;---------------------------------------------------------------
(defun graph-get-shortest-path-distance (ptA ptB / nA nB)
  (setq nA (graph-get-node-index ptA))
  (setq nB (graph-get-node-index ptB))
  (if (and nA nB)
    (graph-get-distance nA nB)
    nil
  )
)

;;;---------------------------------------------------------------
;;;  graph-find-nearest-edge
;;;  Find the nearest graph edge to a given point
;;;  Args: pt    - point list
;;;         line-ss - selection set of lines (nil = use gllst)
;;;         gllst  - optional DXF filter
;;;  Returns: (entity closest-point distance) or nil
;;;---------------------------------------------------------------
(defun graph-find-nearest-edge (pt line-ss gllst / i ent min-ent min-pt min-dis tmp-pt tmp-dis)
  (if (null line-ss)
    (progn
      (if gllst
        (setq line-ss (ssget "x" gllst))
        (setq line-ss (ssget "x" '((0 . "LINE"))))
      )
    )
  )
  (if (null line-ss)
    nil
    (progn
      (setq i 0 min-ent nil min-pt nil min-dis 1e30)
      (repeat (sslength line-ss)
        (setq ent (ssname line-ss i))
        (if ent
          (progn
            (setq tmp-pt (vlax-curve-getClosestPointTo ent pt))
            (if tmp-pt
              (progn
                (setq tmp-dis (distance pt tmp-pt))
                (if (< tmp-dis min-dis)
                  (progn
                    (setq min-dis tmp-dis)
                    (setq min-pt tmp-pt)
                    (setq min-ent ent)
                  )
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )
      (if min-ent (list min-ent min-pt min-dis) nil)
    )
  )
)

;;;---------------------------------------------------------------
;;;  graph-project-point
;;;  Project a point onto the nearest graph edge as a new node
;;;  Args: pt     - point list
;;;         line-ss - selection set (nil = use gllst)
;;;         gllst  - optional DXF filter
;;;  Returns: (node-index projected-point distance) or nil
;;;---------------------------------------------------------------
(defun graph-project-point (pt line-ss gllst / nearest)
  (setq nearest (graph-find-nearest-edge pt line-ss gllst))
  (if nearest
    (progn
      (setq nidx (graph-add-node (cadr nearest)))
      (list nidx (cadr nearest) (caddr nearest))
    )
    nil
  )
)

;;;---------------------------------------------------------------
;;;  graph-assign-devices
;;;  Match a list of device points to a list of junction points
;;;  using pre-computed shortest paths
;;;  Args: device-list - list of device points
;;;         junction-list - list of junction points
;;;         cable-coef - cable length coefficient (e.g. 1.2)
;;;         junction-bias - junction distance bias
;;;  Returns: list of (device-pt junction-pt junction-node total-dist)
;;;---------------------------------------------------------------
(defun graph-assign-devices (device-list junction-list cable-coef junction-bias
                            / dev-nodes jnx-nodes result
                              dev-pt dev-info dev-node dev-dist
                              jnx-pt jnx-info jnx-node jnx-dist
                              total-dist best-jnx best-total
                              graph-dist entry)

  (princ (strcat "\n[graph-assign] Matching "
                 (itoa (length device-list)) " devices to "
                 (itoa (length junction-list)) " junctions"))

  (if (null *graph-floyd-done*)
    (progn (princ "\n[graph-assign] Error: Run graph-floyd-compute first.") nil)
    (progn
      ;; Step 1: Project devices to graph
      (princ "\n[graph-assign] Step 1: Projecting devices...")
      (setq dev-nodes nil)
      (foreach dp device-list
        (setq dev-info (graph-project-point dp nil nil))
        (if dev-info
          (setq dev-nodes (cons (cons dp dev-info) dev-nodes))
        )
      )
      (princ (strcat "\n[graph-assign] Projected " (itoa (length dev-nodes)) " devices"))

      ;; Step 2: Project junctions to graph
      (princ "\n[graph-assign] Step 2: Projecting junctions...")
      (setq jnx-nodes nil)
      (foreach jp junction-list
        (setq jnx-info (graph-project-point jp nil nil))
        (if jnx-info
          (setq jnx-nodes (cons (cons jp jnx-info) jnx-nodes))
        )
      )
      (princ (strcat "\n[graph-assign] Projected " (itoa (length jnx-nodes)) " junctions"))

      ;; Step 3: Match each device to best junction
      (princ "\n[graph-assign] Step 3: Matching...")
      (setq result nil)
      (foreach dev-entry (reverse dev-nodes)
        (setq dev-pt (car dev-entry))
        (setq dev-info (cdr dev-entry))
        (if (= (length dev-info) 3)
          (progn
            (setq dev-node (car dev-info))
            (setq dev-dist (caddr dev-info))
            (if (numberp dev-node)
              (progn
                (setq best-jnx nil best-total 1e30)
                (foreach jnx-entry (reverse jnx-nodes)
                  (setq jnx-pt (car jnx-entry))
                  (setq jnx-info (cdr jnx-entry))
                  (if (= (length jnx-info) 3)
                    (progn
                      (setq jnx-node (car jnx-info))
                      (setq jnx-dist (caddr jnx-info))
                      (if (numberp jnx-node)
                        (progn
                          (setq graph-dist (graph-get-distance dev-node jnx-node))
                          (if (and graph-dist (< graph-dist 1e29))
                            (progn
                              (setq total-dist
                                (+ (* (+ graph-dist dev-dist jnx-dist) cable-coef)
                                   junction-bias))
                              (if (< total-dist best-total)
                                (progn
                                  (setq best-total total-dist)
                                  (setq best-jnx (list jnx-pt jnx-node jnx-dist))
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
                (if best-jnx
                  (progn
                    (princ (strcat "\n[graph-assign] BEST: jnx-node="
                                   (itoa (cadr best-jnx))
                                   " total=" (rtos best-total 2 2)))
                    (setq result (cons (list dev-pt best-jnx best-total) result))
                  )
                )
              )
            )
          )
        )
      )
      (princ (strcat "\n[graph-assign] Done: " (itoa (length result)) " connections"))
      result
    )
  )
)

;;;---------------------------------------------------------------
;;;  graph-get-node-count
;;;  Returns current number of nodes
;;;---------------------------------------------------------------
(defun graph-get-node-count ()
  *graph-node-count*
)

;;;---------------------------------------------------------------
;;;  graph-get-edge-count
;;;  Returns current number of edges
;;;---------------------------------------------------------------
(defun graph-get-edge-count ()
  (length *graph-edges*)
)

;;;===============================================================
;;;  TEST FUNCTIONS
;;;===============================================================

;;;---------------------------------------------------------------
;;;  test-M01-graph-algorithm
;;;  Comprehensive test for graph module
;;;  Run: (test-M01-graph-algorithm)
;;;---------------------------------------------------------------
(defun test-M01-graph-algorithm (/ passed failed old-cmdecho
                                    p1 p2 p3 p4 p5 p6 ss n1 n2 d)
  (setq passed 0 failed 0)
  (setq old-cmdecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)

  (princ "\n\n=== M01 Graph Algorithm Tests ===")

  ;; Test 1: graph-init
  (princ "\n[Test 1] graph-init...")
  (graph-init)
  (if (and (null *graph-nodes*)
           (null *graph-edges*)
           (= *graph-node-count* 0)
           (null *graph-floyd-done*))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 2: graph-coord->key and graph-key->coord
  (princ "\n[Test 2] coord->key / key->coord...")
  (setq p1 (list 100.0 200.0 0.0))
  (setq k1 (graph-coord->key p1))
  (setq p1b (graph-key->coord k1))
  (if (and k1
           (< (abs (- (car p1) (car p1b))) 0.0001)
           (< (abs (- (cadr p1) (cadr p1b))) 0.0001))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 3: graph-add-node
  (princ "\n[Test 3] graph-add-node...")
  (graph-init)
  (setq n1 (graph-add-node (list 0.0 0.0 0.0)))
  (setq n2 (graph-add-node (list 100.0 0.0 0.0)))
  (setq n3 (graph-add-node (list 100.0 100.0 0.0)))
  ;; add duplicate
  (setq n4 (graph-add-node (list 0.0 0.0 0.0)))
  (if (and (= n1 0) (= n2 1) (= n3 2) (= n4 0) (= *graph-node-count* 3))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 4: graph-node-exists
  (princ "\n[Test 4] graph-node-exists...")
  (if (and (graph-node-exists (list 0.0 0.0 0.0))
           (graph-node-exists (list 100.0 0.0 0.0))
           (null (graph-node-exists (list 50.0 50.0 0.0))))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 5: graph-get-node-coord
  (princ "\n[Test 5] graph-get-node-coord...")
  (setq pc (graph-get-node-coord 1))
  (if (and pc
           (< (abs (- (car pc) 100.0)) 0.0001)
           (< (abs (- (cadr pc) 0.0)) 0.0001))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 6: graph-add-edge
  (princ "\n[Test 6] graph-add-edge...")
  (setq r1 (graph-add-edge 0 1 100.0))
  (setq r2 (graph-add-edge 1 2 100.0))
  (setq r3 (graph-add-edge 0 2 141.421))  ; diagonal
  ;; duplicate should return nil
  (setq r4 (graph-add-edge 0 1 100.0))
  (if (and r1 r2 r3 (null r4) (= (length *graph-edges*) 3))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 7: graph-get-adjacent
  (princ "\n[Test 7] graph-get-adjacent...")
  (setq adj (graph-get-adjacent 1))
  (if (= (length adj) 2)
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 8: graph-get-edge-weight
  (princ "\n[Test 8] graph-get-edge-weight...")
  (setq ew (graph-get-edge-weight 0 1))
  (if (and ew (< (abs (- ew 100.0)) 0.001))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 9: graph-floyd-compute
  (princ "\n[Test 9] graph-floyd-compute...")
  (graph-floyd-compute)
  (if *graph-floyd-done*
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 10: graph-get-distance
  (princ "\n[Test 10] graph-get-distance...")
  (setq d01 (graph-get-distance 0 1))
  (setq d02 (graph-get-distance 0 2))
  ;; 0->2 direct = 141.421, 0->1->2 = 200, so shortest = 141.421
  (if (and (< (abs (- d01 100.0)) 0.01)
           (< (abs (- d02 141.421)) 0.1)
           (< (abs (- (graph-get-distance 0 0) 0.0)) 0.001))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 11: graph-build-from-lines (with real entities)
  (princ "\n[Test 11] graph-build-from-lines...")
  (command-s "_.undo" "_be")
  ;; create test lines: a square 1000x1000
  (setvar "clayer" "0")
  (command-s "_.line" "0,0" "1000,0" "")
  (command-s "_.line" "1000,0" "1000,1000" "")
  (command-s "_.line" "1000,1000" "0,1000" "")
  (command-s "_.line" "0,1000" "0,0" "")
  (command-s "_.line" "0,0" "1000,1000" "")  ; diagonal
  (setq ss (ssget "x" '((0 . "LINE") (8 . "0"))))
  (graph-build-from-lines ss nil)
  (graph-floyd-compute)
  ;; should have 5 edges, 5 nodes (4 corners + 1 duplicate for diagonal)
  ;; actually 4 unique nodes, 5 edges
  (if (and (= *graph-node-count* 4)
           (= (length *graph-edges*) 5)
           *graph-floyd-done*)
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn
      (princ (strcat " FAIL (nodes=" (itoa *graph-node-count*)
                     " edges=" (itoa (length *graph-edges*)) ")"))
      (setq failed (1+ failed))
    )
  )
  ;; verify diagonal distance
  (setq diag-d (graph-get-shortest-path-distance
                  (list 0.0 0.0 0.0) (list 1000.0 1000.0 0.0)))
  (princ (strcat "\n  Diagonal distance: " (rtos diag-d 2 2)
                 " (expected ~1414.21)"))
  (command-s "_.undo" "_e")

  ;; Test 12: graph-project-point
  (princ "\n[Test 12] graph-project-point...")
  (command-s "_.undo" "_be")
  (command-s "_.line" "0,0" "1000,0" "")
  (setq ss (ssget "x" '((0 . "LINE") (8 . "0"))))
  (graph-build-from-lines ss nil)
  (setq proj (graph-project-point (list 500.0 100.0 0.0) nil nil))
  ;; point (500,100) should project to (500,0) on the line
  (if (and proj
           (= (length proj) 3)
           (< (abs (- (cadr (cadr proj)) 0.0)) 0.001)
           (< (abs (- (car (cadr proj)) 500.0)) 0.001))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  (command-s "_.undo" "_e")

  ;; Summary
  (princ (strcat "\n\n=== M01 Test Summary: "
                 (itoa passed) " passed, "
                 (itoa failed) " failed ===\n"))

  (setvar "cmdecho" old-cmdecho)
  (list passed failed)
)

;;;---------------------------------------------------------------
;;;  Provide module info
;;;---------------------------------------------------------------
(princ "\n[M01] graph_algorithm.lsp loaded.")
(princ (strcat "  Functions: graph-init, graph-build-from-lines, "
               "graph-floyd-compute, graph-get-distance, "
               "graph-project-point, graph-assign-devices"))
(princ "\n  Test: (test-M01-graph-algorithm)")
(princ)

;;;===============================================================
;;;  --- MODULE SEPARATOR ---
;;;  Previous: M01_graph_algorithm.lsp
;;;  Next: M02_line_utils.lsp
;;;===============================================================

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

;;;===============================================================
;;;  --- MODULE SEPARATOR ---
;;;  Previous: M02_line_utils.lsp
;;;  Next: M03_mline_converter.lsp
;;;===============================================================

;;;===============================================================
;;;  M03 - MLINE Converter Module
;;;  Convert MLINE entities to LINE entities with endpoint connection
;;;===============================================================
;;;  ENCODING: ANSI (ASCII only, no Chinese characters)
;;;  DEPENDENCIES: M02 (line_utils.lsp)
;;;===============================================================

;;;---------------------------------------------------------------
;;;  Global Parameters
;;;---------------------------------------------------------------
;;;  *mline-connect-threshold* - Distance threshold for connecting endpoints
(setq *mline-connect-threshold* 1400.0)

;;;---------------------------------------------------------------
;;;  mline-get-vertices
;;;  Extract vertex points from an MLINE entity
;;;  Args: ent - entity name (ename)
;;;  Returns: list of points or nil
;;;---------------------------------------------------------------
(defun mline-get-vertices (ent / elist pts i n)
  (setq elist (entget ent))
  (setq pts nil)
  (setq i 0)
  (setq n (length elist))
  ;; MLINE vertices are stored in group code 11
  (repeat n
    (if (= (car (nth i elist)) 11)
      (setq pts (cons (cdr (nth i elist)) pts))
    )
    (setq i (1+ i))
  )
  (reverse pts)
)

;;;---------------------------------------------------------------
;;;  mline-convert-to-lines
;;;  Convert MLINE to LINE segments
;;;  Args: ent - entity name
;;;         layer - target layer name (nil = current layer)
;;;  Returns: selection set of created lines
;;;---------------------------------------------------------------
(defun mline-convert-to-lines (ent layer / pts i n ss old-layer)
  (setq pts (mline-get-vertices ent))
  (setq ss (ssadd))
  (if (and pts (> (length pts) 1))
    (progn
      (setq old-layer (getvar "clayer"))
      (if layer (setvar "clayer" layer))
      (setq i 0)
      (setq n (1- (length pts)))
      (repeat n
        (command-s "_.line" (nth i pts) (nth (1+ i) pts) "")
        (setq ss (ssadd (entlast) ss))
        (setq i (1+ i))
      )
      (if layer (setvar "clayer" old-layer))
    )
  )
  ss
)

;;;---------------------------------------------------------------
;;;  mline-get-all-on-layer
;;;  Get all MLINE entities on specified layer(s)
;;;  Args: layer-list - list of layer names (single string also accepted)
;;;  Returns: selection set or nil
;;;---------------------------------------------------------------
(defun mline-get-all-on-layer (layer-list / ss filter-list)
  (if (stringp layer-list)
    (setq layer-list (list layer-list))
  )
  (setq filter-list (list (cons 0 "MLINE")))
  (if layer-list
    (setq filter-list (cons (cons 8 (apply 'strcat (mapcar '(lambda (x) (strcat x ",")) layer-list))) filter-list))
  )
  (setq ss (ssget "x" filter-list))
  ss
)

;;;---------------------------------------------------------------
;;;  mline-convert-selection
;;;  Convert all MLINEs in a selection set to LINEs
;;;  Args: mline-ss - selection set of MLINEs
;;;         target-layer - layer for new lines (nil = current)
;;;  Returns: selection set of all created lines
;;;---------------------------------------------------------------
(defun mline-convert-selection (mline-ss target-layer / i ent all-lines)
  (if mline-ss
    (progn
      (setq all-lines (ssadd))
      (setq i 0)
      (repeat (sslength mline-ss)
        (setq ent (ssname mline-ss i))
        (if ent
          (progn
            (setq new-lines (mline-convert-to-lines ent target-layer))
            (if new-lines
              (progn
                (setq j 0)
                (repeat (sslength new-lines)
                  (setq all-lines (ssadd (ssname new-lines j) all-lines))
                  (setq j (1+ j))
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )
      all-lines
    )
    nil
  )
)

;;;---------------------------------------------------------------
;;;  mline-find-nearby-lines
;;;  Find LINE entities near a given point
;;;  Args: pt - center point
;;;         radius - search radius
;;;         gllst - optional DXF filter for line selection
;;;  Returns: selection set of nearby lines
;;;---------------------------------------------------------------
(defun mline-find-nearby-lines (pt radius gllst / pt1 pt2 filter)
  (setq pt1 (polar pt (* pi 0.75) radius))
  (setq pt2 (polar pt (* pi -0.25) radius))
  (setq filter (list (cons 0 "LINE")))
  (if gllst
    (setq filter (append filter gllst))
  )
  (ssget "_c" pt1 pt2 filter)
)

;;;---------------------------------------------------------------
;;;  mline-check-intersect
;;;  Check if two lines intersect (not just touch at endpoints)
;;;  Args: ent1, ent2 - entity names
;;;  Returns: T if proper intersection, nil otherwise
;;;---------------------------------------------------------------
(defun mline-check-intersect (ent1 ent2 / pts1 pts2 int-pts ep1 ep2 tol is-endpoint-p found)
  ;; Check if two lines have a proper crossing (not just touching at endpoints)
  (setq pts1 (line-get-endpoints ent1))
  (setq pts2 (line-get-endpoints ent2))
  (setq int-pts (lines-get-intersection ent1 ent2))
  (if (null int-pts)
    nil  ; no intersection at all
    (progn
      (setq tol 1.0)
      ;; Collect all 4 endpoints
      (setq ep1 (list (car pts1) (cadr pts1)))
      (setq ep2 (list (car pts2) (cadr pts2)))
      ;; Check if any intersection point is NOT at an endpoint
      ;; If so, the lines properly cross each other
      (setq found nil)
      (foreach ipt int-pts
        ;; Check if this intersection is near any endpoint
        (if (and (not (< (distance ipt (car pts1)) tol))
                 (not (< (distance ipt (cadr pts1)) tol))
                 (not (< (distance ipt (car pts2)) tol))
                 (not (< (distance ipt (cadr pts2)) tol)))
          (setq found T)  ; intersection is NOT at any endpoint = proper crossing
        )
      )
      found
    )
  )
)

;;;---------------------------------------------------------------
;;;  mline-connect-endpoint
;;;  Connect a line endpoint to nearby line endpoints if within threshold
;;;  Args: line-ent - line entity
;;;         endpoint - 'start or 'end
;;;         threshold - distance threshold
;;;         gllst - filter for line selection
;;;  Returns: T if connection made, nil otherwise
;;;---------------------------------------------------------------
(defun mline-connect-endpoint (line-ent endpoint threshold gllst / pt nearby-lines i near-ent near-pts closest-pt min-dist closest-ent)
  (setq pt (if (= endpoint 'start)
             (line-get-startpoint line-ent)
             (line-get-endpoint line-ent)
           ))
  (setq nearby-lines (mline-find-nearby-lines pt threshold gllst))
  (if nearby-lines
    (progn
      ;; Remove self from selection
      (setq nearby-lines (ssdel line-ent nearby-lines))
      (if (> (sslength nearby-lines) 0)
        (progn
          (setq min-dist threshold)
          (setq closest-pt nil)
          (setq i 0)
          (repeat (sslength nearby-lines)
            (setq near-ent (ssname nearby-lines i))
            (setq near-pts (line-get-endpoints near-ent))
            ;; Check both endpoints of nearby line
            (foreach np near-pts
              (setq d (distance pt np))
              (if (< d min-dist)
                (progn
                  (setq min-dist d)
                  (setq closest-pt np)
                  (setq closest-ent near-ent)
                )
              )
            )
            (setq i (1+ i))
          )
          ;; If found close endpoint and no proper intersection, create connection
          (if (and closest-pt
                   (not (mline-check-intersect line-ent closest-ent)))
            (progn
              ;; Get closest point on the nearby line
              (setq proj-pt (line-get-closest-point closest-ent pt))
              (if proj-pt
                (progn
                  ;; Create connection line
                  (command-s "_.line" pt proj-pt "")
                  ;; Check if zero length and delete if so
                  (setq new-ent (entlast))
                  (if (= 0 (line-get-length new-ent))
                    (command-s "_.erase" new-ent "")
                  )
                  T
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
    nil
  )
)

;;;---------------------------------------------------------------
;;;  mline-connect-all-endpoints
;;;  Connect all endpoints of lines in selection set to nearby lines
;;;  Args: line-ss - selection set of lines
;;;         threshold - connection distance threshold
;;;         gllst - filter for line selection
;;;  Returns: number of connections made
;;;---------------------------------------------------------------
(defun mline-connect-all-endpoints (line-ss threshold gllst / i ent count)
  (setq count 0)
  (if line-ss
    (progn
      (setq i 0)
      (repeat (sslength line-ss)
        (setq ent (ssname line-ss i))
        (if ent
          (progn
            ;; Try to connect start point
            (if (mline-connect-endpoint ent 'start threshold gllst)
              (setq count (1+ count))
            )
            ;; Try to connect end point
            (if (mline-connect-endpoint ent 'end threshold gllst)
              (setq count (1+ count))
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )
  count
)

;;;---------------------------------------------------------------
;;;  mline-process-all
;;;  Main function: Convert MLINEs to LINEs and connect endpoints
;;;  Args: mline-layer-list - list of MLINE layer names
;;;         target-layer - layer for new lines
;;;         connect-threshold - endpoint connection threshold (nil = use default)
;;;         gllst - filter for line selection during connection
;;;  Returns: selection set of all created lines
;;;---------------------------------------------------------------
(defun mline-process-all (mline-layer-list target-layer connect-threshold gllst / mline-ss all-lines count)
  (princ (strcat "\n[mline] Processing MLINEs on layers: " 
                 (if (listp mline-layer-list) 
                   (apply 'strcat (mapcar '(lambda (x) (strcat x ",")) mline-layer-list))
                   mline-layer-list)))
  
  ;; Get MLINEs
  (setq mline-ss (mline-get-all-on-layer mline-layer-list))
  
  (if (null mline-ss)
    (progn
      (princ "\n[mline] No MLINEs found.")
      nil
    )
    (progn
      ;; Convert to lines
      (princ (strcat "\n[mline] Converting " (itoa (sslength mline-ss)) " MLINEs..."))
      (setq all-lines (mline-convert-selection mline-ss target-layer))
      
      ;; Connect endpoints
      (if (null connect-threshold)
        (setq connect-threshold *mline-connect-threshold*)
      )
      (princ (strcat "\n[mline] Connecting endpoints within " (rtos connect-threshold 2 0) " units..."))
      (setq count (mline-connect-all-endpoints all-lines connect-threshold gllst))
      (princ (strcat "\n[mline] Created " (itoa count) " connection lines."))
      
      (princ (strcat "\n[mline] Done. Total lines: " (itoa (sslength all-lines))))
      all-lines
    )
  )
)

;;;---------------------------------------------------------------
;;;  mline-set-threshold
;;;  Set the default connection threshold
;;;---------------------------------------------------------------
(defun mline-set-threshold (val)
  (setq *mline-connect-threshold* val)
)

;;;---------------------------------------------------------------
;;;  mline-get-threshold
;;;  Get the current connection threshold
;;;---------------------------------------------------------------
(defun mline-get-threshold ()
  *mline-connect-threshold*
)

;;;===============================================================
;;;  TEST FUNCTIONS
;;;===============================================================

(defun test-M03-mline-converter (/ passed failed old-cmdecho
                                   ml-ent pts lines ss count)
  (setq passed 0 failed 0)
  (setq old-cmdecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)

  (princ "\n\n=== M03 MLINE Converter Tests ===")

  (command-s "_.undo" "_be")

  ;; Test 1: mline-get-vertices
  (princ "\n[Test 1] mline-get-vertices...")
  ;; Create MLINE with 3 vertices
  (command-s "_.mline" "0,0" "1000,0" "1000,1000" "")
  (setq ml-ent (entlast))
  (setq pts (mline-get-vertices ml-ent))
  (if (and pts (= (length pts) 3))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  (command-s "_.erase" ml-ent "")

  ;; Test 2: mline-convert-to-lines
  (princ "\n[Test 2] mline-convert-to-lines...")
  (command-s "_.mline" "0,0" "1000,0" "1000,1000" "")
  (setq ml-ent (entlast))
  (setq lines (mline-convert-to-lines ml-ent nil))
  (if (and lines (= (sslength lines) 2))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  (command-s "_.erase" ml-ent "")
  (if lines
    (progn
      (setq i 0)
      (repeat (sslength lines)
        (command-s "_.erase" (ssname lines i) "")
        (setq i (1+ i))
      )
    )
  )

  ;; Test 3: mline-get-all-on-layer
  (princ "\n[Test 3] mline-get-all-on-layer...")
  (command-s "_.mline" "0,0" "500,0" "")
  (setq ml-ent (entlast))
  (setq ss (mline-get-all-on-layer "0"))
  (if (and ss (> (sslength ss) 0))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  (command-s "_.erase" ml-ent "")

  ;; Test 4: mline-find-nearby-lines
  (princ "\n[Test 4] mline-find-nearby-lines...")
  (command-s "_.line" "0,0" "1000,0" "")
  (setq line-ent (entlast))
  (setq ss (mline-find-nearby-lines (list 500.0 100.0 0.0) 200 nil))
  (if (and ss (> (sslength ss) 0))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  (command-s "_.erase" line-ent "")

  ;; Test 5: mline-connect-endpoint
  (princ "\n[Test 5] mline-connect-endpoint...")
  ;; Create two lines close but not touching
  (command-s "_.line" "0,0" "1000,0" "")
  (setq line1 (entlast))
  (command-s "_.line" "1000,500" "1000,1000" "")  ; 500 units away
  (setq line2 (entlast))
  ;; Try to connect with large threshold
  (setq result (mline-connect-endpoint line1 'end 600 nil))
  (if result
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  (command-s "_.erase" line1 "")
  (command-s "_.erase" line2 "")

  ;; Test 6: mline-process-all (integration)
  (princ "\n[Test 6] mline-process-all...")
  (command-s "_.mline" "0,0" "1000,0" "1000,1000" "")
  (setq ml-ent (entlast))
  (setq all-lines (mline-process-all "0" "0" 100 nil))
  (if (and all-lines (> (sslength all-lines) 0))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  ;; Cleanup
  (if all-lines
    (progn
      (setq i 0)
      (repeat (sslength all-lines)
        (command-s "_.erase" (ssname all-lines i) "")
        (setq i (1+ i))
      )
    )
  )
  (command-s "_.erase" ml-ent "")

  ;; Test 7: threshold get/set
  (princ "\n[Test 7] threshold get/set...")
  (setq old-thresh (mline-get-threshold))
  (mline-set-threshold 2000.0)
  (if (= (mline-get-threshold) 2000.0)
    (progn 
      (princ " PASS") 
      (setq passed (1+ passed))
      (mline-set-threshold old-thresh)  ; restore
    )
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  (command-s "_.undo" "_e")

  ;; Summary
  (princ (strcat "\n\n=== M03 Test Summary: "
                 (itoa passed) " passed, "
                 (itoa failed) " failed ===\n"))

  (setvar "cmdecho" old-cmdecho)
  (list passed failed)
)

;;;---------------------------------------------------------------
;;;  Provide module info
;;;---------------------------------------------------------------
(princ "\n[M03] mline_converter.lsp loaded.")
(princ (strcat "  Functions: mline-get-vertices, mline-convert-to-lines, "
               "mline-connect-all-endpoints, mline-process-all"))
(princ (strcat "\n  Default threshold: " (rtos *mline-connect-threshold* 2 0)))
(princ "\n  Test: (test-M03-mline-converter)")
(princ)

;;;===============================================================
;;;  --- MODULE SEPARATOR ---
;;;  Previous: M03_mline_converter.lsp
;;;  Next: M04_duplicate_remover.lsp
;;;===============================================================

;;;===============================================================
;;;  M04 - Duplicate Remover Module
;;;  Remove duplicate and overlapping line entities
;;;===============================================================
;;;  ENCODING: ANSI (ASCII only, no Chinese characters)
;;;  DEPENDENCIES: M02 (line_utils.lsp)
;;;===============================================================

;;;---------------------------------------------------------------
;;;  Global Parameters
;;;---------------------------------------------------------------
;;;  *dup-tolerance* - Distance tolerance for duplicate detection
(setq *dup-tolerance* 1.0)

;;;---------------------------------------------------------------
;;;  dup-line-get-key
;;;  Generate a sort key for a line (slope, intercept, min-x, min-y)
;;;  Args: ent - entity name
;;;  Returns: (slope intercept min-x min-y) or nil
;;;---------------------------------------------------------------
(defun dup-line-get-key (ent / pts x1 y1 x2 y2 dx dy slope intercept min-x min-y)
  (setq pts (line-get-endpoints ent))
  (if pts
    (progn
      (setq x1 (car (car pts)) y1 (cadr (car pts)))
      (setq x2 (car (cadr pts)) y2 (cadr (cadr pts)))
      (setq min-x (min x1 x2))
      (setq min-y (min y1 y2))
      (setq dx (- x2 x1))
      (setq dy (- y2 y1))
      ;; Handle vertical lines
      (if (< (abs dx) 1e-10)
        (list nil x1 min-x min-y)  ; nil slope = vertical
        (progn
          (setq slope (/ dy dx))
          (setq intercept (- y1 (* slope x1)))
          (list slope intercept min-x min-y)
        )
      )
    )
    nil
  )
)

;;;---------------------------------------------------------------
;;;  dup-lines-colinear-p
;;;  Check if two lines are colinear (same slope and intercept)
;;;  Args: ent1, ent2 - entity names
;;;  Returns: T or nil
;;;---------------------------------------------------------------
(defun dup-lines-colinear-p (ent1 ent2 / key1 key2)
  (setq key1 (dup-line-get-key ent1))
  (setq key2 (dup-line-get-key ent2))
  (if (and key1 key2)
    (progn
      ;; Check slope (handle nil for vertical)
      (if (or (and (null (car key1)) (null (car key2)))
              (and (car key1) (car key2) 
                   (< (abs (- (car key1) (car key2))) 0.001)))
        ;; Check intercept
        (if (or (and (null (car key1)) (null (car key2)))
                (< (abs (- (cadr key1) (cadr key2))) *dup-tolerance*))
          T
          nil
        )
        nil
      )
    )
    nil
  )
)

;;;---------------------------------------------------------------
;;;  dup-line-bounding-box
;;;  Get bounding box of a line (min-x min-y max-x max-y)
;;;  Args: ent - entity name
;;;  Returns: (min-x min-y max-x max-y)
;;;---------------------------------------------------------------
(defun dup-line-bounding-box (ent / pts x1 y1 x2 y2)
  (setq pts (line-get-endpoints ent))
  (if pts
    (progn
      (setq x1 (car (car pts)) y1 (cadr (car pts)))
      (setq x2 (car (cadr pts)) y2 (cadr (cadr pts)))
      (list (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2))
    )
    nil
  )
)

;;;---------------------------------------------------------------
;;;  dup-boxes-overlap-p
;;;  Check if two bounding boxes overlap
;;;  Args: box1, box2 - bounding boxes
;;;  Returns: T or nil
;;;---------------------------------------------------------------
(defun dup-boxes-overlap-p (box1 box2 / tol)
  (setq tol *dup-tolerance*)
  (if (and box1 box2)
    (not (or (< (nth 2 box1) (- (nth 0 box2) tol))  ; box1 right < box2 left
             (< (nth 2 box2) (- (nth 0 box1) tol))  ; box2 right < box1 left
             (< (nth 3 box1) (- (nth 1 box2) tol))  ; box1 top < box2 bottom
             (< (nth 3 box2) (- (nth 1 box1) tol)))) ; box2 top < box1 bottom
    nil
  )
)

;;;---------------------------------------------------------------
;;;  dup-lines-overlap-p
;;;  Check if two colinear lines overlap
;;;  Args: ent1, ent2 - entity names (assumed colinear)
;;;  Returns: T or nil
;;;---------------------------------------------------------------
(defun dup-lines-overlap-p (ent1 ent2 / box1 box2)
  (setq box1 (dup-line-bounding-box ent1))
  (setq box2 (dup-line-bounding-box ent2))
  (dup-boxes-overlap-p box1 box2)
)

;;;---------------------------------------------------------------
;;;  dup-merge-two-lines
;;;  Merge two overlapping colinear lines into one
;;;  Args: ent1, ent2 - entity names
;;;  Returns: entity name of merged line (or nil)
;;;---------------------------------------------------------------
(defun dup-merge-two-lines (ent1 ent2 / pts1 pts2 all-pts min-x max-x min-y max-y new-ent)
  (setq pts1 (line-get-endpoints ent1))
  (setq pts2 (line-get-endpoints ent2))
  (if (and pts1 pts2)
    (progn
      ;; Collect all 4 endpoints
      (setq all-pts (append pts1 pts2))
      ;; Find extreme points
      (setq min-x (apply 'min (mapcar 'car all-pts)))
      (setq max-x (apply 'max (mapcar 'car all-pts)))
      (setq min-y (apply 'min (mapcar 'cadr all-pts)))
      (setq max-y (apply 'max (mapcar 'cadr all-pts)))
      ;; Determine orientation from original line
      (setq dx1 (- (car (cadr pts1)) (car (car pts1))))
      (setq dy1 (- (cadr (cadr pts1)) (cadr (car pts1))))
      ;; Create new line based on orientation
      (if (> (abs dx1) (abs dy1))
        ;; More horizontal: use x extremes
        (setq new-ent (line-create (list min-x (cadr (car pts1)) 0.0)
                                   (list max-x (cadr (car pts1)) 0.0)))
        ;; More vertical or diagonal: use y extremes
        (setq new-ent (line-create (list (car (car pts1)) min-y 0.0)
                                   (list (car (car pts1)) max-y 0.0)))
      )
      ;; Delete original lines
      (entdel ent1)
      (entdel ent2)
      new-ent
    )
    nil
  )
)

;;;---------------------------------------------------------------
;;;  dup-lines-identical-p
;;;  Check if two lines are identical (same endpoints)
;;;  Args: ent1, ent2 - entity names
;;;  Returns: T or nil
;;;---------------------------------------------------------------
(defun dup-lines-identical-p (ent1 ent2 / pts1 pts2 tol)
  (setq pts1 (line-get-endpoints ent1))
  (setq pts2 (line-get-endpoints ent2))
  (setq tol *dup-tolerance*)
  (if (and pts1 pts2)
    (or
      ;; Same direction
      (and (< (distance (car pts1) (car pts2)) tol)
           (< (distance (cadr pts1) (cadr pts2)) tol))
      ;; Reversed direction
      (and (< (distance (car pts1) (cadr pts2)) tol)
           (< (distance (cadr pts1) (car pts2)) tol))
    )
    nil
  )
)

;;;---------------------------------------------------------------
;;;  dup-remove-identical
;;;  Remove identical lines from selection set
;;;  Args: line-ss - selection set
;;;  Returns: number of duplicates removed
;;;---------------------------------------------------------------
(defun dup-remove-identical (line-ss / i j ent1 ent2 count to-remove)
  (setq count 0)
  (setq to-remove nil)
  (if line-ss
    (progn
      (setq i 0)
      (repeat (sslength line-ss)
        (setq ent1 (ssname line-ss i))
        (if (and ent1 (not (member ent1 to-remove)))
          (progn
            (setq j (1+ i))
            (repeat (- (sslength line-ss) (1+ i))
              (setq ent2 (ssname line-ss j))
              (if (and ent2 (not (member ent2 to-remove)))
                (if (dup-lines-identical-p ent1 ent2)
                  (progn
                    (setq to-remove (cons ent2 to-remove))
                    (setq count (1+ count))
                  )
                )
              )
              (setq j (1+ j))
            )
          )
        )
        (setq i (1+ i))
      )
      ;; Delete marked entities
      (foreach ent to-remove
        (if (entget ent) (entdel ent))
      )
    )
  )
  count
)

;;;---------------------------------------------------------------
;;;  dup-merge-colinear
;;;  Merge overlapping colinear lines
;;;  Args: line-ss - selection set
;;;  Returns: number of merges performed
;;;---------------------------------------------------------------
(defun dup-merge-colinear (line-ss / line-list i j ent1 ent2 count merged)
  (setq count 0)
  (setq merged nil)
  (if line-ss
    (progn
      ;; Convert to list for easier manipulation
      (setq line-list nil)
      (setq i 0)
      (repeat (sslength line-ss)
        (setq line-list (cons (ssname line-ss i) line-list))
        (setq i (1+ i))
      )
      (setq line-list (reverse line-list))
      ;; Check each pair
      (setq i 0)
      (while (< i (length line-list))
        (setq ent1 (nth i line-list))
        (if (and ent1 (entget ent1) (not (member ent1 merged)))
          (progn
            (setq j (1+ i))
            (while (< j (length line-list))
              (setq ent2 (nth j line-list))
              (if (and ent2 (entget ent2) (not (member ent2 merged)))
                (if (and (dup-lines-colinear-p ent1 ent2)
                         (dup-lines-overlap-p ent1 ent2))
                  (progn
                    ;; Merge lines
                    (setq new-ent (dup-merge-two-lines ent1 ent2))
                    (if new-ent
                      (progn
                        (setq merged (cons ent1 merged))
                        (setq merged (cons ent2 merged))
                        ;; Replace in list with new entity
                        (setq line-list (subst new-ent ent1 line-list))
                        (setq ent1 new-ent)
                        (setq count (1+ count))
                      )
                    )
                  )
                )
              )
              (setq j (1+ j))
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )
  count
)

;;;---------------------------------------------------------------
;;;  dup-remove-all
;;;  Main function: Remove all duplicates and merge overlaps
;;;  Args: line-ss - selection set (nil = all lines on layer)
;;;         layer - layer name (if line-ss is nil)
;;;  Returns: (identical-removed colinear-merged)
;;;---------------------------------------------------------------
(defun dup-remove-all (line-ss layer / ss identical-count colinear-count)
  (princ "\n[dup] Removing duplicate lines...")
  
  ;; Get lines if not provided
  (if (null line-ss)
    (if layer
      (setq ss (ssget "x" (list (cons 0 "LINE") (cons 8 layer))))
      (setq ss (ssget "x" '((0 . "LINE"))))
    )
    (setq ss line-ss)
  )
  
  (if (null ss)
    (progn
      (princ "\n[dup] No lines found.")
      (list 0 0)
    )
    (progn
      (princ (strcat "\n[dup] Processing " (itoa (sslength ss)) " lines..."))
      
      ;; Step 1: Remove identical lines
      (setq identical-count (dup-remove-identical ss))
      (princ (strcat "\n[dup] Removed " (itoa identical-count) " identical lines."))
      
      ;; Refresh selection set
      (if layer
        (setq ss (ssget "x" (list (cons 0 "LINE") (cons 8 layer))))
        (setq ss (ssget "x" '((0 . "LINE"))))
      )
      
      ;; Step 2: Merge colinear overlapping lines
      (setq colinear-count (dup-merge-colinear ss))
      (princ (strcat "\n[dup] Merged " (itoa colinear-count) " colinear lines."))
      
      (princ "\n[dup] Done.")
      (list identical-count colinear-count)
    )
  )
)

;;;---------------------------------------------------------------
;;;  dup-set-tolerance
;;;  Set the duplicate detection tolerance
;;;---------------------------------------------------------------
(defun dup-set-tolerance (val)
  (setq *dup-tolerance* val)
)

;;;---------------------------------------------------------------
;;;  dup-get-tolerance
;;;  Get the current tolerance
;;;---------------------------------------------------------------
(defun dup-get-tolerance ()
  *dup-tolerance*
)

;;;===============================================================
;;;  TEST FUNCTIONS
;;;===============================================================

(defun test-M04-duplicate-remover (/ passed failed old-cmdecho
                                     ent1 ent2 ent3 ent4 count result)
  (setq passed 0 failed 0)
  (setq old-cmdecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)

  (princ "\n\n=== M04 Duplicate Remover Tests ===")

  (command-s "_.undo" "_be")

  ;; Test 1: dup-line-get-key
  (princ "\n[Test 1] dup-line-get-key...")
  (command-s "_.line" "0,0" "1000,0" "")
  (setq ent1 (entlast))
  (setq key (dup-line-get-key ent1))
  (if (and key
           (< (abs (- (car key) 0.0)) 0.001)    ; slope = 0
           (< (abs (- (cadr key) 0.0)) 0.001))  ; intercept = 0
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  (command-s "_.erase" ent1 "")

  ;; Test 2: dup-lines-identical-p
  (princ "\n[Test 2] dup-lines-identical-p...")
  (command-s "_.line" "0,0" "1000,0" "")
  (setq ent1 (entlast))
  (command-s "_.line" "0,0" "1000,0" "")  ; identical
  (setq ent2 (entlast))
  (command-s "_.line" "1000,0" "0,0" "")  ; reversed
  (setq ent3 (entlast))
  (command-s "_.line" "0,0" "1000,100" "")  ; different
  (setq ent4 (entlast))
  (if (and (dup-lines-identical-p ent1 ent2)
           (dup-lines-identical-p ent1 ent3)
           (null (dup-lines-identical-p ent1 ent4)))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  (command-s "_.erase" ent1 "")
  (command-s "_.erase" ent2 "")
  (command-s "_.erase" ent3 "")
  (command-s "_.erase" ent4 "")

  ;; Test 3: dup-lines-colinear-p
  (princ "\n[Test 3] dup-lines-colinear-p...")
  (command-s "_.line" "0,0" "1000,0" "")
  (setq ent1 (entlast))
  (command-s "_.line" "500,0" "1500,0" "")  ; colinear
  (setq ent2 (entlast))
  (command-s "_.line" "0,0" "1000,100" "")  ; not colinear
  (setq ent3 (entlast))
  (if (and (dup-lines-colinear-p ent1 ent2)
           (null (dup-lines-colinear-p ent1 ent3)))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  (command-s "_.erase" ent1 "")
  (command-s "_.erase" ent2 "")
  (command-s "_.erase" ent3 "")

  ;; Test 4: dup-lines-overlap-p
  (princ "\n[Test 4] dup-lines-overlap-p...")
  (command-s "_.line" "0,0" "1000,0" "")
  (setq ent1 (entlast))
  (command-s "_.line" "500,0" "1500,0" "")  ; overlaps
  (setq ent2 (entlast))
  (command-s "_.line" "1100,0" "2000,0" "")  ; does not overlap
  (setq ent3 (entlast))
  (if (and (dup-lines-overlap-p ent1 ent2)
           (null (dup-lines-overlap-p ent1 ent3)))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  (command-s "_.erase" ent1 "")
  (command-s "_.erase" ent2 "")
  (command-s "_.erase" ent3 "")

  ;; Test 5: dup-remove-identical
  (princ "\n[Test 5] dup-remove-identical...")
  (command-s "_.line" "0,0" "1000,0" "")
  (setq ent1 (entlast))
  (command-s "_.line" "0,0" "1000,0" "")  ; duplicate
  (setq ent2 (entlast))
  (command-s "_.line" "0,0" "1000,0" "")  ; another duplicate
  (setq ent3 (entlast))
  (setq ss (ssadd))
  (setq ss (ssadd ent1 ss))
  (setq ss (ssadd ent2 ss))
  (setq ss (ssadd ent3 ss))
  (setq count (dup-remove-identical ss))
  (if (= count 2)  ; should remove 2 duplicates
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  ;; Cleanup remaining line
  (if (entget ent1) (command-s "_.erase" ent1 ""))

  ;; Test 6: dup-merge-colinear
  (princ "\n[Test 6] dup-merge-colinear...")
  (command-s "_.line" "0,0" "1000,0" "")
  (setq ent1 (entlast))
  (command-s "_.line" "500,0" "1500,0" "")  ; overlaps
  (setq ent2 (entlast))
  (setq ss (ssadd))
  (setq ss (ssadd ent1 ss))
  (setq ss (ssadd ent2 ss))
  (setq count (dup-merge-colinear ss))
  ;; After merge, should have 1 line spanning 0-1500
  (setq remaining-ss (ssget "x" '((0 . "LINE"))))
  (if (and (= count 1)
           remaining-ss
           (= (sslength remaining-ss) 1)
           (> (line-get-length (ssname remaining-ss 0)) 1400))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  (if remaining-ss (command-s "_.erase" (ssname remaining-ss 0) ""))

  ;; Test 7: dup-remove-all (integration)
  (princ "\n[Test 7] dup-remove-all...")
  (command-s "_.line" "0,0" "1000,0" "")
  (setq ent1 (entlast))
  (command-s "_.line" "0,0" "1000,0" "")  ; identical
  (setq ent2 (entlast))
  (command-s "_.line" "500,0" "1500,0" "")  ; overlapping
  (setq ent3 (entlast))
  (setq result (dup-remove-all nil "0"))
  (if (and (= (car result) 1)  ; 1 identical removed
           (= (cadr result) 1)) ; 1 merge performed
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  ;; Cleanup
  (setq ss (ssget "x" '((0 . "LINE"))))
  (if ss (command-s "_.erase" ss ""))

  ;; Test 8: tolerance get/set
  (princ "\n[Test 8] tolerance get/set...")
  (setq old-tol (dup-get-tolerance))
  (dup-set-tolerance 5.0)
  (if (= (dup-get-tolerance) 5.0)
    (progn 
      (princ " PASS") 
      (setq passed (1+ passed))
      (dup-set-tolerance old-tol)
    )
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  (command-s "_.undo" "_e")

  ;; Summary
  (princ (strcat "\n\n=== M04 Test Summary: "
                 (itoa passed) " passed, "
                 (itoa failed) " failed ===\n"))

  (setvar "cmdecho" old-cmdecho)
  (list passed failed)
)

;;;---------------------------------------------------------------
;;;  Provide module info
;;;---------------------------------------------------------------
(princ "\n[M04] duplicate_remover.lsp loaded.")
(princ (strcat "  Functions: dup-remove-identical, dup-merge-colinear, "
               "dup-remove-all, dup-lines-identical-p"))
(princ (strcat "\n  Default tolerance: " (rtos *dup-tolerance* 2 2)))
(princ "\n  Test: (test-M04-duplicate-remover)")
(princ)

;;;===============================================================
;;;  --- MODULE SEPARATOR ---
;;;  Previous: M04_duplicate_remover.lsp
;;;  Next: M05_break_lines.lsp
;;;===============================================================

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

;;;===============================================================
;;;  --- MODULE SEPARATOR ---
;;;  Previous: M05_break_lines.lsp
;;;  Next: M06_block_utils.lsp
;;;===============================================================

;;;===============================================================
;;;  M06 - Block Utilities Module
;;;  Block-related utility functions
;;;===============================================================
;;;  ENCODING: ANSI (ASCII only, no Chinese characters)
;;;  DEPENDENCIES: None
;;;===============================================================

;;;---------------------------------------------------------------
;;;  Global Parameters
;;;---------------------------------------------------------------
;;;  *block-name-search-radius* - Radius for searching text near block
(setq *block-name-search-radius* 3000.0)

;;;---------------------------------------------------------------
;;;  block-get-insertion-point
;;;  Get the insertion point of a block reference
;;;  Args: ent - entity name (block reference)
;;;  Returns: point list or nil
;;;---------------------------------------------------------------
(defun block-get-insertion-point (ent / elist)
  (setq elist (entget ent))
  (if (= (cdr (assoc 0 elist)) "INSERT")
    (cdr (assoc 10 elist))
    nil
  )
)

;;;---------------------------------------------------------------
;;;  block-get-name
;;;  Get the block definition name
;;;  Args: ent - entity name (block reference)
;;;  Returns: string block name or nil
;;;---------------------------------------------------------------
(defun block-get-name (ent / elist)
  (setq elist (entget ent))
  (if (= (cdr (assoc 0 elist)) "INSERT")
    (cdr (assoc 2 elist))
    nil
  )
)

;;;---------------------------------------------------------------
;;;  block-get-layer
;;;  Get the layer of a block reference
;;;  Args: ent - entity name
;;;  Returns: string layer name
;;;---------------------------------------------------------------
(defun block-get-layer (ent / elist)
  (setq elist (entget ent))
  (cdr (assoc 8 elist))
)

;;;---------------------------------------------------------------
;;;  block-get-scale
;;;  Get the scale factors of a block reference
;;;  Args: ent - entity name
;;;  Returns: (x-scale y-scale z-scale) or nil
;;;---------------------------------------------------------------
(defun block-get-scale (ent / elist)
  (setq elist (entget ent))
  (if (= (cdr (assoc 0 elist)) "INSERT")
    (list
      (cdr (assoc 41 elist))
      (cdr (assoc 42 elist))
      (cdr (assoc 43 elist))
    )
    nil
  )
)

;;;---------------------------------------------------------------
;;;  block-get-rotation
;;;  Get the rotation angle of a block reference
;;;  Args: ent - entity name
;;;  Returns: float angle in radians
;;;---------------------------------------------------------------
(defun block-get-rotation (ent / elist)
  (setq elist (entget ent))
  (if (= (cdr (assoc 0 elist)) "INSERT")
    (cdr (assoc 50 elist))
    0.0
  )
)

;;;---------------------------------------------------------------
;;;  block-get-definition
;;;  Get the block definition (entity data) of a block
;;;  Args: block-name - string block name
;;;  Returns: entity data list or nil
;;;---------------------------------------------------------------
(defun block-get-definition (block-name / blk)
  (setq blk (tblsearch "BLOCK" block-name))
  blk
)

;;;---------------------------------------------------------------
;;;  block-get-entities
;;;  Get all entities within a block definition
;;;  Args: block-name - string block name
;;;  Returns: list of entity names or nil
;;;---------------------------------------------------------------
(defun block-get-entities (block-name / blk first-ent result)
  (setq blk (block-get-definition block-name))
  (if blk
    (progn
      (setq first-ent (cdr (assoc -2 blk)))
      (setq result nil)
      (while first-ent
        (setq result (cons first-ent result))
        (setq first-ent (entnext first-ent))
      )
      (reverse result)
    )
    nil
  )
)

;;;---------------------------------------------------------------
;;;  block-entity-get-area
;;;  Calculate area of an entity (for finding largest entity)
;;;  Args: ent - entity name
;;;  Returns: float area or 0
;;;---------------------------------------------------------------
(defun block-entity-get-area (ent / elist etype obj area)
  (setq elist (entget ent))
  (setq etype (cdr (assoc 0 elist)))
  (cond
    ((= etype "CIRCLE")
     (setq r (cdr (assoc 40 elist)))
     (* pi r r)
    )
    ((= etype "LWPOLYLINE")
     (vl-load-com)
     (setq obj (vlax-ename->vla-object ent))
     (if (= (vla-get-closed obj) :vlax-true)
       (vlax-get obj 'area)
       0.0
     )
    )
    ((or (= etype "POLYLINE") (= etype "REGION") (= etype "SOLID"))
     (vl-load-com)
     (setq obj (vlax-ename->vla-object ent))
     (vl-catch-all-apply '(lambda () (vlax-get obj 'area)))
    )
    (T 0.0)
  )
)

;;;---------------------------------------------------------------
;;;  block-entity-is-text-p
;;;  Check if entity is a text entity
;;;  Args: ent - entity name
;;;  Returns: T or nil
;;;---------------------------------------------------------------
(defun block-entity-is-text-p (ent / elist etype)
  (setq elist (entget ent))
  (setq etype (cdr (assoc 0 elist)))
  (or (= etype "TEXT")
      (= etype "MTEXT")
      (= etype "ATTDEF")
      (= etype "ATTRIB"))
)

;;;---------------------------------------------------------------
;;;  block-find-largest-entity
;;;  Find the largest non-text entity in a block
;;;  Args: block-name - string block name
;;;  Returns: entity name of largest entity or nil
;;;---------------------------------------------------------------
(defun block-find-largest-entity (block-name / entities max-area max-ent area)
  (setq entities (block-get-entities block-name))
  (setq max-area 0.0)
  (setq max-ent nil)
  (foreach ent entities
    (if (not (block-entity-is-text-p ent))
      (progn
        (setq area (block-entity-get-area ent))
        (if (> area max-area)
          (progn
            (setq max-area area)
            (setq max-ent ent)
          )
        )
      )
    )
  )
  max-ent
)

;;;---------------------------------------------------------------
;;;  block-entity-get-center
;;;  Get the geometric center of an entity
;;;  Args: ent - entity name
;;;  Returns: point list or nil
;;;---------------------------------------------------------------
(defun block-entity-get-center (ent / elist etype obj minpt maxpt)
  (setq elist (entget ent))
  (setq etype (cdr (assoc 0 elist)))
  (cond
    ((= etype "CIRCLE")
     (cdr (assoc 10 elist))
    )
    ((= etype "LINE")
     (list (/ (+ (car (cdr (assoc 10 elist))) (car (cdr (assoc 11 elist)))) 2.0)
           (/ (+ (cadr (cdr (assoc 10 elist))) (cadr (cdr (assoc 11 elist)))) 2.0)
           0.0)
    )
    ((or (= etype "LWPOLYLINE") (= etype "POLYLINE"))
     (vl-load-com)
     (setq obj (vlax-ename->vla-object ent))
     (vlax-get obj 'Centroid)
    )
    (T
     (vl-load-com)
     (setq obj (vlax-ename->vla-object ent))
     (vlax-get obj 'InsertionPoint)
    )
  )
)

;;;---------------------------------------------------------------
;;;  block-get-base-point
;;;  Get the base point of a block reference
;;;  Finds the center of the largest non-text entity in the block
;;;  Args: ent - entity name (block reference)
;;;  Returns: point list or nil
;;;---------------------------------------------------------------
(defun block-get-base-point (ent / block-name largest-ent center insertion scale rotation)
  (setq block-name (block-get-name ent))
  (if block-name
    (progn
      (setq largest-ent (block-find-largest-entity block-name))
      (if largest-ent
        (progn
          (setq center (block-entity-get-center largest-ent))
          (setq insertion (block-get-insertion-point ent))
          (setq scale (block-get-scale ent))
          (setq rotation (block-get-rotation ent))
          ;; Transform center to world coordinates
          (block-transform-point center insertion scale rotation)
        )
        ;; Fallback: use insertion point
        (block-get-insertion-point ent)
      )
    )
    nil
  )
)

;;;---------------------------------------------------------------
;;;  block-transform-point
;;;  Transform a point from block local to world coordinates
;;;  Args: pt        - point in block local coordinates
;;;         insertion - block insertion point
;;;         scale     - (x-scale y-scale z-scale)
;;;         rotation  - rotation angle in radians
;;;  Returns: transformed point
;;;---------------------------------------------------------------
(defun block-transform-point (pt insertion scale rotation / x y sx sy)
  (setq x (car pt))
  (setq y (cadr pt))
  (setq sx (car scale))
  (setq sy (cadr scale))
  ;; Apply scale
  (setq x (* x sx))
  (setq y (* y sy))
  ;; Apply rotation
  (if (/= rotation 0.0)
    (progn
      (setq cos-r (cos rotation))
      (setq sin-r (sin rotation))
      (setq x-new (- (* x cos-r) (* y sin-r)))
      (setq y-new (+ (* x sin-r) (* y cos-r)))
      (setq x x-new)
      (setq y y-new)
    )
  )
  ;; Apply translation
  (list (+ x (car insertion))
        (+ y (cadr insertion))
        (+ (caddr pt) (caddr insertion)))
)

;;;---------------------------------------------------------------
;;;  block-find-nearest-text
;;;  Find the nearest text entity to a point
;;;  Args: pt     - center point
;;;         radius - search radius
;;;  Returns: (text-entity text-content distance) or nil
;;;---------------------------------------------------------------
(defun block-find-nearest-text (pt radius / pt1 pt2 ss i ent content min-dist nearest)
  (setq pt1 (polar pt (* pi 0.75) radius))
  (setq pt2 (polar pt (* pi -0.25) radius))
  (setq ss (ssget "_c" pt1 pt2 '((0 . "TEXT,MTEXT"))))
  (if ss
    (progn
      (setq min-dist radius)
      (setq nearest nil)
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (if ent
          (progn
            (setq content (block-text-get-content ent))
            (if content
              (progn
                (setq dist (distance pt (block-text-get-position ent)))
                (if (< dist min-dist)
                  (progn
                    (setq min-dist dist)
                    (setq nearest (list ent content dist))
                  )
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )
      nearest
    )
    nil
  )
)

;;;---------------------------------------------------------------
;;;  block-text-get-content
;;;  Get the text content of a text entity
;;;  Args: ent - entity name
;;;  Returns: string content or nil
;;;---------------------------------------------------------------
(defun block-text-get-content (ent / elist etype)
  (setq elist (entget ent))
  (setq etype (cdr (assoc 0 elist)))
  (cond
    ((= etype "TEXT")
     (cdr (assoc 1 elist))
    )
    ((= etype "MTEXT")
     (cdr (assoc 1 elist))
    )
    (T nil)
  )
)

;;;---------------------------------------------------------------
;;;  block-text-get-position
;;;  Get the insertion point of a text entity
;;;  Args: ent - entity name
;;;  Returns: point list
;;;---------------------------------------------------------------
(defun block-text-get-position (ent / elist)
  (setq elist (entget ent))
  (cdr (assoc 10 elist))
)

;;;---------------------------------------------------------------
;;;  block-get-name-from-text
;;;  Get the name of a block from nearby text
;;;  Args: ent    - entity name (block reference)
;;;         radius - search radius (nil = use default)
;;;  Returns: string name or nil
;;;---------------------------------------------------------------
(defun block-get-name-from-text (ent radius / pt nearest)
  (if (null radius)
    (setq radius *block-name-search-radius*)
  )
  (setq pt (block-get-base-point ent))
  (if pt
    (progn
      (setq nearest (block-find-nearest-text pt radius))
      (if nearest
        (cadr nearest)
        nil
      )
    )
    nil
  )
)

;;;---------------------------------------------------------------
;;;  block-get-all-on-layer
;;;  Get all block references on specified layer(s)
;;;  Args: layer-list - list of layer names (single string also accepted)
;;;         block-name - optional block name filter
;;;  Returns: selection set or nil
;;;---------------------------------------------------------------
(defun block-get-all-on-layer (layer-list block-name / filter-list ss)
  (setq filter-list (list (cons 0 "INSERT")))
  (if layer-list
    (if (stringp layer-list)
      (setq filter-list (cons (cons 8 layer-list) filter-list))
      (setq filter-list (cons (cons 8 (apply 'strcat (mapcar '(lambda (x) (strcat x ",")) layer-list))) filter-list))
    )
  )
  (if block-name
    (setq filter-list (cons (cons 2 block-name) filter-list))
  )
  (setq ss (ssget "x" filter-list))
  ss
)

;;;---------------------------------------------------------------
;;;  block-get-all-in-area
;;;  Get all block references within a rectangular area
;;;  Args: pt1, pt2 - corner points of rectangle
;;;         block-name - optional block name filter
;;;  Returns: selection set or nil
;;;---------------------------------------------------------------
(defun block-get-all-in-area (pt1 pt2 block-name / filter-list)
  (setq filter-list (list (cons 0 "INSERT")))
  (if block-name
    (setq filter-list (cons (cons 2 block-name) filter-list))
  )
  (ssget "_c" pt1 pt2 filter-list)
)

;;;---------------------------------------------------------------
;;;  block-set-search-radius
;;;  Set the default text search radius
;;;---------------------------------------------------------------
(defun block-set-search-radius (val)
  (setq *block-name-search-radius* val)
)

;;;---------------------------------------------------------------
;;;  block-get-search-radius
;;;  Get the current text search radius
;;;---------------------------------------------------------------
(defun block-get-search-radius ()
  *block-name-search-radius*
)

;;;===============================================================
;;;  TEST FUNCTIONS
;;;===============================================================

(defun test-M06-block-utils (/ passed failed old-cmdecho
                               blk-ent text-ent base-pt name)
  (setq passed 0 failed 0)
  (setq old-cmdecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)

  (princ "\n\n=== M06 Block Utils Tests ===")

  (command-s "_.undo" "_be")

  ;; Test 1: Create a simple block and test basic functions
  (princ "\n[Test 1] block creation and basic properties...")
  ;; Create geometry for block
  (command-s "_.circle" "0,0" "100" "")
  (setq circle-ent (entlast))
  (command-s "_.line" "-100,-100" "100,100" "")
  (setq line-ent (entlast))
  ;; Create block
  (command-s "_.block" "test-block" "0,0" circle-ent line-ent "")
  ;; Insert block
  (command-s "_.insert" "test-block" "500,500" "1" "1" "0")
  (setq blk-ent (entlast))
  ;; Test functions
  (setq blk-name (block-get-name blk-ent))
  (setq ins-pt (block-get-insertion-point blk-ent))
  (if (and (= blk-name "test-block")
           (< (abs (- (car ins-pt) 500.0)) 0.001)
           (< (abs (- (cadr ins-pt) 500.0)) 0.001))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  (command-s "_.erase" blk-ent "")

  ;; Test 2: block-get-base-point
  (princ "\n[Test 2] block-get-base-point...")
  (command-s "_.insert" "test-block" "1000,1000" "1" "1" "0")
  (setq blk-ent (entlast))
  (setq base-pt (block-get-base-point blk-ent))
  ;; Base point should be at block center (circle center transformed)
  (if (and base-pt
           (< (abs (- (car base-pt) 1000.0)) 0.001)
           (< (abs (- (cadr base-pt) 1000.0)) 0.001))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  (command-s "_.erase" blk-ent "")

  ;; Test 3: block-get-scale and block-get-rotation
  (princ "\n[Test 3] block-get-scale and block-get-rotation...")
  (command-s "_.insert" "test-block" "0,0" "2" "2" "45")
  (setq blk-ent (entlast))
  (setq scale (block-get-scale blk-ent))
  (setq rot (block-get-rotation blk-ent))
  (if (and scale
           (= (length scale) 3)
           (< (abs (- (car scale) 2.0)) 0.001)
           (< (abs (- rot (* pi 0.25))) 0.001))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  (command-s "_.erase" blk-ent "")

  ;; Test 4: block-transform-point
  (princ "\n[Test 4] block-transform-point...")
  (setq pt (list 100.0 0.0 0.0))
  (setq insertion (list 500.0 500.0 0.0))
  (setq scale (list 2.0 2.0 1.0))
  (setq rotation (* pi 0.5))  ; 90 degrees
  (setq result (block-transform-point pt insertion scale rotation))
  ;; (100,0) scaled by 2 = (200,0), rotated 90 = (0,200), translated = (500,700)
  (if (and result
           (< (abs (- (car result) 500.0)) 0.001)
           (< (abs (- (cadr result) 700.0)) 0.001))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 5: block-text-get-content and block-text-get-position
  (princ "\n[Test 5] block-text-get-content...")
  (command-s "_.text" "_j" "_m" "1000,1000" "100" "0" "TestText")
  (setq text-ent (entlast))
  (setq content (block-text-get-content text-ent))
  (setq pos (block-text-get-position text-ent))
  (if (and (= content "TestText")
           (< (abs (- (car pos) 1000.0)) 0.001))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  (command-s "_.erase" text-ent "")

  ;; Test 6: block-find-nearest-text
  (princ "\n[Test 6] block-find-nearest-text...")
  (command-s "_.text" "_j" "_m" "100,100" "50" "0" "NearText")
  (setq text-ent (entlast))
  (setq nearest (block-find-nearest-text (list 120.0 120.0 0.0) 200.0))
  (if (and nearest
           (= (length nearest) 3)
           (= (cadr nearest) "NearText"))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  (command-s "_.erase" text-ent "")

  ;; Test 7: block-get-all-on-layer
  (princ "\n[Test 7] block-get-all-on-layer...")
  (command-s "_.insert" "test-block" "0,0" "1" "1" "0")
  (setq blk1 (entlast))
  (command-s "_.insert" "test-block" "1000,0" "1" "1" "0")
  (setq blk2 (entlast))
  (setq ss (block-get-all-on-layer "0" "test-block"))
  (if (and ss (>= (sslength ss) 2))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  (command-s "_.erase" blk1 "")
  (command-s "_.erase" blk2 "")

  ;; Test 8: block-get-definition
  (princ "\n[Test 8] block-get-definition...")
  (setq blk-def (block-get-definition "test-block"))
  (if blk-def
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 9: block-get-entities
  (princ "\n[Test 9] block-get-entities...")
  (setq entities (block-get-entities "test-block"))
  (if (and entities (>= (length entities) 2))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 10: radius get/set
  (princ "\n[Test 10] radius get/set...")
  (setq old-rad (block-get-search-radius))
  (block-set-search-radius 5000.0)
  (if (= (block-get-search-radius) 5000.0)
    (progn 
      (princ " PASS") 
      (setq passed (1+ passed))
      (block-set-search-radius old-rad)
    )
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Cleanup block definition
  (command-s "_.purge" "_b" "test-block" "_n")

  (command-s "_.undo" "_e")

  ;; Summary
  (princ (strcat "\n\n=== M06 Test Summary: "
                 (itoa passed) " passed, "
                 (itoa failed) " failed ===\n"))

  (setvar "cmdecho" old-cmdecho)
  (list passed failed)
)

;;;---------------------------------------------------------------
;;;  Provide module info
;;;---------------------------------------------------------------
(princ "\n[M06] block_utils.lsp loaded.")
(princ (strcat "  Functions: block-get-base-point, block-get-name, "
               "block-get-name-from-text, block-transform-point, ..."))
(princ (strcat "\n  Default search radius: " (rtos *block-name-search-radius* 2 0)))
(princ "\n  Test: (test-M06-block-utils)")
(princ)

;;;===============================================================
;;;  --- MODULE SEPARATOR ---
;;;  Previous: M06_block_utils.lsp
;;;  Next: M07_device_projection.lsp
;;;===============================================================

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

;;;===============================================================
;;;  --- MODULE SEPARATOR ---
;;;  Previous: M07_device_projection.lsp
;;;  Next: M08_equivalent_points.lsp
;;;===============================================================

;;;===============================================================
;;;  M08 - Equivalent Points Module
;;;  Handle equivalent connectivity points
;;;===============================================================
;;;  ENCODING: ANSI (ASCII only, no Chinese characters)
;;;  DEPENDENCIES: M01, M07
;;;===============================================================

;;;---------------------------------------------------------------
;;;  Global Variables
;;;---------------------------------------------------------------
(setq *equiv-pairs* nil)
(setq *equiv-next-id* 1)

;;;---------------------------------------------------------------
;;;  equiv-clear
;;;  Clear all equivalent point pairs
;;;---------------------------------------------------------------
(defun equiv-clear ()
  (setq *equiv-pairs* nil)
  (setq *equiv-next-id* 1)
)

;;;---------------------------------------------------------------
;;;  equiv-add-pair
;;;  Add an equivalent point pair
;;;  Args: pt1, pt2 - equivalent point coordinates
;;;  Returns: pair id
;;;---------------------------------------------------------------
(defun equiv-add-pair (pt1 pt2 / id)
  (setq id *equiv-next-id*)
  (setq *equiv-pairs* (cons (list (list pt1 pt2) id) *equiv-pairs*))
  (setq *equiv-next-id* (1+ *equiv-next-id*))
  id
)

;;;---------------------------------------------------------------
;;;  equiv-get-all-pairs
;;;  Get all equivalent point pairs
;;;  Returns: list of ((pt1 pt2) id)
;;;---------------------------------------------------------------
(defun equiv-get-all-pairs ()
  *equiv-pairs*
)

;;;---------------------------------------------------------------
;;;  equiv-count
;;;  Get number of equivalent point pairs
;;;---------------------------------------------------------------
(defun equiv-count ()
  (length *equiv-pairs*)
)

;;;---------------------------------------------------------------
;;;  equiv-process-all
;;;  Process all equivalent pairs and connect them to graph
;;;  Args: line-ss - selection set of lines for projection
;;;  Returns: number of connections made
;;;---------------------------------------------------------------
(defun equiv-process-all (line-ss / count)
  (setq count 0)
  (princ (strcat "\n[equiv] Processing " (itoa (length *equiv-pairs*)) " equivalent pairs..."))
  (foreach pair *equiv-pairs*
    (if (equiv-connect-pair (car pair) line-ss)
      (setq count (1+ count))
    )
  )
  (princ (strcat "\n[equiv] Connected " (itoa count) " pairs."))
  count
)

;;;---------------------------------------------------------------
;;;  equiv-connect-pair
;;;  Connect one equivalent pair to the graph
;;;  Args: pt-pair - (pt1 pt2)
;;;         line-ss - selection set of lines
;;;  Returns: T if connected, nil otherwise
;;;---------------------------------------------------------------
(defun equiv-connect-pair (pt-pair line-ss / pt1 pt2 proj1 proj2)
  (setq pt1 (car pt-pair))
  (setq pt2 (cadr pt-pair))
  ;; Project both points to graph
  (setq proj1 (device-project-to-graph pt1 line-ss nil))
  (setq proj2 (device-project-to-graph pt2 line-ss nil))
  ;; Add edge between the two projected nodes
  (if (and proj1 proj2)
    (progn
      (graph-add-edge (car proj1) (car proj2) (distance (cadr proj1) (cadr proj2)))
      T
    )
    nil
  )
)

;;;---------------------------------------------------------------
;;;  equiv-remove-pair
;;;  Remove a pair by id
;;;  Args: id - pair id
;;;---------------------------------------------------------------
(defun equiv-remove-pair (id / pair)
  (setq pair (assoc id *equiv-pairs*))
  (if pair
    (setq *equiv-pairs* (vl-remove pair *equiv-pairs*))
  )
)

;;;===============================================================
;;;  TEST FUNCTIONS
;;;===============================================================

(defun test-M08-equivalent-points (/ passed failed old-cmdecho id count)
  (setq passed 0 failed 0)
  (setq old-cmdecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)

  (princ "\n\n=== M08 Equivalent Points Tests ===")

  ;; Test 1: equiv-clear and equiv-add-pair
  (princ "\n[Test 1] equiv-add-pair...")
  (equiv-clear)
  (setq id1 (equiv-add-pair (list 0.0 0.0 0.0) (list 100.0 0.0 0.0)))
  (setq id2 (equiv-add-pair (list 50.0 50.0 0.0) (list 150.0 50.0 0.0)))
  (if (and (= id1 1) (= id2 2) (= (equiv-count) 2))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 2: equiv-get-all-pairs
  (princ "\n[Test 2] equiv-get-all-pairs...")
  (setq pairs (equiv-get-all-pairs))
  (if (= (length pairs) 2)
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 3: equiv-remove-pair
  (princ "\n[Test 3] equiv-remove-pair...")
  (equiv-remove-pair 1)
  (if (= (equiv-count) 1)
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Cleanup
  (equiv-clear)

  ;; Summary
  (princ (strcat "\n\n=== M08 Test Summary: "
                 (itoa passed) " passed, "
                 (itoa failed) " failed ===\n"))

  (setvar "cmdecho" old-cmdecho)
  (list passed failed)
)

;;;---------------------------------------------------------------
;;;  Provide module info
;;;---------------------------------------------------------------
(princ "\n[M08] equivalent_points.lsp loaded.")
(princ (strcat "  Functions: equiv-add-pair, equiv-process-all, equiv-clear"))
(princ "\n  Test: (test-M08-equivalent-points)")
(princ)

;;;===============================================================
;;;  --- MODULE SEPARATOR ---
;;;  Previous: M08_equivalent_points.lsp
;;;  Next: M09_system_diagram.lsp
;;;===============================================================

;;;===============================================================
;;;  M09 - System Diagram Module
;;;  Generate CCTV system diagram
;;;  Matches original sub1_fenlei + sub1_draw_sys logic
;;;===============================================================
;;;  ENCODING: ANSI (ASCII only, no Chinese characters)
;;;  DEPENDENCIES: M01, M06
;;;===============================================================

;;;---------------------------------------------------------------
;;;  Global Parameters (matching original)
;;;---------------------------------------------------------------
(setq *sysdiag-column-spacing* 12000.0)   ; Column spacing between groups
(setq *sysdiag-block-offset* 6000.0)      ; Horizontal offset for block insertion
(setq *sysdiag-text-height* 250.0)        ; Text height for labels
(setq *sysdiag-rect-width* 1000.0)        ; Rectangle width around groups
(setq *sysdiag-init-y-offset* 1019.0)     ; Initial Y offset (polar angle ~-11deg)
(setq *sysdiag-init-angle* (/ pi -16.4))  ; Initial polar angle
(setq *sysdiag-min-row-spacing* 700.0)    ; Minimum row spacing
(setq *sysdiag-cable-label-prefix* "SXTGDDL")  ; Cable label prefix (was Chinese)
(setq *sysdiag-dist-unit* 1000.0)         ; Distance unit divisor (mm to m)

;;; Block bounding box globals (set during insertion)
(setq *sysdiag-gj-heigh* nil)             ; Block height (set by sysdiag-insert-block)
(setq *sysdiag-gj-length* nil)            ; Block width (set by sysdiag-insert-block)

;;;---------------------------------------------------------------
;;;  sysdiag-extract-number
;;;  Extract numeric characters from a string
;;;  Equivalent to original sub1_vl-string->number
;;;  Args: str - input string
;;;  Returns: string containing only digits and decimal point
;;;---------------------------------------------------------------
(defun sysdiag-extract-number (str / num nu0 nu1 nu2 nu3 number)
  (setq num (vl-string->list str))
  (setq nu0 (vl-string->list ".0123456789"))
  (setq nu2 (length num))
  (setq number nil)
  (repeat nu2
    (setq nu1 (car num))
    (setq nu3 (member nu1 nu0))
    (setq num (cdr num))
    (if nu3
      (setq number (cons nu1 number))
    )
  )
  (if number
    (vl-list->string (reverse number))
    ""
  )
)

;;;---------------------------------------------------------------
;;;  sysdiag-classify-by-junction
;;;  Classify device list by junction name (4th element)
;;;  Equivalent to original sub1_fenlei
;;;  Args: lst - list of (distance block-name camera-name junction-name)
;;;  Returns: list of groups, each group is a list of items
;;;           ((group1-item1 group1-item2 ...) (group2-item1 ...) ...)
;;;---------------------------------------------------------------
(defun sysdiag-classify-by-junction (lst / i end end-drawlist tmp-lst tmp-gjx)
  (setq i 0)
  (setq end nil)
  (setq end-drawlist lst)
  (while end-drawlist
    (setq tmp-lst (list (car end-drawlist)))
    ;; Get junction name from 4th element (nth 3)
    (setq tmp-gjx (nth 3 (car tmp-lst)))
    (setq end-drawlist (cdr end-drawlist))
    (setq i 0)
    (repeat (length end-drawlist)
      (if (= (nth 3 (nth i end-drawlist)) tmp-gjx)
        (progn
          (setq tmp-lst (cons (nth i end-drawlist) tmp-lst))
          (setq end-drawlist (vl-remove (nth i end-drawlist) end-drawlist))
        )
        (setq i (1+ i))
      )
    )
    (setq end (append (list tmp-lst) end))
    (setq i 0)
  )
  end
)

;;;---------------------------------------------------------------
;;;  sysdiag-insert-block
;;;  Insert a camera block at specified point, center-align
;;;  Equivalent to original sub1_insert_block
;;;  Args: pts  - insertion point
;;;        name - block name
;;;  Side effects: sets *sysdiag-gj-heigh* and *sysdiag-gj-length*
;;;---------------------------------------------------------------
(defun sysdiag-insert-block (pts name / tmp-block p1 p2 gj-pts gj-jidian)
  (command-s "_.insert" name pts "" "" 0)
  (setq tmp-block (entlast))
  (vla-GetBoundingBox (vlax-ename->vla-object tmp-block) 'p1 'p2)
  (setq p1 (vlax-safearray->list p1))
  (setq p2 (vlax-safearray->list p2))
  (setq *sysdiag-gj-heigh* (- (cadr p2) (cadr p1)))
  (setq *sysdiag-gj-length* (- (car p2) (car p1)))
  ;; Calculate geometric center (vertical midpoint)
  (setq gj-pts (polar p1 (/ pi 2) (/ *sysdiag-gj-heigh* 2.0)))
  ;; Get actual insertion point of the block
  (setq gj-jidian (cdr (assoc 10 (entget tmp-block))))
  ;; Move block so geometric center aligns with insertion point
  (command-s "_.move" (entlast) "" gj-pts gj-jidian)
  (princ)
)

;;;---------------------------------------------------------------
;;;  sysdiag-draw-classified
;;;  Draw system diagram for classified groups
;;;  Equivalent to original sub1_draw_sys
;;;  Args: htd - insertion point (top-left of diagram)
;;;        lst - classified list from sysdiag-classify-by-junction
;;;              each group: ((dist block-name cam-name jnx-name) ...)
;;;  Returns: T on success
;;;---------------------------------------------------------------
(defun sysdiag-draw-classified (htd lst / cm os end htd-pt1 htd-pt2 htd-pt3
                                       htd-p1 end-p1 tmp-drawblock
                                       tmp-drawblockname tmp-textpts
                                       tmp-textstr tmp-namepts i m
                                       tmp-draw)
  (setq cm (getvar "cmdecho"))
  (setq os (getvar "osmode"))
  (setvar "cmdecho" 0)
  (setvar "osmode" 0)
  (setvar "clayer" "0")
  (setq end lst)
  (setq i 0)
  (setq m 0)

  ;; Process each group (column)
  (repeat (length end)
    ;; Get current group and sort by camera name number (ascending)
    (setq tmp-draw (nth m end))
    (setq tmp-draw
      (vl-sort tmp-draw
        '(lambda (e1 e2)
           (< (atof (sysdiag-extract-number (caddr e1)))
              (atof (sysdiag-extract-number (caddr e2)))))))

    ;; Set starting Y position for this column
    (setq htd-p1 (polar htd *sysdiag-init-angle* *sysdiag-init-y-offset*))

    ;; Draw each device in this group (rows)
    (repeat (length tmp-draw)
      ;; Draw horizontal connection line
      (setq end-p1 (polar htd-p1 0 *sysdiag-block-offset*))
      (command-s "_.pline" htd-p1 end-p1 "")

      ;; Get block info
      (setq tmp-drawblock (nth i tmp-draw))
      (setq tmp-drawblockname (nth 1 tmp-drawblock))

      ;; Cable length label position (slightly right of start)
      (setq tmp-textpts (polar htd-p1 0.2 250.0))
      ;; Cable length label: "SXTGDDL-XXm"
      (setq tmp-textstr
        (strcat *sysdiag-cable-label-prefix* "-"
                (rtos (fix (/ (nth 0 tmp-drawblock) *sysdiag-dist-unit*)) 2 0)
                "m"))

      ;; Insert camera block at end point
      (sysdiag-insert-block end-p1 tmp-drawblockname)

      ;; Camera name label (right of block)
      (setq tmp-namepts (polar end-p1 0 (+ *sysdiag-gj-length* 100.0)))
      (command-s "_.text" tmp-textpts *sysdiag-text-height* 0 tmp-textstr)
      (command-s "_.text" tmp-namepts *sysdiag-text-height* 0 (nth 2 tmp-drawblock))

      (setq i (1+ i))

      ;; Move to next row (downward)
      (if (> (+ *sysdiag-gj-heigh* 100.0) *sysdiag-min-row-spacing*)
        (setq htd-p1 (polar htd-p1 (* pi 1.5) (+ *sysdiag-gj-heigh* 100.0)))
        (setq htd-p1 (polar htd-p1 (* pi 1.5) *sysdiag-min-row-spacing*))
      )
    )

    ;; Draw rectangle around this group
    ;; Height = distance from htd to htd-p1 + 200 margin
    (setq htd-pt1 (polar htd (* pi 1.5)
                         (+ (- (cadr htd) (cadr htd-p1)) 200.0)))
    (setq htd-pt2 (polar htd-pt1 0 *sysdiag-rect-width*))
    (setq htd-pt3 (polar htd-pt2 (* pi 0.5)
                         (+ (- (cadr htd) (cadr htd-p1)) 200.0)))
    (command-s "_.pline" htd htd-pt1 htd-pt2 htd-pt3 "c")

    ;; Junction name label (upper-left of rectangle)
    (command-s "_.text" (polar htd (* pi 0.7) 400.0)
               *sysdiag-text-height* 0 (nth 3 (car tmp-draw)))

    ;; Move to next column (rightward)
    (setq m (1+ m))
    (setq i 0)
    (setq htd (polar htd 0 *sysdiag-column-spacing*))
  )

  ;; Clear block height/width
  (setq *sysdiag-gj-heigh* nil)
  (setvar "cmdecho" cm)
  (setvar "osmode" os)
  T
)

;;;---------------------------------------------------------------
;;;  sysdiag-draw-hjx
;;;  Draw HJX (convergence box) system diagram
;;;  Equivalent to original sub10_draw_sys (without draw_BDXrec)
;;;  Args: htd - insertion point
;;;        lst - list of (distance hjx-name hjx-entity)
;;;  Returns: T on success
;;;---------------------------------------------------------------
(defun sysdiag-draw-hjx (htd lst / cm os end htd-pt1 htd-pt2 htd-pt3
                                htd-p1 end-p1 tmp-drawblock
                                tmp-drawblockname tmp-textpts
                                tmp-textstr tmp-namepts i m
                                tmp-draw actual-height)
  (setq cm (getvar "cmdecho"))
  (setq os (getvar "osmode"))
  (setvar "cmdecho" 0)
  (setvar "osmode" 0)
  (setvar "clayer" "0")
  (setq end lst)
  (setq i 0)
  (setq m 0)

  ;; Sort entire list by name (2nd element) ascending
  (setq end (vl-sort end '(lambda (e1 e2) (< (cadr e1) (cadr e2)))))

  ;; Set starting Y position
  (setq htd-p1 (polar htd *sysdiag-init-angle* *sysdiag-init-y-offset*))

  ;; Draw each HJX device
  (repeat (length end)
    ;; Draw horizontal connection line
    (setq end-p1 (polar htd-p1 0 *sysdiag-block-offset*))
    (command-s "_.pline" htd-p1 end-p1 "")

    ;; Get block info
    (setq tmp-drawblock (nth m end))
    ;; Block name from entity
    (setq tmp-drawblockname
      (cdr (assoc 2 (entget (nth 2 tmp-drawblock)))))

    ;; Cable label position
    (setq tmp-textpts (polar htd-p1 0.2 250.0))
    ;; Cable label: "HJXXL-XXm"
    (setq tmp-textstr
      (strcat "HJXXL-"
              (rtos (fix (/ (nth 0 tmp-drawblock) *sysdiag-dist-unit*)) 2 0)
              "m"))

    ;; Insert block
    (sysdiag-insert-block end-p1 tmp-drawblockname)

    ;; Name label (right of block)
    (setq tmp-namepts (polar end-p1 0 (+ *sysdiag-gj-length* 100.0)))
    (command-s "_.text" tmp-textpts *sysdiag-text-height* 0 tmp-textstr)
    (command-s "_.text" tmp-namepts *sysdiag-text-height* 0 (nth 1 tmp-drawblock))

    (setq i (1+ i))

    ;; Move to next row
    (if (> (+ *sysdiag-gj-heigh* 100.0) 550.0)
      (setq htd-p1 (polar htd-p1 (* pi 1.5) (+ *sysdiag-gj-heigh* 100.0)))
      (setq htd-p1 (polar htd-p1 (* pi 1.5) 100.0))
    )

    (setq m (1+ m))
    (setq i 0)
  )

  ;; Draw rectangle around all HJX devices
  (setq actual-height (- (cadr htd) (cadr htd-p1)))
  (setq htd-pt1 (polar htd (* pi 1.5) actual-height))
  (setq htd-pt2 (polar htd-pt1 0 *sysdiag-rect-width*))
  (setq htd-pt3 (polar htd-pt2 (* pi 0.5) actual-height))

  ;; If height exceeds 18800, use actual height; otherwise use 18800
  (if (> actual-height 18800.0)
    (command-s "_.pline" htd htd-pt1 htd-pt2 htd-pt3 "c")
    (progn
      (setq htd-pt1 (polar htd (* pi 1.5) 18800.0))
      (setq htd-pt2 (polar htd-pt1 0 *sysdiag-rect-width*))
      (setq htd-pt3 (polar htd-pt2 (* pi 0.5) 18800.0))
      (command-s "_.pline" htd htd-pt1 htd-pt2 htd-pt3 "c")
    )
  )

  ;; Note: draw_BDXrec (semi-fixed diagram) is NOT included
  ;; per refactoring requirement to remove semi-fixed diagram drawing

  (setq *sysdiag-gj-heigh* nil)
  (setvar "cmdecho" cm)
  (setvar "osmode" os)
  T
)

;;;---------------------------------------------------------------
;;;  Legacy functions (kept for backward compatibility)
;;;---------------------------------------------------------------

;;;---------------------------------------------------------------
;;;  sysdiag-group-by-junction
;;;  Group devices by junction coordinate key
;;;  Args: device-list - list of (device-pt junction-pt distance device-name)
;;;  Returns: assoc list of (junction-key . device-list)
;;;---------------------------------------------------------------
(defun sysdiag-group-by-junction (device-list / result jnx-key entry)
  (setq result nil)
  (foreach dev device-list
    (setq jnx-key (graph-coord->key (cadr dev)))
    (setq entry (assoc jnx-key result))
    (if entry
      (setq result (subst (cons jnx-key (cons dev (cdr entry))) entry result))
      (setq result (cons (list jnx-key dev) result))
    )
  )
  result
)

;;;---------------------------------------------------------------
;;;  sysdiag-sort-devices
;;;  Sort devices by name number
;;;  Args: device-list - list of devices
;;;  Returns: sorted list
;;;---------------------------------------------------------------
(defun sysdiag-sort-devices (device-list / sorted)
  (vl-sort device-list
    '(lambda (a b)
       (< (atof (sysdiag-extract-number (cadddr a)))
          (atof (sysdiag-extract-number (cadddr b))))))
)

;;;---------------------------------------------------------------
;;;  sysdiag-draw-cable-line
;;;  Draw a cable line with length label
;;;  Args: start-pt - start point
;;;         end-pt   - end point
;;;         length   - cable length
;;;         layer    - layer name
;;;---------------------------------------------------------------
(defun sysdiag-draw-cable-line (start-pt end-pt length layer / mid-pt)
  (command-s "_.line" start-pt end-pt "")
  (setq mid-pt (list (/ (+ (car start-pt) (car end-pt)) 2.0)
                     (/ (+ (cadr start-pt) (cadr end-pt)) 2.0)
                     0.0))
  (command-s "_.text" "_j" "_m" mid-pt *sysdiag-text-height* "0"
             (rtos (/ length *sysdiag-dist-unit*) 2 1))
)

;;;---------------------------------------------------------------
;;;  sysdiag-draw
;;;  Main function to draw system diagram (legacy interface)
;;;  Args: device-list - list of (device-pt junction-pt distance device-name)
;;;         start-pt    - insertion point (nil = use default)
;;;  Returns: T on success
;;;---------------------------------------------------------------
(defun sysdiag-draw (device-list start-pt / grouped current-y jnx-devices ins-pt)
  (if start-pt
    (setq ins-pt start-pt)
    (setq ins-pt (list 0.0 0.0 0.0))
  )

  (princ (strcat "\n[sysdiag] Drawing system diagram for "
                 (itoa (length device-list)) " devices..."))

  ;; Group by junction
  (setq grouped (sysdiag-group-by-junction device-list))

  ;; Draw each junction group
  (setq current-y (cadr ins-pt))
  (foreach group grouped
    (setq jnx-devices (sysdiag-sort-devices (cdr group)))
    (sysdiag-draw-junction-group (car group) jnx-devices current-y ins-pt)
    (setq current-y (- current-y (* (1+ (length jnx-devices)) *sysdiag-min-row-spacing*)))
  )

  (princ "\n[sysdiag] Done.")
  T
)

;;;---------------------------------------------------------------
;;;  sysdiag-draw-junction-group
;;;  Draw one junction group (legacy vertical layout)
;;;---------------------------------------------------------------
(defun sysdiag-draw-junction-group (jnx-key devices start-y ins-pt / jnx-pt x y)
  (setq jnx-pt (graph-key->coord jnx-key))
  (setq x (car ins-pt))
  (setq y start-y)

  ;; Draw junction box
  (command-s "_.rectang" (list x y 0.0) (list (+ x 500.0) (+ y 300.0) 0.0))
  (command-s "_.text" "_j" "_m" (list (+ x 250.0) (+ y 150.0) 0.0)
             *sysdiag-text-height* "0" "Junction")

  ;; Draw each device connection
  (foreach dev devices
    (setq y (- y *sysdiag-min-row-spacing*))
    (sysdiag-draw-cable-line (list (+ x 500.0) (+ y 150.0) 0.0)
                             (list (+ x 1000.0) (+ y 150.0) 0.0)
                             (caddr dev)
                             nil)
    (command-s "_.text" "_j" "_ml" (list (+ x 1050.0) (+ y 150.0) 0.0)
               *sysdiag-text-height* "0" (cadddr dev))
  )
)

;;;===============================================================
;;;  TEST FUNCTIONS
;;;===============================================================

(defun test-M09-system-diagram (/ passed failed result grouped sorted)
  (setq passed 0 failed 0)

  (princ "\n\n=== M09 System Diagram Tests ===")

  ;; Test 1: sysdiag-extract-number - basic digits
  (princ "\n[Test 1] sysdiag-extract-number (basic)...")
  (setq result (sysdiag-extract-number "CAM-123"))
  (if (= result "123")
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ (strcat " FAIL (got '" result "')")) (setq failed (1+ failed)))
  )

  ;; Test 2: sysdiag-extract-number - with decimal
  (princ "\n[Test 2] sysdiag-extract-number (decimal)...")
  (setq result (sysdiag-extract-number "dist-12.5m"))
  (if (= result "12.5")
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ (strcat " FAIL (got '" result "')")) (setq failed (1+ failed)))
  )

  ;; Test 3: sysdiag-extract-number - no digits
  (princ "\n[Test 3] sysdiag-extract-number (no digits)...")
  (setq result (sysdiag-extract-number "ABC"))
  (if (= result "")
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ (strcat " FAIL (got '" result "')")) (setq failed (1+ failed)))
  )

  ;; Test 4: sysdiag-extract-number - mixed
  (princ "\n[Test 4] sysdiag-extract-number (mixed)...")
  (setq result (sysdiag-extract-number "CCTV-15-HD"))
  (if (= result "15")
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ (strcat " FAIL (got '" result "')")) (setq failed (1+ failed)))
  )

  ;; Test 5: sysdiag-classify-by-junction
  (princ "\n[Test 5] sysdiag-classify-by-junction...")
  (setq test-list
    (list
      (list 100.0 "BLK1" "CAM1" "JNX-A")
      (list 200.0 "BLK2" "CAM2" "JNX-B")
      (list 150.0 "BLK3" "CAM3" "JNX-A")
      (list 180.0 "BLK4" "CAM4" "JNX-B")
      (list 120.0 "BLK5" "CAM5" "JNX-A")
    ))
  (setq result (sysdiag-classify-by-junction test-list))
  ;; Should produce 2 groups: JNX-A has 3 items, JNX-B has 2 items
  (if (and (= (length result) 2)
           (= (length (car result)) 3)
           (= (length (cadr result)) 2))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ (strcat " FAIL (groups=" (itoa (length result)) ")"))
           (setq failed (1+ failed)))
  )

  ;; Test 6: sysdiag-classify-by-junction - all same group
  (princ "\n[Test 6] sysdiag-classify-by-junction (same group)...")
  (setq test-list
    (list
      (list 100.0 "BLK1" "CAM1" "JNX-X")
      (list 200.0 "BLK2" "CAM2" "JNX-X")
    ))
  (setq result (sysdiag-classify-by-junction test-list))
  (if (and (= (length result) 1)
           (= (length (car result)) 2))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 7: sysdiag-classify-by-junction - empty list
  (princ "\n[Test 7] sysdiag-classify-by-junction (empty)...")
  (setq result (sysdiag-classify-by-junction nil))
  (if (null result)
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 8: sysdiag-sort-devices
  (princ "\n[Test 8] sysdiag-sort-devices...")
  (setq test-devices
    (list
      (list (list 0.0 0.0 0.0) (list 100.0 0.0 0.0) 100.0 "CAM-15")
      (list (list 0.0 100.0 0.0) (list 100.0 0.0 0.0) 150.0 "CAM-3")
      (list (list 200.0 0.0 0.0) (list 300.0 0.0 0.0) 200.0 "CAM-8")
    ))
  (setq sorted (sysdiag-sort-devices test-devices))
  (if (and (= (atof (sysdiag-extract-number (cadddr (car sorted)))) 3.0)
           (= (atof (sysdiag-extract-number (cadddr (cadr sorted)))) 8.0)
           (= (atof (sysdiag-extract-number (cadddr (caddr sorted)))) 15.0))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 9: sysdiag-group-by-junction (legacy)
  (princ "\n[Test 9] sysdiag-group-by-junction (legacy)...")
  (setq test-devices
    (list
      (list (list 0.0 0.0 0.0) (list 100.0 0.0 0.0) 100.0 "CAM1")
      (list (list 0.0 100.0 0.0) (list 100.0 0.0 0.0) 150.0 "CAM2")
      (list (list 200.0 0.0 0.0) (list 300.0 0.0 0.0) 200.0 "CAM3")
    ))
  (setq grouped (sysdiag-group-by-junction test-devices))
  (if (= (length grouped) 2)
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Summary
  (princ (strcat "\n\n=== M09 Test Summary: "
                 (itoa passed) " passed, "
                 (itoa failed) " failed ===\n"))

  (list passed failed)
)

;;;---------------------------------------------------------------
;;;  Provide module info
;;;---------------------------------------------------------------
(princ "\n[M09] system_diagram.lsp loaded.")
(princ (strcat "  Functions: sysdiag-classify-by-junction, sysdiag-draw-classified, "
               "sysdiag-draw-hjx, sysdiag-insert-block"))
(princ "\n  Legacy: sysdiag-draw, sysdiag-group-by-junction, sysdiag-sort-devices")
(princ "\n  Test: (test-M09-system-diagram)")
(princ)

;;;===============================================================
;;;  --- MODULE SEPARATOR ---
;;;  Previous: M09_system_diagram.lsp
;;;  Next: M10_parameter_io.lsp
;;;===============================================================

;;;===============================================================
;;;  M10 - Parameter I/O Module
;;;  Read and write configuration files
;;;===============================================================
;;;  ENCODING: ANSI (ASCII only, no Chinese characters)
;;;  DEPENDENCIES: None
;;;===============================================================

;;;---------------------------------------------------------------
;;;  Global Parameters
;;;---------------------------------------------------------------
;;;  *param-default-file* - Default parameter file path
(setq *param-default-file* "CCTV_params.txt")

;;;---------------------------------------------------------------
;;;  param-string-split
;;;  Split a string by delimiter
;;;  Args: str       - string to split
;;;         delimiter - delimiter character (default ",")
;;;  Returns: list of substrings
;;;---------------------------------------------------------------
(defun param-string-split (str delimiter / result pos)
  (if (null delimiter) (setq delimiter ","))
  (setq result nil)
  (while (setq pos (vl-string-search delimiter str))
    (setq result (cons (substr str 1 pos) result))
    (setq str (substr str (+ pos 2)))
  )
  (setq result (cons str result))
  (reverse result)
)

;;;---------------------------------------------------------------
;;;  param-string-join
;;;  Join a list of strings with delimiter
;;;  Args: lst       - list of strings
;;;         delimiter - delimiter string (default ",")
;;;  Returns: joined string
;;;---------------------------------------------------------------
(defun param-string-join (lst delimiter / result)
  (if (null delimiter) (setq delimiter ","))
  (setq result "")
  (foreach item lst
    (if (= result "")
      (setq result item)
      (setq result (strcat result delimiter item))
    )
  )
  result
)

;;;---------------------------------------------------------------
;;;  param-string-trim
;;;  Trim whitespace from both ends of a string
;;;  Args: str - string to trim
;;;  Returns: trimmed string
;;;---------------------------------------------------------------
(defun param-string-trim (str / start end)
  (setq start 1)
  (setq end (strlen str))
  ;; Trim from start
  (while (and (<= start end)
              (wcmatch (substr str start 1) "[ ]"))
    (setq start (1+ start))
  )
  ;; Trim from end
  (while (and (>= end start)
              (wcmatch (substr str end 1) "[ ]"))
    (setq end (1- end))
  )
  (if (<= start end)
    (substr str start (- end start -1))
    ""
  )
)

;;;---------------------------------------------------------------
;;;  param-parse-line
;;;  Parse a parameter line "name=value1,value2,..."
;;;  Args: line - input line
;;;  Returns: (name (value1 value2 ...)) or nil
;;;---------------------------------------------------------------
(defun param-parse-line (line / pos name values)
  (setq line (param-string-trim line))
  ;; Skip empty lines and comments
  (if (or (= line "") (= (substr line 1 1) ";"))
    nil
    (progn
      (setq pos (vl-string-search "=" line))
      (if pos
        (progn
          (setq name (param-string-trim (substr line 1 pos)))
          (setq values (param-string-split (substr line (+ pos 2)) ","))
          (list name values)
        )
        nil
      )
    )
  )
)

;;;---------------------------------------------------------------
;;;  param-format-line
;;;  Format a parameter as "name=value1,value2,..."
;;;  Args: name   - parameter name
;;;         values - list of values
;;;  Returns: formatted string
;;;---------------------------------------------------------------
(defun param-format-line (name values)
  (strcat name "=" (param-string-join values ","))
)

;;;---------------------------------------------------------------
;;;  param-load
;;;  Load parameters from file
;;;  Args: filename - file path (nil = use default)
;;;  Returns: association list of parameters or nil
;;;---------------------------------------------------------------
(defun param-load (filename / fp line params parsed)
  (if (null filename)
    (setq filename *param-default-file*)
  )
  (setq params nil)
  (setq fp (open filename "r"))
  (if fp
    (progn
      (princ (strcat "\n[param] Loading from " filename "..."))
      (while (setq line (read-line fp))
        (setq parsed (param-parse-line line))
        (if parsed
          (setq params (cons parsed params))
        )
      )
      (close fp)
      (princ (strcat "\n[param] Loaded " (itoa (length params)) " parameters."))
      (reverse params)
    )
    (progn
      (princ (strcat "\n[param] File not found: " filename))
      nil
    )
  )
)

;;;---------------------------------------------------------------
;;;  param-save
;;;  Save parameters to file
;;;  Args: params   - association list of parameters
;;;         filename - file path (nil = use default)
;;;  Returns: T on success, nil on failure
;;;---------------------------------------------------------------
(defun param-save (params filename / fp)
  (if (null filename)
    (setq filename *param-default-file*)
  )
  (setq fp (open filename "w"))
  (if fp
    (progn
      (princ (strcat "\n[param] Saving to " filename "..."))
      ;; Write header
      (princ "; CCTV System Parameters\n" fp)
      (princ "; Auto-generated file\n" fp)
      (princ "\n" fp)
      ;; Write parameters
      (foreach param params
        (princ (param-format-line (car param) (cadr param)) fp)
        (princ "\n" fp)
      )
      (close fp)
      (princ (strcat "\n[param] Saved " (itoa (length params)) " parameters."))
      T
    )
    (progn
      (princ (strcat "\n[param] Cannot write to: " filename))
      nil
    )
  )
)

;;;---------------------------------------------------------------
;;;  param-get
;;;  Get a parameter value by name
;;;  Args: params - parameter list
;;;         name   - parameter name
;;;  Returns: list of values or nil
;;;---------------------------------------------------------------
(defun param-get (params name / entry)
  (setq entry (assoc name params))
  (if entry (cadr entry) nil)
)

;;;---------------------------------------------------------------
;;;  param-set
;;;  Set a parameter value
;;;  Args: params - parameter list
;;;         name   - parameter name
;;;         values - list of values
;;;  Returns: updated parameter list
;;;---------------------------------------------------------------
(defun param-set (params name values / entry)
  (setq entry (assoc name params))
  (if entry
    (subst (list name values) entry params)
    (cons (list name values) params)
  )
)

;;;---------------------------------------------------------------
;;;  param-remove
;;;  Remove a parameter
;;;  Args: params - parameter list
;;;         name   - parameter name
;;;  Returns: updated parameter list
;;;---------------------------------------------------------------
(defun param-remove (params name / entry)
  (setq entry (assoc name params))
  (if entry
    (vl-remove entry params)
    params
  )
)

;;;---------------------------------------------------------------
;;;  param-value-to-string
;;;  Convert a value to string
;;;  Args: val - value (number, string, point, etc.)
;;;  Returns: string representation
;;;---------------------------------------------------------------
(defun param-value-to-string (val)
  (cond
    ((null val) "")
    ((numberp val) (rtos val 2 6))
    ((listp val)
     (param-string-join (mapcar 'param-value-to-string val) ";")
    )
    (T (strcat "" val))
  )
)

;;;---------------------------------------------------------------
;;;  param-string-to-value
;;;  Convert a string to appropriate type
;;;  Args: str - input string
;;;  Returns: number, string, or list
;;;---------------------------------------------------------------
(defun param-string-to-value (str / num is-num)
  (setq str (param-string-trim str))
  (if (= str "")
    ""
    (progn
      ;; Check if string represents a number
      (setq is-num T)
      (foreach ch (vl-string->list str)
        (if (and (/= ch 45) (/= ch 46) (< ch 48) (> ch 57))
          (setq is-num nil)
        )
      )
      (if is-num
        (atof str)
        str
      )
    )
  )
)

;;;---------------------------------------------------------------
;;;  param-list-to-string
;;;  Convert a list to comma-separated string
;;;  Args: lst - list of values
;;;  Returns: string
;;;---------------------------------------------------------------
(defun param-list-to-string (lst)
  (param-string-join (mapcar 'param-value-to-string lst) ",")
)

;;;---------------------------------------------------------------
;;;  param-string-to-list
;;;  Convert comma-separated string to list
;;;  Args: str - input string
;;;  Returns: list of values
;;;---------------------------------------------------------------
(defun param-string-to-list (str)
  (mapcar 'param-string-to-value (param-string-split str ","))
)

;;;---------------------------------------------------------------
;;;  param-set-default-file
;;;  Set the default parameter file path
;;;---------------------------------------------------------------
(defun param-set-default-file (path)
  (setq *param-default-file* path)
)

;;;---------------------------------------------------------------
;;;  param-get-default-file
;;;  Get the default parameter file path
;;;---------------------------------------------------------------
(defun param-get-default-file ()
  *param-default-file*
)

;;;===============================================================
;;;  TEST FUNCTIONS
;;;===============================================================

(defun test-M10-parameter-io (/ passed failed params loaded)
  (setq passed 0 failed 0)

  (princ "\n\n=== M10 Parameter I/O Tests ===")

  ;; Test 1: param-string-split
  (princ "\n[Test 1] param-string-split...")
  (setq result (param-string-split "a,b,c,d" ","))
  (if (and (= (length result) 4)
           (= (car result) "a")
           (= (cadddr result) "d"))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 2: param-string-join
  (princ "\n[Test 2] param-string-join...")
  (setq result (param-string-join '("x" "y" "z") ";"))
  (if (= result "x;y;z")
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 3: param-string-trim
  (princ "\n[Test 3] param-string-trim...")
  (setq result (param-string-trim "  hello world  "))
  (if (= result "hello world")
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 4: param-parse-line
  (princ "\n[Test 4] param-parse-line...")
  (setq result (param-parse-line "camera_blocks=CAM1,CAM2,CAM3"))
  (if (and result
           (= (car result) "camera_blocks")
           (= (length (cadr result)) 3)
           (= (car (cadr result)) "CAM1"))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 5: param-format-line
  (princ "\n[Test 5] param-format-line...")
  (setq result (param-format-line "junction_blocks" '("GJX1" "GJX2")))
  (if (= result "junction_blocks=GJX1,GJX2")
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 6: param-set and param-get
  (princ "\n[Test 6] param-set and param-get...")
  (setq params nil)
  (setq params (param-set params "layer1" '("value1" "value2")))
  (setq params (param-set params "layer2" '("value3")))
  (setq val (param-get params "layer1"))
  (if (and (= (length val) 2)
           (= (car val) "value1"))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 7: param-remove
  (princ "\n[Test 7] param-remove...")
  (setq params (param-remove params "layer1"))
  (setq val (param-get params "layer1"))
  (if (null val)
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 8: param-value-to-string
  (princ "\n[Test 8] param-value-to-string...")
  (setq result (param-value-to-string 3.14159))
  (if (and result (wcmatch result "3.14159*"))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 9: param-string-to-value
  (princ "\n[Test 9] param-string-to-value...")
  (setq result (param-string-to-value "123.456"))
  (if (and (numberp result) (< (abs (- result 123.456)) 0.001))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 10: param-save and param-load (file I/O)
  (princ "\n[Test 10] param-save and param-load...")
  (setq test-file "test_params.txt")
  (setq test-params
    (list
      (list "camera_blocks" '("CAM-A" "CAM-B" "CAM-C"))
      (list "junction_blocks" '("GJX-01" "GJX-02"))
      (list "cable_tray_layer" '("CABLE_TRAY"))
      (list "coefficient" '("1.2"))
    ))
  (param-save test-params test-file)
  (setq loaded (param-load test-file))
  (if (and loaded
           (= (length loaded) 4)
           (param-get loaded "camera_blocks"))
    (progn 
      (princ " PASS") 
      (setq passed (1+ passed))
      ;; Cleanup
      (vl-file-delete test-file)
    )
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 11: default file get/set
  (princ "\n[Test 11] default file get/set...")
  (setq old-file (param-get-default-file))
  (param-set-default-file "new_params.txt")
  (if (= (param-get-default-file) "new_params.txt")
    (progn 
      (princ " PASS") 
      (setq passed (1+ passed))
      (param-set-default-file old-file)
    )
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Summary
  (princ (strcat "\n\n=== M10 Test Summary: "
                 (itoa passed) " passed, "
                 (itoa failed) " failed ===\n"))

  (list passed failed)
)

;;;---------------------------------------------------------------
;;;  Provide module info
;;;---------------------------------------------------------------
(princ "\n[M10] parameter_io.lsp loaded.")
(princ (strcat "  Functions: param-load, param-save, param-get, param-set, "
               "param-string-split, param-string-join"))
(princ (strcat "\n  Default file: " *param-default-file*))
(princ "\n  Test: (test-M10-parameter-io)")
(princ)

;;;===============================================================
;;;  --- MODULE SEPARATOR ---
;;;  Previous: M10_parameter_io.lsp
;;;  Next: M11_gui_handlers.lsp
;;;===============================================================

;;;===============================================================
;;;  M11 - GUI Handlers Module (Simplified)
;;;  Command-line based user interaction
;;;  Note: Original used OpenDCL, this is a simplified version
;;;  Updated to support all M12 functions
;;;===============================================================
;;;  ENCODING: ANSI (ASCII only, no Chinese characters)
;;;  DEPENDENCIES: M01-M12
;;;===============================================================

;;;---------------------------------------------------------------
;;;  gui-select-blocks
;;;  Interactive block selection
;;;---------------------------------------------------------------
(defun gui-select-blocks (prompt / ss names i name)
  (princ (strcat "\n" prompt))
  (setq ss (ssget '((0 . "INSERT"))))
  (if ss
    (progn
      (setq names nil)
      (setq i 0)
      (repeat (sslength ss)
        (setq name (cdr (assoc 2 (entget (ssname ss i)))))
        (if (and name (not (member name names)))
          (setq names (cons name names))
        )
        (setq i (1+ i))
      )
      names
    )
    nil
  )
)

;;;---------------------------------------------------------------
;;;  gui-select-layer
;;;  Interactive layer selection
;;;---------------------------------------------------------------
(defun gui-select-layer (prompt / layer)
  (princ (strcat "\n" prompt))
  (setq layer (getstring "\nEnter layer name: "))
  (if (/= layer "") layer nil)
)

;;;---------------------------------------------------------------
;;;  gui-select-point
;;;  Interactive point selection
;;;---------------------------------------------------------------
(defun gui-select-point (prompt / pt)
  (princ (strcat "\n" prompt))
  (setq pt (getpoint))
  pt
)

;;;---------------------------------------------------------------
;;;  gui-select-layers-multi
;;;  Select multiple layers interactively
;;;  Returns: list of layer names
;;;---------------------------------------------------------------
(defun gui-select-layers-multi (prompt / layers input done)
  (setq layers nil)
  (setq done nil)
  (princ (strcat "\n" prompt))
  (princ "\n  Enter layer names one by one. Enter empty string to finish.")
  (while (null done)
    (setq input (getstring "\n  Layer name (Enter to finish): "))
    (if (= input "")
      (setq done T)
      (setq layers (cons input layers))
    )
  )
  (if layers (reverse layers) nil)
)

;;;---------------------------------------------------------------
;;;  gui-configure-workflow
;;;  Interactive workflow configuration
;;;  Matches original OpenDCL form fields
;;;---------------------------------------------------------------
(defun gui-configure-workflow (/ cam-blocks jnx-blocks tray-layer pipe-layer
                                     name-layers coef jnx-bias room-bias)
  (princ "\n\n========================================")
  (princ "\n  CCTV System Configuration")
  (princ "\n========================================")

  ;; Step 1: Camera blocks
  (princ "\n\n[1/8] Camera block selection")
  (princ "\n  Select camera block(s) in drawing:")
  (setq cam-blocks (gui-select-blocks "Select camera blocks: "))
  (if cam-blocks
    (progn
      (main-set-camera-blocks cam-blocks)
      (princ (strcat "\n  Selected " (itoa (length cam-blocks)) " block type(s)."))
    )
    (princ "\n  Warning: No camera blocks selected.")
  )

  ;; Step 2: Camera name text layers
  (princ "\n\n[2/8] Camera name text layers")
  (princ "\n  Layers containing camera name text labels.")
  (setq name-layers (gui-select-layers-multi "Camera name text layers: "))
  (if name-layers
    (progn
      (main-set-camera-name-layers name-layers)
      (princ (strcat "\n  Set " (itoa (length name-layers)) " name layer(s)."))
    )
    (princ "\n  No name layers set.")
  )

  ;; Step 3: Junction box blocks
  (princ "\n\n[3/8] Junction box block selection")
  (princ "\n  Select junction box block(s) in drawing:")
  (setq jnx-blocks (gui-select-blocks "Select junction box blocks: "))
  (if jnx-blocks
    (progn
      (main-set-junction-blocks jnx-blocks)
      (princ (strcat "\n  Selected " (itoa (length jnx-blocks)) " block type(s)."))
    )
    (princ "\n  Warning: No junction blocks selected.")
  )

  ;; Step 4: Cable tray (MLINE) layer
  (princ "\n\n[4/8] Cable tray layer (MLINE)")
  (setq tray-layer (gui-select-layer "Cable tray MLINE layer name: "))
  (if tray-layer
    (progn
      (main-set-cable-tray-layer tray-layer)
      (main-bltc-add tray-layer)
      (princ (strcat "\n  Cable tray layer: " tray-layer))
    )
    (princ "\n  No cable tray layer set.")
  )

  ;; Step 5: Pipe layer (optional)
  (princ "\n\n[5/8] Pipe layer (optional)")
  (setq pipe-layer (gui-select-layer "Pipe layer name (Enter to skip): "))
  (if pipe-layer
    (progn
      (main-set-pipe-layer pipe-layer)
      (princ (strcat "\n  Pipe layer: " pipe-layer))
    )
    (princ "\n  No pipe layer set.")
  )

  ;; Step 6: Cable coefficient
  (princ "\n\n[6/8] Cable length coefficient")
  (setq coef (getreal "\n  Enter coefficient (default 1.2): "))
  (if coef
    (main-set-cable-coefficient coef)
    (main-set-cable-coefficient 1.2)
  )
  (princ (strcat "\n  Coefficient: " (rtos *main-cable-coefficient* 2 2)))

  ;; Step 7: Distance biases
  (princ "\n\n[7/8] Distance biases")
  (setq jnx-bias (getreal "\n  Junction bias (default 10000): "))
  (setq room-bias (getreal "\n  Room entry bias (default 25000): "))
  (main-set-biases
    (if jnx-bias jnx-bias 10000.0)
    (if room-bias room-bias 25000.0))
  (princ (strcat "\n  Junction bias: " (rtos *main-junction-bias* 2 0)))
  (princ (strcat "\n  Room bias: " (rtos *main-room-bias* 2 0)))

  ;; Step 8: Room entry points (optional)
  (princ "\n\n[8/8] Room entry points (optional)")
  (if (= (getstring "\n  Add room entry points? (y/n): ") "y")
    (c:CCTV-RoomPts)
    (princ "\n  No room points set.")
  )

  (princ "\n\n========================================")
  (princ "\n  Configuration complete!")
  (princ "\n========================================")
  T
)

;;;---------------------------------------------------------------
;;;  c:CCTV-Config
;;;  Configuration command
;;;---------------------------------------------------------------
(defun c:CCTV-Config ()
  (gui-configure-workflow)
  (princ)
)

;;;---------------------------------------------------------------
;;;  c:CCTV-Run
;;;  Run workflow with current config
;;;---------------------------------------------------------------
(defun c:CCTV-Run ()
  (if (null *main-camera-blocks*)
    (progn
      (princ "\nError: No camera blocks configured.")
      (princ "\nRun CCTV-Config first.")
      (gui-configure-workflow)
    )
  )
  (main-run-workflow)
  (princ)
)

;;;---------------------------------------------------------------
;;;  c:CCTV-Save
;;;  Save configuration to file
;;;---------------------------------------------------------------
(defun c:CCTV-Save (/ filename)
  (setq filename (getfiled "Save Configuration" "" "txt" 1))
  (if filename
    (progn
      (main-save-parameters filename)
      (princ (strcat "\nConfiguration saved to: " filename))
    )
    (princ "\nSave cancelled.")
  )
  (princ)
)

;;;---------------------------------------------------------------
;;;  c:CCTV-Load
;;;  Load configuration from file
;;;---------------------------------------------------------------
(defun c:CCTV-Load (/ filename)
  (setq filename (getfiled "Load Configuration" "" "txt" 0))
  (if filename
    (progn
      (main-load-parameters filename)
      (princ (strcat "\nConfiguration loaded from: " filename))
    )
    (princ "\nLoad cancelled.")
  )
  (princ)
)

;;;---------------------------------------------------------------
;;;  c:CCTV-RoomPts
;;;  Set room entry points interactively
;;;  Equivalent to original room point selection
;;;---------------------------------------------------------------
(defun c:CCTV-RoomPts (/ pt pts cont)
  (setq pts nil)
  (setq cont T)
  (princ "\n=== Room Entry Points ===")
  (princ "\n  Pick room entry points. Press ESC or Enter to finish.")
  (while cont
    (setq pt (getpoint "\n  Pick room entry point (Enter to finish): "))
    (if pt
      (progn
        (setq pts (cons pt pts))
        (princ (strcat "  Added point: (" (rtos (car pt) 2 0) ","
                       (rtos (cadr pt) 2 0) ")"))
      )
      (setq cont nil)
    )
  )
  (if pts
    (progn
      (setq pts (reverse pts))
      (main-set-room-points pts)
      (princ (strcat "\n  Total " (itoa (length pts)) " room point(s) set."))
    )
    (princ "\n  No room points set.")
  )
  (princ)
)

;;;---------------------------------------------------------------
;;;  c:CCTV-MLINEArea
;;;  Select MLINE area for cable tray processing
;;;  Equivalent to original TextButton14 (mline_set)
;;;---------------------------------------------------------------
(defun c:CCTV-MLINEArea ()
  (princ "\n=== Cable Tray Area Selection ===")
  (main-select-mline-area)
  (if *main-mline-set*
    (princ (strcat "\n  Selected " (itoa (sslength *main-mline-set*)) " MLINE(s)."))
    (princ "\n  No MLINEs selected.")
  )
  (princ)
)

;;;---------------------------------------------------------------
;;;  c:CCTV-EquivAdd
;;;  Add equivalent point pair
;;;  Equivalent to original equivalent point add
;;;---------------------------------------------------------------
(defun c:CCTV-EquivAdd (/ pt1 pt2)
  (princ "\n=== Add Equivalent Point Pair ===")
  (setq pt1 (getpoint "\n  First point: "))
  (if pt1
    (progn
      (setq pt2 (getpoint "\n  Second point: "))
      (if pt2
        (progn
          (equiv-add-pair pt1 pt2)
          (princ "\n  Equivalent point pair added.")
          (princ (strcat "\n  Total pairs: " (itoa (length *equiv-points*)))))
        (princ "\n  Cancelled.")
      )
    )
  )
  (princ)
)

;;;---------------------------------------------------------------
;;;  c:CCTV-EquivClear
;;;  Clear all equivalent point pairs
;;;---------------------------------------------------------------
(defun c:CCTV-EquivClear ()
  (equiv-clear)
  (princ "\n  All equivalent point pairs cleared.")
  (princ)
)

;;;---------------------------------------------------------------
;;;  c:CCTV-LayerProtect
;;;  Add/remove protected layers (bltc)
;;;  Layers in bltc stay visible during gbtc
;;;---------------------------------------------------------------
(defun c:CCTV-LayerProtect (/ action layer)
  (princ "\n=== Protected Layer Management ===")
  (princ (strcat "\n  Current protected layers: "
                 (if *main-bltc*
                   (apply 'strcat (mapcar '(lambda (x) (strcat x " ")) *main-bltc*))
                   "(none)")))
  (setq action (getstring "\n  [A]dd / [R]emove / [C]ancel: "))
  (cond
    ((or (= (strcase action) "A") (= (strcase action) "ADD"))
     (setq layer (getstring "\n  Layer name to protect: "))
     (if (/= layer "")
       (progn
         (main-bltc-add layer)
         (princ (strcat "\n  Layer '" layer "' added to protected list."))
       )
     )
    )
    ((or (= (strcase action) "R") (= (strcase action) "REMOVE"))
     (setq layer (getstring "\n  Layer name to remove: "))
     (if (/= layer "")
       (progn
         (main-bltc-remove layer)
         (princ (strcat "\n  Layer '" layer "' removed from protected list."))
       )
     )
    )
    (T
     (princ "\n  Cancelled.")
    )
  )
  (princ)
)

;;;---------------------------------------------------------------
;;;  c:CCTV-ShowConfig
;;;  Display current configuration
;;;---------------------------------------------------------------
(defun c:CCTV-ShowConfig ()
  (princ "\n\n========================================")
  (princ "\n  Current CCTV Configuration")
  (princ "\n========================================")
  (princ (strcat "\n  Camera blocks: "
                 (if *main-camera-blocks*
                   (apply 'strcat (mapcar '(lambda (x) (strcat x ", ")) *main-camera-blocks*))
                   "(not set)")))
  (princ (strcat "\n  Camera name layers: "
                 (if *main-camera-name-layers*
                   (apply 'strcat (mapcar '(lambda (x) (strcat x ", ")) *main-camera-name-layers*))
                   "(not set)")))
  (princ (strcat "\n  Junction blocks: "
                 (if *main-junction-blocks*
                   (apply 'strcat (mapcar '(lambda (x) (strcat x ", ")) *main-junction-blocks*))
                   "(not set)")))
  (princ (strcat "\n  Cable tray layer: "
                 (if *main-cable-tray-layer* *main-cable-tray-layer* "(not set)")))
  (princ (strcat "\n  Pipe layer: "
                 (if *main-pipe-layer* *main-pipe-layer* "(not set)")))
  (princ (strcat "\n  Cable coefficient: " (rtos *main-cable-coefficient* 2 2)))
  (princ (strcat "\n  Junction bias: " (rtos *main-junction-bias* 2 0)))
  (princ (strcat "\n  Room bias: " (rtos *main-room-bias* 2 0)))
  (princ (strcat "\n  Room points: " (itoa (length *main-room-points*))))
  (princ (strcat "\n  Equiv point pairs: " (itoa (length *equiv-points*))))
  (princ (strcat "\n  MLINE area: "
                 (if *main-mline-set*
                   (strcat (itoa (sslength *main-mline-set*)) " MLINE(s)")
                   "(not set)")))
  (princ (strcat "\n  Protected layers (bltc): "
                 (if *main-bltc*
                   (apply 'strcat (mapcar '(lambda (x) (strcat x ", ")) *main-bltc*))
                   "(none)")))
  (princ "\n========================================\n")
  (princ)
)

;;;---------------------------------------------------------------
;;;  c:CCTV-Help
;;;  Show help
;;;---------------------------------------------------------------
(defun c:CCTV-Help ()
  (princ "\n\n========================================")
  (princ "\n  CCTV System - Command Reference")
  (princ "\n========================================")
  (princ "\n")
  (princ "\n  Workflow Commands:")
  (princ "\n    CCTV-Config      Configure all parameters")
  (princ "\n    CCTV-Run         Run the full workflow")
  (princ "\n    drawCCTV         Run workflow (shortcut)")
  (princ "\n")
  (princ "\n  File Commands:")
  (princ "\n    CCTV-Save        Save configuration to file")
  (princ "\n    CCTV-Load        Load configuration from file")
  (princ "\n    CCTV-ShowConfig  Display current configuration")
  (princ "\n")
  (princ "\n  Setup Commands:")
  (princ "\n    CCTV-RoomPts     Set room entry points")
  (princ "\n    CCTV-MLINEArea   Select cable tray area")
  (princ "\n    CCTV-LayerProtect Manage protected layers")
  (princ "\n")
  (princ "\n  Equivalent Points:")
  (princ "\n    CCTV-EquivAdd    Add equivalent point pair")
  (princ "\n    CCTV-EquivClear  Clear all equivalent points")
  (princ "\n")
  (princ "\n  Testing:")
  (princ "\n    test-all         Run all module tests")
  (princ "\n    test-module      Run specific module test")
  (princ "\n")
  (princ "\n========================================\n")
  (princ)
)

;;;===============================================================
;;;  Provide module info
;;;===============================================================
(princ "\n[M11] gui_handlers.lsp loaded.")
(princ "\n  Commands: CCTV-Config, CCTV-Run, CCTV-Save, CCTV-Load,")
(princ "\n           CCTV-RoomPts, CCTV-MLINEArea, CCTV-LayerProtect,")
(princ "\n           CCTV-EquivAdd, CCTV-EquivClear, CCTV-ShowConfig,")
(princ "\n           CCTV-Help, drawCCTV")
(princ "\n  Usage: Type CCTV-Help for command list")
(princ)

;;;===============================================================
;;;  --- MODULE SEPARATOR ---
;;;  Previous: M11_gui_handlers.lsp
;;;  Next: M12_main.lsp
;;;===============================================================

;;;===============================================================
;;;  M12 - Main Module
;;;  Main entry point and workflow coordination
;;;  Refactored to match original drawCCTV logic
;;;===============================================================
;;;  ENCODING: ANSI (ASCII only, no Chinese characters)
;;;  DEPENDENCIES: M01-M11
;;;===============================================================

;;;---------------------------------------------------------------
;;;  Global Configuration Variables
;;;---------------------------------------------------------------
(setq *main-camera-blocks* nil)         ; List of camera block names
(setq *main-camera-name-layers* nil)    ; List of camera name text layers
(setq *main-junction-blocks* nil)       ; List of junction box block names
(setq *main-cable-tray-layer* nil)      ; Cable tray layer name
(setq *main-pipe-layer* nil)            ; Pipe layer name (optional)
(setq *main-room-points* nil)           ; List of room entry points
(setq *main-equiv-points* nil)          ; List of equivalent point pairs
(setq *main-cable-coefficient* 1.2)     ; Cable length coefficient
(setq *main-junction-bias* 10000.0)     ; Junction box distance bias
(setq *main-room-bias* 25000.0)         ; Room entry distance bias
(setq *main-temp-layer* "TEMP_CCTV")    ; Temporary layer name (was test1)
(setq *main-temp-layer2* "TEMP_CCTV2")  ; Second temp layer (was test2)
(setq *main-mline-set* nil)             ; MLINE selection set (user area pick)
(setq *main-mline-p1* nil)              ; MLINE area corner 1
(setq *main-mline-p2* nil)              ; MLINE area corner 2

;;; bltc - protected layer list (layers to keep visible)
(setq *main-bltc* nil)

;;; hiddenLayers - layers that were off before gbtc ran
(setq *main-hidden-layers* nil)

;;; System variable backup
(setq *main-saved-vars* nil)
(setq *main-old-error* nil)

;;;---------------------------------------------------------------
;;;  main-env-start
;;;  Save system variables and set working environment
;;;  Equivalent to original Berni_Start
;;;---------------------------------------------------------------
(defun main-env-start (/ )
  (setq *main-saved-vars*
    (list (getvar "osmode")
          (getvar "cmdecho")
          (getvar "clayer")
          (getvar "textstyle")
          (getvar "cecolor")
          (getvar "dimstyle")
          (getvar "plinewid")
          (getvar "attdia")
          (getvar "PICKSTYLE")
          (getvar "PEDITACCEPT")
          (getvar "dynmode")
          (getvar "nomutt")))
  (setvar "cmdecho" 0)
  (command-s "_.undo" "_be")
  (setq *main-old-error* *error*)
  (setvar "osmode" 0)
  (setvar "attdia" 0)
  (setvar "PICKSTYLE" 0)
  (setvar "PEDITACCEPT" 1)
  (setvar "dynmode" 0)
  (princ)
)

;;;---------------------------------------------------------------
;;;  main-env-end
;;;  Restore system variables
;;;  Equivalent to original Berni_End
;;;---------------------------------------------------------------
(defun main-env-end (/ )
  (if *main-saved-vars*
    (progn
      (setvar "osmode" (nth 0 *main-saved-vars*))
      (setvar "clayer" (nth 2 *main-saved-vars*))
      (setvar "textstyle" (nth 3 *main-saved-vars*))
      (setvar "cecolor" (nth 4 *main-saved-vars*))
      (setvar "plinewid" (nth 6 *main-saved-vars*))
      (setvar "attdia" (nth 7 *main-saved-vars*))
      (setvar "PICKSTYLE" (nth 8 *main-saved-vars*))
      (setvar "PEDITACCEPT" (nth 9 *main-saved-vars*))
      (setvar "dynmode" (nth 10 *main-saved-vars*))
      (setvar "nomutt" (nth 11 *main-saved-vars*))
      (if *main-old-error* (setq *error* *main-old-error*))
      (command-s "_.undo" "_end")
      (setvar "cmdecho" (nth 1 *main-saved-vars*))
    )
  )
  (princ)
)

;;;---------------------------------------------------------------
;;;  main-gbtc
;;;  Turn off layers NOT in bltc (protected layer list)
;;;  Equivalent to original gbtc
;;;---------------------------------------------------------------
(defun main-gbtc (/ layers tcmb layerName)
  (vl-load-com)
  ;; Record currently hidden layers
  (setq *main-hidden-layers* nil)
  (setq layers (vla-get-layers (vla-get-activedocument (vlax-get-acad-object))))
  (vlax-for layer layers
    (if (= (vla-get-LayerOn layer) :vlax-false)
      (setq *main-hidden-layers* (cons (vla-get-name layer) *main-hidden-layers*))
    )
  )
  ;; Build list of all layers
  (setq tcmb nil)
  (vlax-for layer layers
    (setq tcmb (cons (list (vla-get-name layer) layer) tcmb))
  )
  ;; Turn off layers NOT in bltc
  (if *main-bltc*
    (progn
      (foreach ent *main-bltc*
        (setq tcmb (vl-remove (assoc ent tcmb) tcmb))
      )
      (foreach tcm tcmb
        (vla-put-LayerOn (cadr tcm) :vlax-false)
      )
    )
  )
  (princ)
)

;;;---------------------------------------------------------------
;;;  main-dktc
;;;  Restore layers turned off by main-gbtc
;;;  Equivalent to original dktc
;;;---------------------------------------------------------------
(defun main-dktc ( / )
  (vl-load-com)
  (vlax-for layer (vla-get-layers (vla-get-activedocument (vlax-get-acad-object)))
    (if (and (= (vla-get-LayerOn layer) :vlax-false)
             (null (member (vla-get-name layer) *main-hidden-layers*)))
      (vla-put-LayerOn layer :vlax-true)
    )
  )
  (princ)
)

;;;---------------------------------------------------------------
;;;  main-bltc-add
;;;  Add a layer to protected list
;;;---------------------------------------------------------------
(defun main-bltc-add (layer-name)
  (if (and layer-name (null (member layer-name *main-bltc*)))
    (setq *main-bltc* (cons layer-name *main-bltc*))
  )
)

;;;---------------------------------------------------------------
;;;  main-bltc-remove
;;;  Remove a layer from protected list
;;;---------------------------------------------------------------
(defun main-bltc-remove (layer-name)
  (if layer-name
    (setq *main-bltc* (vl-remove layer-name *main-bltc*))
  )
)

;;;---------------------------------------------------------------
;;;  main-init
;;;  Initialize global variables and create temp layers
;;;---------------------------------------------------------------
(defun main-init ( / )
  (graph-init)
  (equiv-clear)
  (setq *main-bltc* nil)
  ;; Create temp layers
  (if (null (tblsearch "LAYER" *main-temp-layer*))
    (command-s "_.layer" "_m" *main-temp-layer* "_c" "3" *main-temp-layer* "")
  )
  (main-bltc-add *main-temp-layer*)
  (if (null (tblsearch "LAYER" *main-temp-layer2*))
    (command-s "_.layer" "_m" *main-temp-layer2* "_c" "3" *main-temp-layer2* "")
  )
  (main-bltc-add *main-temp-layer2*)
  T
)

;;;---------------------------------------------------------------
;;;  main-cleanup
;;;  Cleanup temporary entities
;;;  Equivalent to original clean_creen
;;;---------------------------------------------------------------
(defun main-cleanup (/ clean-ss i tmp)
  (setq i 0)
  (setq clean-ss (ssget "x" (list (cons 8 (strcat *main-temp-layer* "," *main-temp-layer2*)))))
  (if clean-ss
    (repeat (sslength clean-ss)
      (setq tmp (ssname clean-ss i))
      (command-s "_.erase" tmp "")
      (setq i (1+ i))
    )
  )
  T
)

;;;---------------------------------------------------------------
;;;  main-set-camera-blocks
;;;---------------------------------------------------------------
(defun main-set-camera-blocks (block-list)
  (setq *main-camera-blocks* block-list)
)

;;;---------------------------------------------------------------
;;;  main-set-camera-name-layers
;;;  Set camera name text layers
;;;---------------------------------------------------------------
(defun main-set-camera-name-layers (layer-list)
  (setq *main-camera-name-layers* layer-list)
)

;;;---------------------------------------------------------------
;;;  main-set-junction-blocks
;;;---------------------------------------------------------------
(defun main-set-junction-blocks (block-list)
  (setq *main-junction-blocks* block-list)
)

;;;---------------------------------------------------------------
;;;  main-set-cable-tray-layer
;;;---------------------------------------------------------------
(defun main-set-cable-tray-layer (layer)
  (setq *main-cable-tray-layer* layer)
)

;;;---------------------------------------------------------------
;;;  main-set-pipe-layer
;;;---------------------------------------------------------------
(defun main-set-pipe-layer (layer)
  (setq *main-pipe-layer* layer)
)

;;;---------------------------------------------------------------
;;;  main-set-room-points
;;;  Set room entry points
;;;---------------------------------------------------------------
(defun main-set-room-points (pt-list)
  (setq *main-room-points* pt-list)
)

;;;---------------------------------------------------------------
;;;  main-set-cable-coefficient
;;;---------------------------------------------------------------
(defun main-set-cable-coefficient (coef)
  (setq *main-cable-coefficient* coef)
)

;;;---------------------------------------------------------------
;;;  main-set-biases
;;;---------------------------------------------------------------
(defun main-set-biases (junction-bias room-bias)
  (setq *main-junction-bias* junction-bias)
  (setq *main-room-bias* room-bias)
)

;;;---------------------------------------------------------------
;;;  main-select-mline-area
;;;  Select MLINE area (equivalent to TextButton14)
;;;  User picks two corners of a window
;;;---------------------------------------------------------------
(defun main-select-mline-area ( / p1 p2 filter)
  (setq p1 (getpoint "\nFirst corner of cable tray area: "))
  (if p1
    (progn
      (setq p2 (getcorner p1 "\nOther corner: "))
      (if p2
        (progn
          (setq *main-mline-p1* p1)
          (setq *main-mline-p2* p2)
          (setq filter (list (cons 0 "MLINE")))
          (if *main-cable-tray-layer*
            (setq filter (cons (cons 8 *main-cable-tray-layer*) filter))
          )
          (setq *main-mline-set* (ssget "_c" p1 p2 filter))
          (if *main-mline-set*
            (princ (strcat "\n[main] Selected " (itoa (sslength *main-mline-set*)) " MLINEs."))
            (princ "\n[main] No MLINEs found in selected area.")
          )
        )
      )
    )
  )
)

;;;---------------------------------------------------------------
;;;  main-process-cable-trays
;;;  Process cable tray MLINEs
;;;  Uses mline_set if available (area selection), otherwise entire layer
;;;---------------------------------------------------------------
(defun main-process-cable-trays (/ lines gllst)
  (princ "\n[main] Processing cable trays...")
  (setq gllst (list (cons 0 "LINE") (cons 8 (strcat *main-temp-layer* "," *main-temp-layer2*))))

  ;; Convert MLINEs to LINEs
  (if *main-mline-set*
    (progn
      ;; Use area-selected MLINEs
      (setq lines (mline-convert-selection *main-mline-set* *main-temp-layer*))
      (princ (strcat "\n[main] Converted " (itoa (sslength *main-mline-set*)) " MLINEs (area)."))
    )
    (progn
      ;; Use entire layer
      (if *main-cable-tray-layer*
        (setq lines (mline-process-all (list *main-cable-tray-layer*) *main-temp-layer* nil nil))
      )
    )
  )

  ;; Remove duplicates
  (dup-remove-all nil *main-temp-layer*)

  ;; Break at intersections
  (break-lines-all (list *main-temp-layer* *main-temp-layer2*))

  (princ "\n[main] Cable trays processed.")
  T
)

;;;---------------------------------------------------------------
;;;  main-build-graph
;;;  Build graph from processed lines
;;;---------------------------------------------------------------
(defun main-build-graph (/ ss gllst)
  (princ "\n[main] Building graph...")
  (setq gllst (list (cons 0 "LINE") (cons 8 (strcat *main-temp-layer* "," *main-temp-layer2*))))
  (setq ss (ssget "x" gllst))
  (if (and ss (> (sslength ss) 0))
    (progn
      (graph-build-from-lines ss nil)
      (graph-floyd-compute)
      (princ (strcat "\n[main] Graph: " (itoa (graph-get-node-count)) " nodes, "
                     (itoa (graph-get-edge-count)) " edges."))
      T
    )
    (progn
      (princ "\n[main] Error: No lines found for graph.")
      nil
    )
  )
)

;;;---------------------------------------------------------------
;;;  main-process-room-points
;;;  Process room entry points - three branch logic
;;;  Branch 1: No room + has junctions
;;;  Branch 2: Has room + has junctions
;;;  Branch 3: Has room + no junctions
;;;  Returns: (gjx_list gjx_name_list) where gjx_list = ((pt . dist) ...)
;;;---------------------------------------------------------------
(defun main-process-room-points (junction-ss room-pts / gjx-list gjx-name-list
                                       i ent base-pt proj-pt proj-dist name
                                       y tmp-pt tmp-proj j)
  (setq gjx-list nil)
  (setq gjx-name-list nil)

  (cond
    ;; Branch 1: No room points, has junctions
    ((and (null room-pts) junction-ss)
     (princ "\n[main] Branch 1: No room, has junctions.")
     (setq i 0)
     (repeat (sslength junction-ss)
       (setq ent (ssname junction-ss i))
       (setq base-pt (block-get-base-point ent))
       (if base-pt
         (progn
           (setq proj-info (device-project-to-graph base-pt nil nil))
           (if proj-info
             (progn
               (setq proj-pt (cadr proj-info))
               (setq proj-dist (caddr proj-info))
               ;; Draw connection line on temp layer
               (setvar "clayer" *main-temp-layer*)
               (command-s "_.line" "_non" (trans base-pt 0 1) "_non" (trans proj-pt 0 1) "")
               (setq name (block-get-name-from-text ent nil))
               (if (null name) (setq name "JNX"))
               (setq gjx-list (cons (cons proj-pt proj-dist) gjx-list))
               (setq gjx-name-list (cons name gjx-name-list))
             )
           )
         )
       )
       (setq i (1+ i))
     )
     (setq gjx-list (reverse gjx-list))
     (setq gjx-name-list (reverse gjx-name-list))
    )

    ;; Branch 2: Has room points, has junctions
    ((and room-pts junction-ss)
     (princ "\n[main] Branch 2: Has room, has junctions.")
     ;; Connect room points to graph
     (setq y 0)
     (repeat (length room-pts)
       (setq tmp-pt (nth y room-pts))
       (setq proj-info (device-project-to-graph tmp-pt nil nil))
       (if proj-info
         (progn
           (setq proj-pt (cadr proj-info))
           (setvar "clayer" *main-temp-layer*)
           (command-s "_.line" "_non" (trans tmp-pt 0 1) "_non" (trans proj-pt 0 1) "")
           ;; First room point becomes a target (like junction)
           (if (= y 0)
             (progn
               (setq gjx-list (cons (cons proj-pt (distance tmp-pt proj-pt)) gjx-list))
               (setq gjx-name-list (cons "RoomEntry" gjx-name-list))
             )
           )
         )
       )
       (setq y (1+ y))
     )
     ;; Connect room points to each other (closed loop)
     (setq y 0)
     (repeat (1- (length room-pts))
       (command-s "_.line" "_non" (trans (nth y room-pts) 0 1)
                            "_non" (trans (nth (1+ y) room-pts) 0 1) "")
       (setq y (1+ y))
     )
     (if (> (length room-pts) 1)
       (command-s "_.line" "_non" (trans (nth 0 room-pts) 0 1)
                          "_non" (trans (nth (1- (length room-pts)) room-pts) 0 1) "")
     )
     ;; Process junctions
     (setq i 0)
     (repeat (sslength junction-ss)
       (setq ent (ssname junction-ss i))
       (setq base-pt (block-get-base-point ent))
       (if base-pt
         (progn
           (setq proj-info (device-project-to-graph base-pt nil nil))
           (if proj-info
             (progn
               (setq proj-pt (cadr proj-info))
               (setq proj-dist (caddr proj-info))
               (setvar "clayer" *main-temp-layer*)
               (command-s "_.line" "_non" (trans base-pt 0 1) "_non" (trans proj-pt 0 1) "")
               (setq name (block-get-name-from-text ent nil))
               (if (null name) (setq name "JNX"))
               (setq gjx-list (cons (cons proj-pt proj-dist) gjx-list))
               (setq gjx-name-list (cons name gjx-name-list))
             )
           )
         )
       )
       (setq i (1+ i))
     )
     (setq gjx-list (reverse gjx-list))
     (setq gjx-name-list (reverse gjx-name-list))
    )

    ;; Branch 3: Has room points, no junctions
    ((and room-pts (null junction-ss))
     (princ "\n[main] Branch 3: Has room, no junctions.")
     (setq y 0)
     (repeat (length room-pts)
       (setq tmp-pt (nth y room-pts))
       (setq proj-info (device-project-to-graph tmp-pt nil nil))
       (if proj-info
         (progn
           (setq proj-pt (cadr proj-info))
           (setvar "clayer" *main-temp-layer*)
           (command-s "_.line" "_non" (trans tmp-pt 0 1) "_non" (trans proj-pt 0 1) "")
           (if (= y 0)
             (progn
               (setq gjx-list (cons (cons proj-pt (distance tmp-pt proj-pt)) gjx-list))
               (setq gjx-name-list (cons "RoomEntry" gjx-name-list))
             )
           )
         )
       )
       (setq y (1+ y))
     )
     ;; Connect room points to each other (closed loop)
     (setq y 0)
     (repeat (1- (length room-pts))
       (command-s "_.line" "_non" (trans (nth y room-pts) 0 1)
                          "_non" (trans (nth (1+ y) room-pts) 0 1) "")
       (setq y (1+ y))
     )
     (if (> (length room-pts) 1)
       (command-s "_.line" "_non" (trans (nth 0 room-pts) 0 1)
                          "_non" (trans (nth (1- (length room-pts)) room-pts) 0 1) "")
     )
    )

    (T
     (princ "\n[main] No room points and no junctions.")
    )
  )

  (list gjx-list gjx-name-list)
)

;;;---------------------------------------------------------------
;;;  main-process-equivalent-points
;;;  Process equivalent point pairs
;;;---------------------------------------------------------------
(defun main-process-equivalent-points (/ ss)
  (princ "\n[main] Processing equivalent points...")
  (setq ss (ssget "x" (list (cons 0 "LINE") (cons 8 *main-temp-layer*))))
  (if ss
    (progn
      (equiv-process-all ss)
      ;; Remove duplicates again after adding equiv lines
      (dup-remove-all nil *main-temp-layer*)
      ;; Recompute Floyd after adding equivalent edges
      (graph-floyd-compute)
      (princ "\n[main] Equivalent points processed.")
      T
    )
    nil
  )
)

;;;---------------------------------------------------------------
;;;  main-get-camera-blocks
;;;  Get camera block selection set
;;;---------------------------------------------------------------
(defun main-get-camera-blocks (/ ss result)
  (setq result (ssadd))
  (foreach blk-name *main-camera-blocks*
    (setq ss (block-get-all-on-layer nil blk-name))
    (if ss
      (progn
        (setq i 0)
        (repeat (sslength ss)
          (setq result (ssadd (ssname ss i) result))
          (setq i (1+ i))
        )
      )
    )
  )
  (if (> (sslength result) 0) result nil)
)

;;;---------------------------------------------------------------
;;;  main-get-junction-blocks
;;;  Get junction block selection set
;;;---------------------------------------------------------------
(defun main-get-junction-blocks (/ ss result)
  (setq result (ssadd))
  (foreach blk-name *main-junction-blocks*
    (setq ss (block-get-all-on-layer nil blk-name))
    (if ss
      (progn
        (setq i 0)
        (repeat (sslength ss)
          (setq result (ssadd (ssname ss i) result))
          (setq i (1+ i))
        )
      )
    )
  )
  (if (> (sslength result) 0) result nil)
)

;;;---------------------------------------------------------------
;;;  main-run-workflow
;;;  Execute main workflow - matches original drawCCTV logic
;;;---------------------------------------------------------------
(defun main-run-workflow (/ cameras junctions junction-ss room-result
                              gjx-list gjx-name-list connections
                              workflow-ok draw-pts i ent
                              base-pt proj-pt proj-dist proj-info dev-name blk-name
                              tmp-dis end-dis best-jnx best-name
                              dev-node graph-dist tmp-jnx tmp-jnx-pt
                              tmp-jnx-dist tmp-jnx-name use-bias
                              end-drawlist m_n)
  (princ "\n\n=== CCTV System Workflow ===")

  ;; Step 1: Environment setup (Berni_Start)
  (main-env-start)
  (main-init)
  (setq workflow-ok T)

  ;; Step 2: Get user input for diagram insertion point
  (setq draw-pts (getpoint "\nSystem diagram insertion point: "))
  (if (null draw-pts) (setq draw-pts (list 0.0 0.0 0.0)))

  ;; Step 3: Turn off non-working layers (gbtc)
  (main-gbtc)

  ;; Step 4: Process cable trays (MLINE -> LINE)
  (if (null (main-process-cable-trays))
    (princ "\n[main] Warning: Cable tray processing returned nil.")
  )

  ;; Step 5: Remove duplicates
  (princ "\n[main] Removing duplicates...")
  (dup-remove-all nil *main-temp-layer*)

  ;; Step 6: Process room entry points (three-branch logic)
  (setq junction-ss (main-get-junction-blocks))
  (setq room-result (main-process-room-points junction-ss *main-room-points*))
  (setq gjx-list (car room-result))
  (setq gjx-name-list (cadr room-result))

  ;; Step 7: Process equivalent points
  (main-process-equivalent-points)

  ;; Step 8: Break all intersections
  (princ "\n[main] Breaking intersections...")
  (break-lines-all (list *main-temp-layer* *main-temp-layer2*))

  ;; Step 9: Build graph
  (if (null (main-build-graph))
    (progn
      (princ "\n[main] Workflow aborted: graph build failed.")
      (setq workflow-ok nil)
    )
  )

  ;; Step 10-12: Process cameras
  (if workflow-ok
    (progn
      ;; Turn off cable tray layer during calculation
      (if *main-cable-tray-layer*
        (command-s "_.layer" "_off" *main-cable-tray-layer* "")
      )

      (setq cameras (main-get-camera-blocks))
      (if (null cameras)
        (progn
          (princ "\n[main] Error: No cameras found.")
          (setq workflow-ok nil)
        )
      )

      (if workflow-ok
        (progn
          (princ (strcat "\n[main] Processing " (itoa (sslength cameras)) " cameras..."))
          (setq end-drawlist nil)
          (setq i 0)

          (repeat (sslength cameras)
            (setq ent (ssname cameras i))
            (setq base-pt (block-get-base-point ent))
            (if base-pt
              (progn
                ;; Get camera block name and camera name from nearby text
                (setq blk-name (cdr (assoc 2 (entget ent))))
                (setq dev-name (block-get-name-from-text ent nil))
                (if (null dev-name) (setq dev-name "CAM"))

                ;; Project camera to graph
                (setq proj-info (device-project-to-graph base-pt nil nil))
                (if proj-info
                  (progn
                    (setq proj-pt (cadr proj-info))
                    (setq proj-dist (caddr proj-info))

                    ;; Find best junction
                    ;; Original checks closest 3 by straight-line distance,
                    ;; but checking all is more accurate (improvement)
                    (setq end-dis 1000000.0)
                    (setq best-jnx nil)
                    (setq best-name nil)

                    (setq m_n 0)
                    (repeat (length gjx-list)
                      (setq tmp-jnx (nth m_n gjx-list))
                      (setq tmp-jnx-pt (car tmp-jnx))
                      (setq tmp-jnx-dist (cdr tmp-jnx))
                      (setq tmp-jnx-name (nth m_n gjx-name-list))

                      ;; Calculate shortest path distance
                      (setq dev-node (car proj-info))
                      (setq jnx-node (graph-get-node-index tmp-jnx-pt))
                      (if jnx-node
                        (progn
                          (setq graph-dist (graph-get-distance dev-node jnx-node))
                          (if (and graph-dist (< graph-dist 1e29))
                            (progn
                              ;; Distance formula: (graph_dist + dev_proj + jnx_proj) * coef + bias
                              ;; Original uses different bias for room entry vs junction:
                              ;;   room entry (jifang_ptt): jf_bias (default 25000)
                              ;;   junction box: gjx_bias (default 10000)
                              (setq use-bias
                                (if (= tmp-jnx-name "RoomEntry")
                                  *main-room-bias*
                                  *main-junction-bias*))
                              (setq tmp-dis (+ (* (+ graph-dist proj-dist tmp-jnx-dist)
                                                   *main-cable-coefficient*)
                                             use-bias))
                              (if (< tmp-dis end-dis)
                                (progn
                                  (setq end-dis tmp-dis)
                                  (setq best-jnx tmp-jnx)
                                  (setq best-name tmp-jnx-name)
                                )
                              )
                            )
                          )
                        )
                      )
                      (setq m_n (1+ m_n))
                    )

                    (if best-jnx
                      (progn
                        ;; Add to result: (distance block-name camera-name junction-name)
                        (setq end-drawlist
                          (append end-drawlist
                                  (list (list end-dis blk-name dev-name best-name))))
                        (princ (strcat "\n  CAM: " dev-name " -> " best-name
                                       " dist=" (rtos end-dis 2 0)))
                      )
                      (princ (strcat "\n  CAM: " dev-name " -> NO JUNCTION FOUND"))
                    )
                  )
                  (princ (strcat "\n  CAM: " dev-name " -> projection failed"))
                )
              )
            )
            (setq i (1+ i))
          )

          ;; Turn cable tray layer back on
          (if *main-cable-tray-layer*
            (command-s "_.layer" "_on" *main-cable-tray-layer* "")
          )

          ;; Step 13: Restore layers (dktc)
          (main-dktc)

          ;; Step 14: Classify results by junction
          (setq end-drawlist (sysdiag-classify-by-junction end-drawlist))

          ;; Step 15: Draw system diagram
          (if end-drawlist
            (progn
              (princ (strcat "\n[main] Drawing system diagram with "
                             (itoa (length end-drawlist)) " groups..."))
              (sysdiag-draw-classified draw-pts end-drawlist)
            )
            (princ "\n[main] Warning: No results to draw.")
          )
        )
      )
    )
  )

  ;; Step 16: Cleanup
  (main-cleanup)

  ;; Step 17: Restore environment (Berni_End)
  (main-env-end)

  (if workflow-ok
    (princ "\n[main] Workflow completed.")
    (princ "\n[main] Workflow finished with errors.")
  )
  workflow-ok
)

;;;---------------------------------------------------------------
;;;  c:drawCCTV
;;;  Main command entry
;;;---------------------------------------------------------------
(defun c:drawCCTV ()
  (main-run-workflow)
  (princ)
)

;;;---------------------------------------------------------------
;;;  main-load-parameters
;;;  Load parameters from file
;;;---------------------------------------------------------------
(defun main-load-parameters (filename / params val)
  (setq params (param-load filename))
  (if params
    (progn
      (setq val (param-get params "camera_blocks"))
      (if val (setq *main-camera-blocks* val))

      (setq val (param-get params "camera_name_layers"))
      (if val (setq *main-camera-name-layers* val))

      (setq val (param-get params "junction_blocks"))
      (if val (setq *main-junction-blocks* val))

      (setq val (param-get params "cable_tray_layer"))
      (if val (setq *main-cable-tray-layer* (car val)))

      (setq val (param-get params "pipe_layer"))
      (if val (setq *main-pipe-layer* (car val)))

      (setq val (param-get params "cable_coefficient"))
      (if val (setq *main-cable-coefficient* (atof (car val))))

      (setq val (param-get params "junction_bias"))
      (if val (setq *main-junction-bias* (atof (car val))))

      (setq val (param-get params "room_bias"))
      (if val (setq *main-room-bias* (atof (car val))))

      ;; Restore bltc
      (setq val (param-get params "bltc_layers"))
      (if val (setq *main-bltc* val))

      (princ "\n[main] Parameters loaded.")
      T
    )
    nil
  )
)

;;;---------------------------------------------------------------
;;;  main-save-parameters
;;;  Save parameters to file
;;;---------------------------------------------------------------
(defun main-save-parameters (filename / params)
  (setq params
    (list
      (list "camera_blocks" *main-camera-blocks*)
      (list "camera_name_layers" *main-camera-name-layers*)
      (list "junction_blocks" *main-junction-blocks*)
      (list "cable_tray_layer" (if *main-cable-tray-layer* (list *main-cable-tray-layer*)))
      (list "pipe_layer" (if *main-pipe-layer* (list *main-pipe-layer*)))
      (list "cable_coefficient" (list (rtos *main-cable-coefficient* 2 2)))
      (list "junction_bias" (list (rtos *main-junction-bias* 2 0)))
      (list "room_bias" (list (rtos *main-room-bias* 2 0)))
      (list "bltc_layers" *main-bltc*)
    ))
  (param-save params filename)
)

(princ)

;;;===============================================================
;;;  --- MODULE SEPARATOR ---
;;;  Previous: M12_main.lsp
;;;  Next: M13_test_suite.lsp
;;;===============================================================

;;;===============================================================
;;;  M13 - Test Suite Module
;;;  Comprehensive test suite for all modules
;;;===============================================================
;;;  ENCODING: ANSI (ASCII only, no Chinese characters)
;;;  DEPENDENCIES: M01-M12
;;;===============================================================

;;;---------------------------------------------------------------
;;;  Test Results Storage
;;;---------------------------------------------------------------
(setq *test-results* nil)
(setq *test-total-passed* 0)
(setq *test-total-failed* 0)

;;;---------------------------------------------------------------
;;;  test-log
;;;  Log a test result
;;;---------------------------------------------------------------
(defun test-log (module passed failed)
  (setq *test-results* (cons (list module passed failed) *test-results*))
  (setq *test-total-passed* (+ *test-total-passed* passed))
  (setq *test-total-failed* (+ *test-total-failed* failed))
)

;;;---------------------------------------------------------------
;;;  test-assert-equal
;;;  Assert two values are equal
;;;---------------------------------------------------------------
(defun test-assert-equal (expected actual msg / result)
  (setq result (< (abs (- expected actual)) 0.0001))
  (if result
    (princ (strcat "\n  [PASS] " msg))
    (princ (strcat "\n  [FAIL] " msg " - Expected: " (rtos expected 2 4) 
                   " Got: " (rtos actual 2 4)))
  )
  result
)

;;;---------------------------------------------------------------
;;;  test-assert-true
;;;  Assert value is true
;;;---------------------------------------------------------------
(defun test-assert-true (value msg)
  (if value
    (progn (princ (strcat "\n  [PASS] " msg)) T)
    (progn (princ (strcat "\n  [FAIL] " msg)) nil)
  )
)

;;;---------------------------------------------------------------
;;;  test-assert-not-nil
;;;  Assert value is not nil
;;;---------------------------------------------------------------
(defun test-assert-not-nil (value msg)
  (if value
    (progn (princ (strcat "\n  [PASS] " msg)) T)
    (progn (princ (strcat "\n  [FAIL] " msg " - Value is nil")) nil)
  )
)

;;;---------------------------------------------------------------
;;;  test-module
;;;  Run tests for a specific module
;;;---------------------------------------------------------------
(defun test-module (module-name / result)
  (princ (strcat "\n\n=== Testing " module-name " ==="))
  (setq result
    (cond
      ((= module-name "M01") (test-M01-graph-algorithm))
      ((= module-name "M02") (test-M02-line-utils))
      ((= module-name "M03") (test-M03-mline-converter))
      ((= module-name "M04") (test-M04-duplicate-remover))
      ((= module-name "M05") (test-M05-break-lines))
      ((= module-name "M06") (test-M06-block-utils))
      ((= module-name "M07") (test-M07-device-projection))
      ((= module-name "M08") (test-M08-equivalent-points))
      ((= module-name "M09") (test-M09-system-diagram))
      ((= module-name "M10") (test-M10-parameter-io))
      (T (princ "\nUnknown module") nil)
    ))
  (if result
    (test-log module-name (car result) (cadr result))
  )
  result
)

;;;---------------------------------------------------------------
;;;  test-all
;;;  Run all module tests
;;;---------------------------------------------------------------
(defun test-all (/ old-cmdecho)
  (setq old-cmdecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  
  (setq *test-results* nil)
  (setq *test-total-passed* 0)
  (setq *test-total-failed* 0)
  
  (princ "\n\n========================================")
  (princ "\n    CCTV System Test Suite")
  (princ "\n========================================")
  
  ;; Run all module tests
  (test-module "M01")
  (test-module "M02")
  (test-module "M03")
  (test-module "M04")
  (test-module "M05")
  (test-module "M06")
  (test-module "M07")
  (test-module "M08")
  (test-module "M09")
  (test-module "M10")
  
  ;; Summary
  (test-report)
  
  (setvar "cmdecho" old-cmdecho)
  (list *test-total-passed* *test-total-failed*)
)

;;;---------------------------------------------------------------
;;;  test-report
;;;  Generate test report
;;;---------------------------------------------------------------
(defun test-report ()
  (princ "\n\n========================================")
  (princ "\n    Test Summary")
  (princ "\n========================================")
  
  (foreach result *test-results*
    (princ (strcat "\n" (car result) ": "
                   (itoa (cadr result)) " passed, "
                   (itoa (caddr result)) " failed"))
  )
  
  (princ "\n----------------------------------------")
  (princ (strcat "\nTOTAL: " (itoa *test-total-passed*) " passed, "
                 (itoa *test-total-failed*) " failed"))
  (princ "\n========================================\n")
)

;;;---------------------------------------------------------------
;;;  c:CCTV-Test
;;;  Run test suite command
;;;---------------------------------------------------------------
(defun c:CCTV-Test ()
  (test-all)
  (princ)
)

;;;===============================================================
;;;  Provide module info
;;;===============================================================
(princ "\n[M13] test_suite.lsp loaded.")
(princ "\n  Commands: CCTV-Test")
(princ "\n  Functions: test-all, test-module, test-report")
(princ "\n  Usage: (test-all) or command: CCTV-Test")
(princ)
