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
  (setq key (cons (min nodeA nodeB) (max nodeA nodeB)))
  (if (assoc key *graph-edges*)
    nil
    (progn
      (setq *graph-edges* (cons (cons key weight) *graph-edges*))
      (setq adjA (assoc nodeA *graph-adj*))
      (if adjA
        (setq *graph-adj*
          (subst (cons nodeA (cons (cons nodeB weight) (cdr adjA))) adjA *graph-adj*)
        )
        (setq *graph-adj* (cons (list nodeA (cons nodeB weight)) *graph-adj*))
      )
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
  (if (null ent)
    nil
    (progn
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
  )
)

;;;---------------------------------------------------------------
;;;  graph-get-polyline-points
;;;  Get all vertex points from a polyline
;;;  Args: ent - entity name
;;;  Returns: list of points
;;;---------------------------------------------------------------
(defun graph-get-polyline-points (ent / obj pts n i pt result)
  (vl-load-com)
  (setq result
    (vl-catch-all-apply
      '(lambda ()
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
    )
  )
  (if (vl-catch-all-error-p result)
    nil
    result
  )
)

;;;---------------------------------------------------------------
;;;  graph-get-arc-points
;;;  Get start and end points from an arc
;;;  Args: ent - entity name
;;;  Returns: list of 2 points (start, end)
;;;---------------------------------------------------------------
(defun graph-get-arc-points (ent / obj center radius sa ea result)
  (vl-load-com)
  (setq result
    (vl-catch-all-apply
      '(lambda ()
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
    )
  )
  (if (vl-catch-all-error-p result)
    nil
    result
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
            (if (and pts (= (length pts) 2))
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
                (setq row (cons 1e30 row))
              )
            )
          )
          (setq j (1+ j))
        )
        (setq *graph-dist* (cons (reverse row) *graph-dist*))
        (setq i (1+ i))
      )
      (setq *graph-dist* (reverse *graph-dist*))

      (if (null *graph-dist*)
        (progn (princ "\n[graph-floyd] Error: distance matrix is nil.") nil)
        (progn
          (princ "\n[graph-floyd] Running optimization...")

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
    (if (null *graph-dist*)
      (progn (princ "\n[graph] Error: distance matrix is nil.") nil)
      (if (and (< nodeA *graph-node-count*)
               (< nodeB *graph-node-count*))
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
;;;  graph-distance-via-edge
;;;  Compute distance from a projection point to a target node
;;;  via the two endpoints of the edge the projection lies on.
;;;  This avoids needing to add the projection point as a new node
;;;  and re-run Floyd-Warshall.
;;;
;;;  Formula: dist = proj_dist + min(
;;;    graph_dist(ep1_node, target_node) + dist(proj_pt, ep1_pt),
;;;    graph_dist(ep2_node, target_node) + dist(proj_pt, ep2_pt)
;;;  )
;;;
;;;  Args: proj-pt   - projection point on the edge
;;;         proj-dist - distance from original point to proj-pt
;;;         edge-ent  - the LINE entity the projection lies on
;;;         target-node - integer node index of the target
;;;  Returns: float total distance or nil
;;;---------------------------------------------------------------
(defun graph-distance-via-edge (proj-pt proj-dist edge-ent target-node /
                                 endpts ep1 ep2 ep1-node ep2-node
                                 d1 d2 dist-via-ep1 dist-via-ep2)
  (if (null edge-ent)
    nil
    (progn
      (setq endpts (line-get-endpoints edge-ent))
      (if (null endpts)
        nil
        (progn
          (setq ep1 (car endpts))
          (setq ep2 (cadr endpts))
          (setq ep1-node (graph-get-node-index ep1))
          (setq ep2-node (graph-get-node-index ep2))
          (if (or (null ep1-node) (null ep2-node))
            nil
            (progn
              (setq d1 (graph-get-distance ep1-node target-node))
              (setq d2 (graph-get-distance ep2-node target-node))
              (setq dist-via-ep1 nil)
              (setq dist-via-ep2 nil)
              (if (and d1 (< d1 1e29))
                (setq dist-via-ep1 (+ proj-dist (distance proj-pt ep1) d1))
              )
              (if (and d2 (< d2 1e29))
                (setq dist-via-ep2 (+ proj-dist (distance proj-pt ep2) d2))
              )
              (cond
                ((and dist-via-ep1 dist-via-ep2)
                 (min dist-via-ep1 dist-via-ep2))
                (dist-via-ep1 dist-via-ep1)
                (dist-via-ep2 dist-via-ep2)
                (T nil)
              )
            )
          )
        )
      )
    )
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
            (setq tmp-pt (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list ent pt)))
            (if (and tmp-pt (not (vl-catch-all-error-p tmp-pt)))
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
(defun graph-project-point (pt line-ss gllst / nearest nidx)
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
      (princ "\n[graph-assign] Step 1: Projecting devices...")
      (setq dev-nodes nil)
      (foreach dp device-list
        (setq dev-info (graph-project-point dp nil nil))
        (if (and dev-info (= (length dev-info) 3) (numberp (car dev-info)))
          (setq dev-nodes (cons (cons dp dev-info) dev-nodes))
        )
      )
      (princ (strcat "\n[graph-assign] Projected " (itoa (length dev-nodes)) " devices"))

      (princ "\n[graph-assign] Step 2: Projecting junctions...")
      (setq jnx-nodes nil)
      (foreach jp junction-list
        (setq jnx-info (graph-project-point jp nil nil))
        (if (and jnx-info (= (length jnx-info) 3) (numberp (car jnx-info)))
          (setq jnx-nodes (cons (cons jp jnx-info) jnx-nodes))
        )
      )
      (princ (strcat "\n[graph-assign] Projected " (itoa (length jnx-nodes)) " junctions"))

      (princ "\n[graph-assign] Step 3: Matching...")
      (setq result nil)
      (foreach dev-entry (reverse dev-nodes)
        (setq dev-pt (car dev-entry))
        (setq dev-info (cdr dev-entry))
        (if (and dev-info (= (length dev-info) 3))
          (progn
            (setq dev-node (car dev-info))
            (setq dev-dist (caddr dev-info))
            (if (numberp dev-node)
              (progn
                (setq best-jnx nil best-total 1e30)
                (foreach jnx-entry (reverse jnx-nodes)
                  (setq jnx-pt (car jnx-entry))
                  (setq jnx-info (cdr jnx-entry))
                  (if (and jnx-info (= (length jnx-info) 3))
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
