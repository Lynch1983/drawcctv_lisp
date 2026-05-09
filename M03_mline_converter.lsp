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
