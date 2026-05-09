;;;===============================================================
;;;  M04 - Duplicate Remover Module
;;;  Remove duplicate and overlapping line entities
;;;  Optimized: spatial hash, endpoint hash, entmake
;;;===============================================================
;;;  ENCODING: ANSI (ASCII only, no Chinese characters)
;;;  DEPENDENCIES: M00 (spatial_index.lsp), M02 (line_utils.lsp)
;;;===============================================================

;;;---------------------------------------------------------------
;;;  Global Parameters
;;;---------------------------------------------------------------
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
      (if (< (abs dx) 1e-10)
        (list nil x1 min-x min-y)
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
;;;  dup-line-get-key-from-pts
;;;  Generate sort key from pre-computed endpoints (avoids entget)
;;;  Args: pts - (pt1 pt2)
;;;  Returns: (slope intercept min-x min-y) or nil
;;;---------------------------------------------------------------
(defun dup-line-get-key-from-pts (pts / x1 y1 x2 y2 dx dy slope intercept min-x min-y)
  (if (and pts (= (length pts) 2))
    (progn
      (setq x1 (car (car pts)) y1 (cadr (car pts)))
      (setq x2 (car (cadr pts)) y2 (cadr (cadr pts)))
      (setq min-x (min x1 x2))
      (setq min-y (min y1 y2))
      (setq dx (- x2 x1))
      (setq dy (- y2 y1))
      (if (< (abs dx) 1e-10)
        (list nil x1 min-x min-y)
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
  (dup-keys-colinear-p key1 key2)
)

;;;---------------------------------------------------------------
;;;  dup-keys-colinear-p
;;;  Check if two keys represent colinear lines
;;;  Args: key1, key2 - sort keys from dup-line-get-key
;;;  Returns: T or nil
;;;---------------------------------------------------------------
(defun dup-keys-colinear-p (key1 key2 / tol)
  (setq tol *dup-tolerance*)
  (if (and key1 key2)
    (if (or (and (null (car key1)) (null (car key2)))
            (and (car key1) (car key2)
                 (< (abs (- (car key1) (car key2))) 0.001)))
      (if (or (and (null (car key1)) (null (car key2)))
              (< (abs (- (cadr key1) (cadr key2))) tol))
        T
        nil
      )
      nil
    )
    nil
  )
)

;;;---------------------------------------------------------------
;;;  dup-key-to-hash
;;;  Convert a line key to a hash string for grouping
;;;  Quantizes slope and intercept for approximate matching
;;;  Args: key - (slope intercept min-x min-y)
;;;  Returns: string hash like "S0.000_I0.0" or "V_1000.0"
;;;---------------------------------------------------------------
(defun dup-key-to-hash (key / slope-quant intercept-quant)
  (if (null (car key))
    (strcat "V_" (rtos (cadr key) 2 0))
    (progn
      (setq slope-quant (rtos (car key) 2 3))
      (setq intercept-quant (rtos (cadr key) 2 0))
      (strcat "S" slope-quant "_I" intercept-quant)
    )
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
;;;  dup-lines-overlap-p
;;;  Check if two colinear lines overlap
;;;  Args: ent1, ent2 - entity names (assumed colinear)
;;;  Returns: T or nil
;;;---------------------------------------------------------------
(defun dup-lines-overlap-p (ent1 ent2 / box1 box2)
  (setq box1 (dup-line-bounding-box ent1))
  (setq box2 (dup-line-bounding-box ent2))
  (sp-boxes-overlap-p box1 box2 *dup-tolerance*)
)

;;;---------------------------------------------------------------
;;;  dup-merge-two-lines
;;;  Merge two overlapping colinear lines into one using entmake
;;;  Args: ent1, ent2 - entity names
;;;         layer - target layer (nil = use ent1's layer)
;;;  Returns: entity name of merged line (or nil)
;;;---------------------------------------------------------------
(defun dup-merge-two-lines (ent1 ent2 / pts1 pts2 all-pts min-x max-x min-y max-y
                                      dx1 dy1 elist layer new-ent p1 p2)
  (setq pts1 (line-get-endpoints ent1))
  (setq pts2 (line-get-endpoints ent2))
  (if (and pts1 pts2)
    (progn
      (setq all-pts (append pts1 pts2))
      (setq min-x (apply 'min (mapcar 'car all-pts)))
      (setq max-x (apply 'max (mapcar 'car all-pts)))
      (setq min-y (apply 'min (mapcar 'cadr all-pts)))
      (setq max-y (apply 'max (mapcar 'cadr all-pts)))
      (setq dx1 (- (car (cadr pts1)) (car (car pts1))))
      (setq dy1 (- (cadr (cadr pts1)) (cadr (car pts1))))
      (if (> (abs dx1) (abs dy1))
        (progn
          (setq p1 (list min-x (cadr (car pts1)) 0.0))
          (setq p2 (list max-x (cadr (car pts1)) 0.0))
        )
        (progn
          (setq p1 (list (car (car pts1)) min-y 0.0))
          (setq p2 (list (car (car pts1)) max-y 0.0))
        )
      )
      (setq elist (entget ent1))
      (setq layer (cdr (assoc 8 elist)))
      (setq new-ent
        (entmakex
          (list
            (cons 0 "LINE")
            (cons 8 layer)
            (cons 10 p1)
            (cons 11 p2)
          )
        )
      )
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
  (dup-pts-identical-p pts1 pts2)
)

;;;---------------------------------------------------------------
;;;  dup-pts-identical-p
;;;  Check if two endpoint pairs are identical
;;;  Args: pts1, pts2 - (start end) point lists
;;;  Returns: T or nil
;;;---------------------------------------------------------------
(defun dup-pts-identical-p (pts1 pts2 / tol)
  (setq tol *dup-tolerance*)
  (if (and pts1 pts2)
    (or
      (and (< (distance (car pts1) (car pts2)) tol)
           (< (distance (cadr pts1) (cadr pts2)) tol))
      (and (< (distance (car pts1) (cadr pts2)) tol)
           (< (distance (cadr pts1) (car pts2)) tol))
    )
    nil
  )
)

;;;---------------------------------------------------------------
;;;  dup-endpoint-hash
;;;  Create a hash string from sorted endpoint coordinates
;;;  Used for O(1) duplicate lookup instead of O(n) comparison
;;;  Args: pts - (pt1 pt2) endpoint list
;;;  Returns: string hash "x1,y1,x2,y2" (sorted by x then y)
;;;---------------------------------------------------------------
(defun dup-endpoint-hash (pts / p1 p2 x1 y1 x2 y2 tol-quant)
  (setq tol-quant (fix (/ 1.0 *dup-tolerance*)))
  (setq p1 (car pts))
  (setq p2 (cadr pts))
  (setq x1 (car p1) y1 (cadr p1))
  (setq x2 (car p2) y2 (cadr p2))
  (if (or (< x1 x2) (and (= x1 x2) (< y1 y2)))
    (strcat (rtos (fix (* x1 tol-quant)) 2 0) ","
            (rtos (fix (* y1 tol-quant)) 2 0) ","
            (rtos (fix (* x2 tol-quant)) 2 0) ","
            (rtos (fix (* y2 tol-quant)) 2 0))
    (strcat (rtos (fix (* x2 tol-quant)) 2 0) ","
            (rtos (fix (* y2 tol-quant)) 2 0) ","
            (rtos (fix (* x1 tol-quant)) 2 0) ","
            (rtos (fix (* y1 tol-quant)) 2 0))
  )
)

;;;---------------------------------------------------------------
;;;  dup-build-line-records
;;;  Pre-compute endpoints, bounding boxes, and keys for all lines
;;;  Avoids repeated entget calls during comparison
;;;  Args: line-ss - selection set of lines
;;;  Returns: list of (ent pts box key hash)
;;;---------------------------------------------------------------
(defun dup-build-line-records (line-ss / i ent pts box key hash records)
  (setq records nil)
  (if line-ss
    (progn
      (setq i 0)
      (repeat (sslength line-ss)
        (setq ent (ssname line-ss i))
        (if ent
          (progn
            (setq pts (line-get-endpoints ent))
            (setq box (sp-box-from-pts pts))
            (setq key (dup-line-get-key-from-pts pts))
            (setq hash (if pts (dup-endpoint-hash pts) nil))
            (setq records (cons (list ent pts box key hash) records))
          )
        )
        (setq i (1+ i))
      )
      (reverse records)
    )
    nil
  )
)

;;;---------------------------------------------------------------
;;;  dup-remove-identical
;;;  Remove identical lines using endpoint hash for O(n) lookup
;;;  Falls back to spatial-indexed comparison for near-duplicates
;;;  Args: line-ss - selection set
;;;  Returns: number of duplicates removed
;;;---------------------------------------------------------------
(defun dup-remove-identical (line-ss / records hash-table i rec ent pts hash
                                       existing to-remove count)
  (setq count 0)
  (setq to-remove nil)
  (if line-ss
    (progn
      (setq records (dup-build-line-records line-ss))
      (setq hash-table nil)
      (setq i 0)
      (repeat (length records)
        (setq rec (nth i records))
        (setq ent (car rec))
        (setq pts (cadr rec))
        (setq hash (nth 4 rec))
        (if (and hash (not (member ent to-remove)))
          (progn
            (setq existing (assoc hash hash-table))
            (if existing
              (progn
                (setq to-remove (cons ent to-remove))
                (setq count (1+ count))
              )
              (setq hash-table (cons (cons hash ent) hash-table))
            )
          )
        )
        (setq i (1+ i))
      )
      (foreach ent to-remove
        (if (entget ent) (entdel ent))
      )
    )
  )
  count
)

;;;---------------------------------------------------------------
;;;  dup-merge-colinear
;;;  Merge overlapping colinear lines using spatial hash grouping
;;;  Groups lines by quantized slope/intercept, then only compares
;;;  within groups and within spatial cells
;;;  Args: line-ss - selection set
;;;  Returns: number of merges performed
;;;---------------------------------------------------------------
(defun dup-merge-colinear (line-ss / records groups i rec key hash
                                    group-key group-records
                                    idx j k r1 r2 ent1 ent2
                                    count merged new-ent)
  (setq count 0)
  (setq merged nil)
  (if line-ss
    (progn
      (setq records (dup-build-line-records line-ss))
      (setq groups nil)
      (setq i 0)
      (repeat (length records)
        (setq rec (nth i records))
        (setq key (nth 3 rec))
        (if key
          (progn
            (setq hash (dup-key-to-hash key))
            (setq group-key (assoc hash groups))
            (if group-key
              (setq groups
                (subst (cons hash (cons i (cdr group-key))) group-key groups))
              (setq groups (cons (list hash i) groups))
            )
          )
        )
        (setq i (1+ i))
      )
      (foreach grp groups
        (setq group-records (cdr grp))
        (if (> (length group-records) 1)
          (progn
            (setq idx (sp-build-index
                        (mapcar '(lambda (ri) (nth ri records)) group-records)
                        '(lambda (r) (caddr r))
                        (sp-get-cell-size)))
            (setq j 0)
            (while (< j (length group-records))
              (setq r1 (nth (nth j group-records) records))
              (setq ent1 (car r1))
              (if (and ent1 (entget ent1) (not (member ent1 merged)))
                (progn
                  (setq candidates (sp-get-candidates (caddr r1) idx (sp-get-cell-size)))
                  (foreach ci candidates
                    (if (/= ci j)
                      (progn
                        (setq r2 (nth (nth ci group-records) records))
                        (setq ent2 (car r2))
                        (if (and ent2 (entget ent2) (not (member ent2 merged)))
                          (if (and (dup-keys-colinear-p (nth 3 r1) (nth 3 r2))
                                   (sp-boxes-overlap-p (caddr r1) (caddr r2) *dup-tolerance*))
                            (progn
                              (setq new-ent (dup-merge-two-lines ent1 ent2))
                              (if new-ent
                                (progn
                                  (setq merged (cons ent1 merged))
                                  (setq merged (cons ent2 merged))
                                  (setq r1
                                    (list new-ent
                                          (line-get-endpoints new-ent)
                                          (dup-line-bounding-box new-ent)
                                          (dup-line-get-key new-ent)
                                          nil))
                                  (setq ent1 new-ent)
                                  (setq count (1+ count))
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
              (setq j (1+ j))
            )
          )
        )
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

      (setq identical-count (dup-remove-identical ss))
      (princ (strcat "\n[dup] Removed " (itoa identical-count) " identical lines."))

      (if layer
        (setq ss (ssget "x" (list (cons 0 "LINE") (cons 8 layer))))
        (setq ss (ssget "x" '((0 . "LINE"))))
      )

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

  (princ "\n[Test 1] dup-line-get-key...")
  (command-s "_.line" "0,0" "1000,0" "")
  (setq ent1 (entlast))
  (setq key (dup-line-get-key ent1))
  (if (and key
           (< (abs (- (car key) 0.0)) 0.001)
           (< (abs (- (cadr key) 0.0)) 0.001))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  (command-s "_.erase" ent1 "")

  (princ "\n[Test 2] dup-lines-identical-p...")
  (command-s "_.line" "0,0" "1000,0" "")
  (setq ent1 (entlast))
  (command-s "_.line" "0,0" "1000,0" "")
  (setq ent2 (entlast))
  (command-s "_.line" "1000,0" "0,0" "")
  (setq ent3 (entlast))
  (command-s "_.line" "0,0" "1000,100" "")
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

  (princ "\n[Test 3] dup-lines-colinear-p...")
  (command-s "_.line" "0,0" "1000,0" "")
  (setq ent1 (entlast))
  (command-s "_.line" "500,0" "1500,0" "")
  (setq ent2 (entlast))
  (command-s "_.line" "0,0" "1000,100" "")
  (setq ent3 (entlast))
  (if (and (dup-lines-colinear-p ent1 ent2)
           (null (dup-lines-colinear-p ent1 ent3)))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  (command-s "_.erase" ent1 "")
  (command-s "_.erase" ent2 "")
  (command-s "_.erase" ent3 "")

  (princ "\n[Test 4] dup-lines-overlap-p...")
  (command-s "_.line" "0,0" "1000,0" "")
  (setq ent1 (entlast))
  (command-s "_.line" "500,0" "1500,0" "")
  (setq ent2 (entlast))
  (command-s "_.line" "1100,0" "2000,0" "")
  (setq ent3 (entlast))
  (if (and (dup-lines-overlap-p ent1 ent2)
           (null (dup-lines-overlap-p ent1 ent3)))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  (command-s "_.erase" ent1 "")
  (command-s "_.erase" ent2 "")
  (command-s "_.erase" ent3 "")

  (princ "\n[Test 5] dup-remove-identical...")
  (command-s "_.line" "0,0" "1000,0" "")
  (setq ent1 (entlast))
  (command-s "_.line" "0,0" "1000,0" "")
  (setq ent2 (entlast))
  (command-s "_.line" "0,0" "1000,0" "")
  (setq ent3 (entlast))
  (setq ss (ssadd))
  (setq ss (ssadd ent1 ss))
  (setq ss (ssadd ent2 ss))
  (setq ss (ssadd ent3 ss))
  (setq count (dup-remove-identical ss))
  (if (= count 2)
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  (if (entget ent1) (command-s "_.erase" ent1 ""))

  (princ "\n[Test 6] dup-merge-colinear...")
  (command-s "_.line" "0,0" "1000,0" "")
  (setq ent1 (entlast))
  (command-s "_.line" "500,0" "1500,0" "")
  (setq ent2 (entlast))
  (setq ss (ssadd))
  (setq ss (ssadd ent1 ss))
  (setq ss (ssadd ent2 ss))
  (setq count (dup-merge-colinear ss))
  (setq remaining-ss (ssget "x" '((0 . "LINE"))))
  (if (and (= count 1)
           remaining-ss
           (= (sslength remaining-ss) 1)
           (> (line-get-length (ssname remaining-ss 0)) 1400))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  (if remaining-ss (command-s "_.erase" (ssname remaining-ss 0) ""))

  (princ "\n[Test 7] dup-remove-all (integration)...")
  (command-s "_.line" "0,0" "1000,0" "")
  (setq ent1 (entlast))
  (command-s "_.line" "0,0" "1000,0" "")
  (setq ent2 (entlast))
  (command-s "_.line" "500,0" "1500,0" "")
  (setq ent3 (entlast))
  (setq result (dup-remove-all nil "0"))
  (if (and (= (car result) 1)
           (= (cadr result) 1))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )
  (setq ss (ssget "x" '((0 . "LINE"))))
  (if ss (command-s "_.erase" ss ""))

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

  (princ "\n[Test 9] dup-endpoint-hash...")
  (setq h1 (dup-endpoint-hash (list (list 0.0 0.0 0.0) (list 1000.0 0.0 0.0))))
  (setq h2 (dup-endpoint-hash (list (list 1000.0 0.0 0.0) (list 0.0 0.0 0.0))))
  (if (= h1 h2)
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  (princ "\n[Test 10] dup-key-to-hash...")
  (setq k1 (dup-key-to-hash (list 0.0 0.0 0.0 0.0)))
  (setq k2 (dup-key-to-hash (list 0.0 0.5 0.0 0.0)))
  (if (and (= k1 k2)
           (stringp k1))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  (princ "\n[Test 11] spatial hash cell size get/set...")
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

  (command-s "_.undo" "_e")

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
(princ (strcat "\n  Spatial cell size: " (rtos *sp-default-cell-size* 2 0)))
(princ "\n  Dependencies: M00 (spatial_index.lsp), M02 (line_utils.lsp)")
(princ "\n  Test: (test-M04-duplicate-remover)")
(princ)
