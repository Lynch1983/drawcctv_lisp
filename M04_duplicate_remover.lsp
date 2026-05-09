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
