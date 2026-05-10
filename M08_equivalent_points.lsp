;;;===============================================================
;;;  M08 - Equivalent Points Module
;;;  Handle equivalent connectivity points
;;;===============================================================
;;;  ENCODING: ANSI (ASCII only, no Chinese characters)
;;;  DEPENDENCIES: M01, M02, M07
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
  (if (or (null pt1) (null pt2))
    (progn
      (princ "\n[equiv] Error: nil point passed to equiv-add-pair")
      nil
    )
    (progn
      (setq id *equiv-next-id*)
      (setq *equiv-pairs* (cons (list id (list pt1 pt2)) *equiv-pairs*))
      (setq *equiv-next-id* (1+ *equiv-next-id*))
      id
    )
  )
)

;;;---------------------------------------------------------------
;;;  equiv-get-all-pairs
;;;  Get all equivalent point pairs
;;;  Returns: list of (id (pt1 pt2))
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
;;;        layer-name - layer name for drawing connection lines
;;;  Returns: number of connections made
;;;---------------------------------------------------------------
(defun equiv-process-all (line-ss layer-name / count)
  (if (null line-ss)
    (progn
      (princ "\n[equiv] Error: nil selection set in equiv-process-all")
      0
    )
    (progn
      (setq count 0)
      (princ (strcat "\n[equiv] Processing " (itoa (length *equiv-pairs*)) " equivalent pairs..."))
      (foreach pair *equiv-pairs*
        (if (equiv-connect-pair (cadr pair) line-ss layer-name)
          (setq count (1+ count))
        )
      )
      (princ (strcat "\n[equiv] Connected " (itoa count) " pairs."))
      count
    )
  )
)

;;;---------------------------------------------------------------
;;;  equiv-connect-pair
;;;  Connect one equivalent pair to the graph
;;;  Args: pt-pair - (pt1 pt2)
;;;         line-ss - selection set of lines
;;;         layer-name - layer name for drawing connection LINE
;;;  Returns: T if connected, nil otherwise
;;;---------------------------------------------------------------
(defun equiv-connect-pair (pt-pair line-ss layer-name / pt1 pt2 proj1 proj2)
  (setq pt1 (car pt-pair))
  (setq pt2 (cadr pt-pair))
  (setq proj1 (device-find-nearest-line pt1 line-ss nil))
  (if (null proj1)
    (progn
      (princ "\n[equiv] Warning: device-find-nearest-line returned nil for pt1")
      nil
    )
    (progn
      (setq proj2 (device-find-nearest-line pt2 line-ss nil))
      (if (null proj2)
        (progn
          (princ "\n[equiv] Warning: device-find-nearest-line returned nil for pt2")
          nil
        )
        (progn
          (entmakex (list (cons 0 "LINE") (cons 8 layer-name)
                          (cons 10 (cadr proj1)) (cons 11 (cadr proj2))))
          T
        )
      )
    )
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
