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
(setq *block-base-point-cache* nil)

;;;---------------------------------------------------------------
;;;  block-get-insertion-point
;;;  Get the insertion point of a block reference
;;;  Args: ent - entity name (block reference)
;;;  Returns: point list or nil
;;;---------------------------------------------------------------
(defun block-get-insertion-point (ent / elist)
  (if (null ent)
    nil
    (progn
      (setq elist (entget ent))
      (if (= (cdr (assoc 0 elist)) "INSERT")
        (cdr (assoc 10 elist))
        nil
      )
    )
  )
)

;;;---------------------------------------------------------------
;;;  block-get-name
;;;  Get the block definition name
;;;  Args: ent - entity name (block reference)
;;;  Returns: string block name or nil
;;;---------------------------------------------------------------
(defun block-get-name (ent / elist)
  (if (null ent)
    nil
    (progn
      (setq elist (entget ent))
      (if (= (cdr (assoc 0 elist)) "INSERT")
        (cdr (assoc 2 elist))
        nil
      )
    )
  )
)

;;;---------------------------------------------------------------
;;;  block-get-layer
;;;  Get the layer of a block reference
;;;  Args: ent - entity name
;;;  Returns: string layer name
;;;---------------------------------------------------------------
(defun block-get-layer (ent / elist)
  (if (null ent)
    nil
    (progn
      (setq elist (entget ent))
      (cdr (assoc 8 elist))
    )
  )
)

;;;---------------------------------------------------------------
;;;  block-get-scale
;;;  Get the scale factors of a block reference
;;;  Args: ent - entity name
;;;  Returns: (x-scale y-scale z-scale) or nil
;;;---------------------------------------------------------------
(defun block-get-scale (ent / elist)
  (if (null ent)
    nil
    (progn
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
  )
)

;;;---------------------------------------------------------------
;;;  block-get-rotation
;;;  Get the rotation angle of a block reference
;;;  Args: ent - entity name
;;;  Returns: float angle in radians
;;;---------------------------------------------------------------
(defun block-get-rotation (ent / elist)
  (if (null ent)
    nil
    (progn
      (setq elist (entget ent))
      (if (= (cdr (assoc 0 elist)) "INSERT")
        (cdr (assoc 50 elist))
        0.0
      )
    )
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
(defun block-entity-get-area (ent / elist etype obj area r)
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
     (setq area (vl-catch-all-apply
       '(lambda ()
          (if (= (vla-get-closed obj) :vlax-true)
            (vlax-get obj 'area)
            0.0
          )
        )
     ))
     (if (vl-catch-all-error-p area) 0.0 area)
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
     (vl-catch-all-apply '(lambda () (vlax-get obj 'Centroid)))
    )
    (T
     (vl-load-com)
     (setq obj (vlax-ename->vla-object ent))
     (vl-catch-all-apply '(lambda () (vlax-get obj 'InsertionPoint)))
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
(defun block-get-base-point (ent / block-name largest-ent center insertion scale rotation cache-key cached)
  (setq block-name (block-get-name ent))
  (if (null block-name)
    nil
    (progn
      (setq scale (block-get-scale ent))
      (setq rotation (block-get-rotation ent))
      (setq cache-key (strcat block-name "|"
                        (rtos (car scale) 2 4) ","
                        (rtos (cadr scale) 2 4) ","
                        (rtos rotation 2 6)))
      (setq cached (assoc cache-key *block-base-point-cache*))
      (if cached
        (block-transform-point (cdr cached) (block-get-insertion-point ent) scale rotation)
        (progn
          (setq largest-ent (block-find-largest-entity block-name))
          (setq center (if largest-ent
                         (block-entity-get-center largest-ent)
                         (list 0.0 0.0 0.0)))
          (setq *block-base-point-cache*
            (cons (cons cache-key center) *block-base-point-cache*))
          (block-transform-point center (block-get-insertion-point ent) scale rotation)
        )
      )
    )
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
(defun block-transform-point (pt insertion scale rotation / x y sx sy cos-r sin-r x-new y-new)
  (setq x (car pt))
  (setq y (cadr pt))
  (setq sx (car scale))
  (setq sy (cadr scale))
  (setq x (* x sx))
  (setq y (* y sy))
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
(defun block-find-nearest-text (pt radius / pt1 pt2 ss i ent content min-dist nearest dist)
  (setq pt1 (polar pt (* pi 0.75) radius))
  (setq pt2 (polar pt (* pi -0.25) radius))
  (setq ss (vl-catch-all-apply 'ssget (list "_c" pt1 pt2 '((0 . "TEXT,MTEXT")))))
  (if (vl-catch-all-error-p ss)
    (setq ss nil)
  )
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
  (if (null ent)
    nil
    (progn
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
  )
)

;;;---------------------------------------------------------------
;;;  block-text-get-position
;;;  Get the insertion point of a text entity
;;;  Args: ent - entity name
;;;  Returns: point list
;;;---------------------------------------------------------------
(defun block-text-get-position (ent / elist)
  (if (null ent)
    nil
    (progn
      (setq elist (entget ent))
      (cdr (assoc 10 elist))
    )
  )
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
