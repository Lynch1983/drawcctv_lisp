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
(setq *sysdiag-column-spacing* 12000.0)
(setq *sysdiag-block-offset* 6000.0)
(setq *sysdiag-text-height* 250.0)
(setq *sysdiag-rect-width* 1000.0)
(setq *sysdiag-init-y-offset* 1019.0)
(setq *sysdiag-init-angle* (/ pi -16.4))
(setq *sysdiag-min-row-spacing* 700.0)
(setq *sysdiag-cable-label-prefix* "SXTGDDL")
(setq *sysdiag-dist-unit* 1000.0)

(setq *sysdiag-gj-heigh* nil)
(setq *sysdiag-gj-length* nil)

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
(defun sysdiag-classify-by-junction (lst / i end end-drawlist tmp-lst tmp-gjx catch-result)
  (if (null lst)
    nil
    (progn
      (setq i 0)
      (setq end nil)
      (setq end-drawlist lst)
      (while end-drawlist
        (setq tmp-lst (list (car end-drawlist)))
        (setq catch-result
          (vl-catch-all-apply
            '(lambda ()
              (nth 3 (car tmp-lst))
            )
          )
        )
        (if (vl-catch-all-error-p catch-result)
          (setq tmp-gjx nil)
          (setq tmp-gjx catch-result)
        )
        (setq end-drawlist (cdr end-drawlist))
        (setq i 0)
        (repeat (length end-drawlist)
          (setq catch-result
            (vl-catch-all-apply
              '(lambda ()
                (nth 3 (nth i end-drawlist))
              )
            )
          )
          (if (and (not (vl-catch-all-error-p catch-result))
                   (= catch-result tmp-gjx))
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
  )
)

;;;---------------------------------------------------------------
;;;  sysdiag-insert-block
;;;  Insert a camera block at specified point, center-align
;;;  Equivalent to original sub1_insert_block
;;;  Args: pts  - insertion point
;;;        name - block name
;;;  Side effects: sets *sysdiag-gj-heigh* and *sysdiag-gj-length*
;;;---------------------------------------------------------------
(defun sysdiag-insert-block (pts name / tmp-block p1 p2 gj-pts gj-jidian bbox-result)
  (command-s "_.insert" name pts "" "" 0)
  (setq tmp-block (entlast))
  (if (null tmp-block)
    (progn
      (princ "\n[sysdiag] Error: entlast returned nil after block insert")
      nil
    )
    (progn
      (setq bbox-result
        (vl-catch-all-apply
          'vla-GetBoundingBox
          (list (vlax-ename->vla-object tmp-block) 'p1 'p2)
        )
      )
      (if (vl-catch-all-error-p bbox-result)
        (progn
          (princ (strcat "\n[sysdiag] Error: GetBoundingBox failed - "
                         (vl-catch-all-error-message bbox-result)))
          (setq *sysdiag-gj-heigh* nil)
          (setq *sysdiag-gj-length* nil)
          nil
        )
        (progn
          (setq p1 (vlax-safearray->list p1))
          (setq p2 (vlax-safearray->list p2))
          (setq *sysdiag-gj-heigh* (- (cadr p2) (cadr p1)))
          (setq *sysdiag-gj-length* (- (car p2) (car p1)))
          (setq gj-pts (polar p1 (/ pi 2) (/ *sysdiag-gj-heigh* 2.0)))
          (setq gj-jidian (cdr (assoc 10 (entget tmp-block))))
          (command-s "_.move" (entlast) "" gj-pts gj-jidian)
          (princ)
        )
      )
    )
  )
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
  (if (null lst)
    (progn
      (princ "\n[sysdiag] Error: nil list passed to sysdiag-draw-classified")
      nil
    )
    (progn
      (setq cm (getvar "cmdecho"))
      (setq os (getvar "osmode"))
      (setvar "cmdecho" 0)
      (setvar "osmode" 0)
      (setvar "clayer" "0")
      (setq end lst)
      (setq i 0)
      (setq m 0)

      (repeat (length end)
        (setq tmp-draw (nth m end))
        (setq tmp-draw
          (vl-sort tmp-draw
            '(lambda (e1 e2)
               (< (atof (sysdiag-extract-number (caddr e1)))
                  (atof (sysdiag-extract-number (caddr e2)))))))

        (setq htd-p1 (polar htd *sysdiag-init-angle* *sysdiag-init-y-offset*))

        (repeat (length tmp-draw)
          (setq end-p1 (polar htd-p1 0 *sysdiag-block-offset*))
          (command-s "_.pline" htd-p1 end-p1 "")

          (setq tmp-drawblock (nth i tmp-draw))
          (setq tmp-drawblockname (nth 1 tmp-drawblock))

          (setq tmp-textpts (polar htd-p1 0.2 250.0))
          (setq tmp-textstr
            (strcat *sysdiag-cable-label-prefix* "-"
                    (rtos (fix (/ (nth 0 tmp-drawblock) *sysdiag-dist-unit*)) 2 0)
                    "m"))

          (sysdiag-insert-block end-p1 tmp-drawblockname)

          (setq tmp-namepts (polar end-p1 0 (+ *sysdiag-gj-length* 100.0)))
          (command-s "_.text" tmp-textpts *sysdiag-text-height* 0 tmp-textstr)
          (command-s "_.text" tmp-namepts *sysdiag-text-height* 0 (nth 2 tmp-drawblock))

          (setq i (1+ i))

          (if (> (+ *sysdiag-gj-heigh* 100.0) *sysdiag-min-row-spacing*)
            (setq htd-p1 (polar htd-p1 (* pi 1.5) (+ *sysdiag-gj-heigh* 100.0)))
            (setq htd-p1 (polar htd-p1 (* pi 1.5) *sysdiag-min-row-spacing*))
          )
        )

        (setq htd-pt1 (polar htd (* pi 1.5)
                             (+ (- (cadr htd) (cadr htd-p1)) 200.0)))
        (setq htd-pt2 (polar htd-pt1 0 *sysdiag-rect-width*))
        (setq htd-pt3 (polar htd-pt2 (* pi 0.5)
                             (+ (- (cadr htd) (cadr htd-p1)) 200.0)))
        (command-s "_.pline" htd htd-pt1 htd-pt2 htd-pt3 "c")

        (command-s "_.text" (polar htd (* pi 0.7) 400.0)
                   *sysdiag-text-height* 0 (nth 3 (car tmp-draw)))

        (setq m (1+ m))
        (setq i 0)
        (setq htd (polar htd 0 *sysdiag-column-spacing*))
      )

      (setq *sysdiag-gj-heigh* nil)
      (setvar "cmdecho" cm)
      (setvar "osmode" os)
      T
    )
  )
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
  (if (null lst)
    (progn
      (princ "\n[sysdiag] Error: nil list passed to sysdiag-draw-hjx")
      nil
    )
    (progn
      (setq cm (getvar "cmdecho"))
      (setq os (getvar "osmode"))
      (setvar "cmdecho" 0)
      (setvar "osmode" 0)
      (setvar "clayer" "0")
      (setq end lst)
      (setq i 0)
      (setq m 0)

      (setq end (vl-sort end '(lambda (e1 e2) (< (cadr e1) (cadr e2)))))

      (setq htd-p1 (polar htd *sysdiag-init-angle* *sysdiag-init-y-offset*))

      (repeat (length end)
        (setq end-p1 (polar htd-p1 0 *sysdiag-block-offset*))
        (command-s "_.pline" htd-p1 end-p1 "")

        (setq tmp-drawblock (nth m end))
        (setq tmp-drawblockname
          (cdr (assoc 2 (entget (nth 2 tmp-drawblock)))))

        (setq tmp-textpts (polar htd-p1 0.2 250.0))
        (setq tmp-textstr
          (strcat "HJXXL-"
                  (rtos (fix (/ (nth 0 tmp-drawblock) *sysdiag-dist-unit*)) 2 0)
                  "m"))

        (sysdiag-insert-block end-p1 tmp-drawblockname)

        (setq tmp-namepts (polar end-p1 0 (+ *sysdiag-gj-length* 100.0)))
        (command-s "_.text" tmp-textpts *sysdiag-text-height* 0 tmp-textstr)
        (command-s "_.text" tmp-namepts *sysdiag-text-height* 0 (nth 1 tmp-drawblock))

        (setq i (1+ i))

        (if (> (+ *sysdiag-gj-heigh* 100.0) 550.0)
          (setq htd-p1 (polar htd-p1 (* pi 1.5) (+ *sysdiag-gj-heigh* 100.0)))
          (setq htd-p1 (polar htd-p1 (* pi 1.5) 100.0))
        )

        (setq m (1+ m))
        (setq i 0)
      )

      (setq actual-height (- (cadr htd) (cadr htd-p1)))
      (setq htd-pt1 (polar htd (* pi 1.5) actual-height))
      (setq htd-pt2 (polar htd-pt1 0 *sysdiag-rect-width*))
      (setq htd-pt3 (polar htd-pt2 (* pi 0.5) actual-height))

      (if (> actual-height 18800.0)
        (command-s "_.pline" htd htd-pt1 htd-pt2 htd-pt3 "c")
        (progn
          (setq htd-pt1 (polar htd (* pi 1.5) 18800.0))
          (setq htd-pt2 (polar htd-pt1 0 *sysdiag-rect-width*))
          (setq htd-pt3 (polar htd-pt2 (* pi 0.5) 18800.0))
          (command-s "_.pline" htd htd-pt1 htd-pt2 htd-pt3 "c")
        )
      )

      (setq *sysdiag-gj-heigh* nil)
      (setvar "cmdecho" cm)
      (setvar "osmode" os)
      T
    )
  )
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
  (entmakex (list (cons 0 "LINE") (cons 8 layer) (cons 10 start-pt) (cons 11 end-pt)))
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

  (setq grouped (sysdiag-group-by-junction device-list))

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

  (command-s "_.rectang" (list x y 0.0) (list (+ x 500.0) (+ y 300.0) 0.0))
  (command-s "_.text" "_j" "_m" (list (+ x 250.0) (+ y 150.0) 0.0)
             *sysdiag-text-height* "0" "Junction")

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
