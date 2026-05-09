;;;===============================================================
;;;  M12 - Main Module
;;;  Main entry point and workflow coordination
;;;  Refactored to match original drawCCTV logic
;;;===============================================================
;;;  ENCODING: ANSI (ASCII only, no Chinese characters)
;;;  DEPENDENCIES: M00-M11
;;;  LOAD ORDER: M00, M01, M02, M03, M04, M05, M06, M07, M08, M09, M10, M11, M12
;;;===============================================================

;;;---------------------------------------------------------------
;;;  Global Configuration Variables
;;;---------------------------------------------------------------
(setq *main-camera-blocks* nil)
(setq *main-camera-name-layers* nil)
(setq *main-junction-blocks* nil)
(setq *main-cable-tray-layer* nil)
(setq *main-pipe-layer* nil)
(setq *main-room-points* nil)
(setq *main-equiv-points* nil)
(setq *main-cable-coefficient* 1.2)
(setq *main-junction-bias* 10000.0)
(setq *main-room-bias* 25000.0)
(setq *main-temp-layer* "TEMP_CCTV")
(setq *main-temp-layer2* "TEMP_CCTV2")
(setq *main-mline-set* nil)
(setq *main-mline-p1* nil)
(setq *main-mline-p2* nil)

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
  (setq *main-hidden-layers* nil)
  (setq layers (vla-get-layers (vla-get-activedocument (vlax-get-acad-object))))
  (vlax-for layer layers
    (if (= (vla-get-LayerOn layer) :vlax-false)
      (setq *main-hidden-layers* (cons (vla-get-name layer) *main-hidden-layers*))
    )
  )
  (setq tcmb nil)
  (vlax-for layer layers
    (setq tcmb (cons (list (vla-get-name layer) layer) tcmb))
  )
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
;;;  main-setup-temp-layer
;;;  Create a temporary layer with error handling
;;;  Returns T on success, nil on failure
;;;---------------------------------------------------------------
(defun main-setup-temp-layer (layer-name color / err-result)
  (if (null layer-name)
    (progn
      (princ "\n[main] Setup temp layer: layer name is nil.")
      nil
    )
    (progn
      (setq err-result (vl-catch-all-apply
        '(lambda ()
          (if (null (tblsearch "LAYER" layer-name))
            (command-s "_.layer" "_m" layer-name "_c" color layer-name "")
          )
          T
        )))
      (if (vl-catch-all-error-p err-result)
        (progn
          (princ (strcat "\n[main] Layer setup error for " layer-name ": "
                         (vl-catch-all-error-message err-result)))
          nil
        )
        err-result
      )
    )
  )
)

;;;---------------------------------------------------------------
;;;  main-cleanup-temp-layer
;;;  Erase all entities on a temporary layer with error handling
;;;  Returns T on success, nil on failure
;;;---------------------------------------------------------------
(defun main-cleanup-temp-layer (layer-name / clean-ss i tmp err-result)
  (if (null layer-name)
    (progn
      (princ "\n[main] Cleanup temp layer: layer name is nil.")
      nil
    )
    (progn
      (setq err-result (vl-catch-all-apply
        '(lambda ()
          (setq i 0)
          (setq clean-ss (ssget "x" (list (cons 8 layer-name))))
          (if clean-ss
            (repeat (sslength clean-ss)
              (setq tmp (ssname clean-ss i))
              (command-s "_.erase" tmp "")
              (setq i (1+ i))
            )
          )
          T
        )))
      (if (vl-catch-all-error-p err-result)
        (progn
          (princ (strcat "\n[main] Layer cleanup error for " layer-name ": "
                         (vl-catch-all-error-message err-result)))
          nil
        )
        err-result
      )
    )
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
  (main-setup-temp-layer *main-temp-layer* "3")
  (main-bltc-add *main-temp-layer*)
  (main-setup-temp-layer *main-temp-layer2* "3")
  (main-bltc-add *main-temp-layer2*)
  T
)

;;;---------------------------------------------------------------
;;;  main-cleanup
;;;  Cleanup temporary entities
;;;  Equivalent to original clean_creen
;;;---------------------------------------------------------------
(defun main-cleanup (/ )
  (main-cleanup-temp-layer *main-temp-layer*)
  (main-cleanup-temp-layer *main-temp-layer2*)
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

  (if *main-mline-set*
    (progn
      (setq lines (mline-convert-selection *main-mline-set* *main-temp-layer*))
      (if lines
        (princ (strcat "\n[main] Converted " (itoa (sslength *main-mline-set*)) " MLINEs (area)."))
        (princ "\n[main] Warning: MLINE conversion returned nil.")
      )
    )
    (progn
      (if *main-cable-tray-layer*
        (progn
          (setq lines (mline-process-all (list *main-cable-tray-layer*) *main-temp-layer* nil nil))
          (if (null lines)
            (princ "\n[main] Warning: mline-process-all returned nil.")
          )
        )
      )
    )
  )

  (dup-remove-all nil *main-temp-layer*)

  (break-lines-all (list *main-temp-layer* *main-temp-layer2*))

  (princ "\n[main] Cable trays processed.")
  T
)

;;;---------------------------------------------------------------
;;;  main-build-graph
;;;  Build graph from processed lines
;;;---------------------------------------------------------------
(defun main-build-graph (/ ss gllst build-result)
  (princ "\n[main] Building graph...")
  (setq gllst (list (cons 0 "LINE") (cons 8 (strcat *main-temp-layer* "," *main-temp-layer2*))))
  (setq ss (ssget "x" gllst))
  (if (and ss (> (sslength ss) 0))
    (progn
      (setq build-result (graph-build-from-lines ss nil))
      (if (null build-result)
        (progn
          (princ "\n[main] Error: graph-build-from-lines returned nil.")
          nil
        )
        (progn
          (graph-floyd-compute)
          (princ (strcat "\n[main] Graph: " (itoa (graph-get-node-count)) " nodes, "
                         (itoa (graph-get-edge-count)) " edges."))
          T
        )
      )
    )
    (progn
      (princ "\n[main] Error: No lines found for graph.")
      nil
    )
  )
)

;;;---------------------------------------------------------------
;;;  main-process-branch1
;;;  Branch 1: No room points, has junctions
;;;  Returns: (gjx-list gjx-name-list)
;;;---------------------------------------------------------------
(defun main-process-branch1 (junction-ss / gjx-list gjx-name-list
                                       i ent base-pt proj-info proj-pt proj-dist name
                                       temp-ss)
  (if (null junction-ss)
    (progn
      (princ "\n[main] Branch 1: junction selection set is nil.")
      (list nil nil)
    )
    (progn
      (setq temp-ss (ssget "x" (list (cons 0 "LINE") (cons 8 *main-temp-layer*))))
      (if (null temp-ss)
        (progn
          (princ "\n[main] Branch 1: No lines found (graph-build-from-lines may have failed).")
          (list nil nil)
        )
        (progn
          (princ "\n[main] Branch 1: No room, has junctions.")
          (setq gjx-list nil)
          (setq gjx-name-list nil)
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
                    (entmakex (list (cons 0 "LINE") (cons 8 *main-temp-layer*) (cons 10 base-pt) (cons 11 proj-pt)))
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
          (list gjx-list gjx-name-list)
        )
      )
    )
  )
)

;;;---------------------------------------------------------------
;;;  main-process-branch2
;;;  Branch 2: Has room points, has junctions
;;;  Returns: (gjx-list gjx-name-list)
;;;---------------------------------------------------------------
(defun main-process-branch2 (junction-ss room-pts / gjx-list gjx-name-list
                                        i ent base-pt proj-info proj-pt proj-dist name
                                        y tmp-pt temp-ss)
  (if (or (null junction-ss) (null room-pts))
    (progn
      (princ "\n[main] Branch 2: junction-ss or room-pts is nil.")
      (list nil nil)
    )
    (progn
      (setq temp-ss (ssget "x" (list (cons 0 "LINE") (cons 8 *main-temp-layer*))))
      (if (null temp-ss)
        (progn
          (princ "\n[main] Branch 2: No lines found (graph-build-from-lines may have failed).")
          (list nil nil)
        )
        (progn
          (princ "\n[main] Branch 2: Has room, has junctions.")
          (setq gjx-list nil)
          (setq gjx-name-list nil)
          (setq y 0)
          (repeat (length room-pts)
            (setq tmp-pt (nth y room-pts))
            (setq proj-info (device-project-to-graph tmp-pt nil nil))
            (if proj-info
              (progn
                (setq proj-pt (cadr proj-info))
                (entmakex (list (cons 0 "LINE") (cons 8 *main-temp-layer*) (cons 10 tmp-pt) (cons 11 proj-pt)))
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
          (setq y 0)
          (repeat (1- (length room-pts))
            (entmakex (list (cons 0 "LINE") (cons 8 *main-temp-layer*) (cons 10 (nth y room-pts)) (cons 11 (nth (1+ y) room-pts))))
            (setq y (1+ y))
          )
          (if (> (length room-pts) 1)
            (entmakex (list (cons 0 "LINE") (cons 8 *main-temp-layer*) (cons 10 (nth 0 room-pts)) (cons 11 (nth (1- (length room-pts)) room-pts))))
          )
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
                    (entmakex (list (cons 0 "LINE") (cons 8 *main-temp-layer*) (cons 10 base-pt) (cons 11 proj-pt)))
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
          (list gjx-list gjx-name-list)
        )
      )
    )
  )
)

;;;---------------------------------------------------------------
;;;  main-process-branch3
;;;  Branch 3: Has room points, no junctions
;;;  Returns: (gjx-list gjx-name-list)
;;;---------------------------------------------------------------
(defun main-process-branch3 (room-pts / gjx-list gjx-name-list
                                       y tmp-pt proj-info proj-pt
                                       temp-ss)
  (if (null room-pts)
    (progn
      (princ "\n[main] Branch 3: room-pts is nil.")
      (list nil nil)
    )
    (progn
      (setq temp-ss (ssget "x" (list (cons 0 "LINE") (cons 8 *main-temp-layer*))))
      (if (null temp-ss)
        (progn
          (princ "\n[main] Branch 3: No lines found (graph-build-from-lines may have failed).")
          (list nil nil)
        )
        (progn
          (princ "\n[main] Branch 3: Has room, no junctions.")
          (setq gjx-list nil)
          (setq gjx-name-list nil)
          (setq y 0)
          (repeat (length room-pts)
            (setq tmp-pt (nth y room-pts))
            (setq proj-info (device-project-to-graph tmp-pt nil nil))
            (if proj-info
              (progn
                (setq proj-pt (cadr proj-info))
                (entmakex (list (cons 0 "LINE") (cons 8 *main-temp-layer*) (cons 10 tmp-pt) (cons 11 proj-pt)))
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
          (setq y 0)
          (repeat (1- (length room-pts))
            (entmakex (list (cons 0 "LINE") (cons 8 *main-temp-layer*) (cons 10 (nth y room-pts)) (cons 11 (nth (1+ y) room-pts))))
            (setq y (1+ y))
          )
          (if (> (length room-pts) 1)
            (entmakex (list (cons 0 "LINE") (cons 8 *main-temp-layer*) (cons 10 (nth 0 room-pts)) (cons 11 (nth (1- (length room-pts)) room-pts))))
          )
          (list gjx-list gjx-name-list)
        )
      )
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
                                       i ent base-pt proj-pt proj-dist proj-info name
                                       y tmp-pt tmp-proj j)
  (cond
    ((and (null room-pts) junction-ss)
     (main-process-branch1 junction-ss))

    ((and room-pts junction-ss)
     (main-process-branch2 junction-ss room-pts))

    ((and room-pts (null junction-ss))
     (main-process-branch3 room-pts))

    (T
     (princ "\n[main] No room points and no junctions.")
     (list nil nil)
    )
  )
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
      (dup-remove-all nil *main-temp-layer*)
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
(defun main-get-camera-blocks (/ ss result i)
  (if (null *main-camera-blocks*)
    (progn
      (princ "\n[main] Warning: *main-camera-blocks* is nil.")
      nil
    )
    (progn
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
  )
)

;;;---------------------------------------------------------------
;;;  main-get-junction-blocks
;;;  Get junction block selection set
;;;---------------------------------------------------------------
(defun main-get-junction-blocks (/ ss result i)
  (if (null *main-junction-blocks*)
    (progn
      (princ "\n[main] Warning: *main-junction-blocks* is nil.")
      nil
    )
    (progn
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
  )
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
                              end-drawlist m_n main-workflow-error)
  (princ "\n\n=== CCTV System Workflow ===")

  (defun main-workflow-error (msg)
    (princ (strcat "\n[main] Error: " msg))
    (vl-catch-all-apply 'main-cleanup nil)
    (vl-catch-all-apply 'main-dktc nil)
    (vl-catch-all-apply 'main-env-end nil)
  )

  (main-env-start)
  (setq *error* main-workflow-error)
  (main-init)
  (setq workflow-ok T)

  (setq draw-pts (getpoint "\nSystem diagram insertion point: "))
  (if (null draw-pts) (setq draw-pts (list 0.0 0.0 0.0)))

  (main-gbtc)

  (if (null (main-process-cable-trays))
    (princ "\n[main] Warning: Cable tray processing returned nil.")
  )

  (princ "\n[main] Removing duplicates...")
  (dup-remove-all nil *main-temp-layer*)

  (setq junction-ss (main-get-junction-blocks))
  (setq room-result (main-process-room-points junction-ss *main-room-points*))
  (setq gjx-list (car room-result))
  (setq gjx-name-list (cadr room-result))

  (main-process-equivalent-points)

  (princ "\n[main] Breaking intersections...")
  (break-lines-all (list *main-temp-layer* *main-temp-layer2*))

  (if (null (main-build-graph))
    (progn
      (princ "\n[main] Workflow aborted: graph build failed.")
      (setq workflow-ok nil)
    )
  )

  (if workflow-ok
    (progn
      (if *main-cable-tray-layer*
        (command-s "_.layer" "_off" *main-cable-tray-layer* "")
      )

      (if (null *main-camera-blocks*)
        (progn
          (princ "\n[main] Error: *main-camera-blocks* is nil, no cameras configured.")
          (setq workflow-ok nil)
        )
      )

      (if workflow-ok
        (progn
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
                    (setq blk-name (cdr (assoc 2 (entget ent))))
                    (setq dev-name (block-get-name-from-text ent nil))
                    (if (null dev-name) (setq dev-name "CAM"))

                    (setq proj-info (device-project-to-graph base-pt nil nil))
                    (if proj-info
                      (progn
                        (setq proj-pt (cadr proj-info))
                        (setq proj-dist (caddr proj-info))

                        (setq end-dis 1000000.0)
                        (setq best-jnx nil)
                        (setq best-name nil)

                        (setq m_n 0)
                        (repeat (length gjx-list)
                          (setq tmp-jnx (nth m_n gjx-list))
                          (setq tmp-jnx-pt (car tmp-jnx))
                          (setq tmp-jnx-dist (cdr tmp-jnx))
                          (setq tmp-jnx-name (nth m_n gjx-name-list))

                          (setq dev-node (car proj-info))
                          (setq jnx-node (graph-get-node-index tmp-jnx-pt))
                          (if jnx-node
                            (progn
                              (setq graph-dist (graph-get-distance dev-node jnx-node))
                              (if (and graph-dist (< graph-dist 1e29))
                                (progn
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

              (if *main-cable-tray-layer*
                (command-s "_.layer" "_on" *main-cable-tray-layer* "")
              )

              (main-dktc)

              (setq end-drawlist (sysdiag-classify-by-junction end-drawlist))

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
    )
  )

  (main-cleanup)

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
