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
