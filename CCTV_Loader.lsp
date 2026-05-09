;;;===============================================================
;;;  CCTV_Loader.lsp - Master Loader
;;;  Load all modules in correct dependency order
;;;===============================================================
;;;  ENCODING: ANSI (ASCII only, no Chinese characters)
;;;  Usage: Put this file and all M01-M13 files in the same folder,
;;;         then in AutoCAD type: (load "CCTV_Loader.lsp")
;;;
;;;  Alternatively, use the merged file:
;;;         (load "CCTV_AllInOne.lsp")
;;;===============================================================
;;;
;;;  Module List (13 modules, dependency order):
;;;    1. M01_graph_algorithm.lsp   - Core graph data structure & Floyd-Warshall
;;;    2. M02_line_utils.lsp         - LINE entity utility functions
;;;    3. M03_mline_converter.lsp    - MLINE to LINE conversion & endpoint connection
;;;    4. M04_duplicate_remover.lsp  - Duplicate & overlapping line removal
;;;    5. M05_break_lines.lsp        - Break lines at intersection points
;;;    6. M06_block_utils.lsp        - Block reference utility functions
;;;    7. M07_device_projection.lsp  - Device point projection to graph
;;;    8. M08_equivalent_points.lsp  - Equivalent connectivity points
;;;    9. M09_system_diagram.lsp     - CCTV system diagram generation
;;;   10. M10_parameter_io.lsp       - Configuration file read/write
;;;   11. M11_gui_handlers.lsp       - Command-line GUI & user interaction
;;;   12. M12_main.lsp               - Main workflow coordination
;;;   13. M13_test_suite.lsp         - Comprehensive test suite
;;;
;;;  Merged file: CCTV_AllInOne.lsp (all 13 modules in one file)
;;;===============================================================

;;;---------------------------------------------------------------
;;;  Configuration: Set module file path here
;;;  If files are in a different folder, change this variable
;;;---------------------------------------------------------------
(setq *cctv-module-path* "")

;;;---------------------------------------------------------------
;;;  cctv-load-module
;;;  Load a single module file
;;;  Args: filename - module filename
;;;  Returns: T if loaded, nil if failed
;;;---------------------------------------------------------------
(defun cctv-load-module (filename / fullpath result)
  (setq fullpath (strcat *cctv-module-path* filename))
  (if (findfile fullpath)
    (progn
      (setq result (load fullpath))
      (if result
        (princ (strcat "\n  [OK] " filename))
        (princ (strcat "\n  [FAIL] " filename " - load error"))
      )
      result
    )
    (progn
      (princ (strcat "\n  [SKIP] " filename " - file not found"))
      nil
    )
  )
)

;;;---------------------------------------------------------------
;;;  cctv-load-all
;;;  Load all modules in dependency order
;;;  Returns: T if all loaded, nil if any failed
;;;---------------------------------------------------------------
(defun cctv-load-all (/ all-ok)
  (princ "\n========================================")
  (princ "\n  CCTV System - Loading Modules")
  (princ "\n========================================")

  (setq all-ok T)

  ;; Phase 1: Core (no dependencies)
  (princ "\n\n[Phase 1] Core modules:")
  (if (null (cctv-load-module "M01_graph_algorithm.lsp")) (setq all-ok nil))
  (if (null (cctv-load-module "M02_line_utils.lsp")) (setq all-ok nil))

  ;; Phase 2: Geometry processing (depends on M02)
  (princ "\n\n[Phase 2] Geometry processing:")
  (if (null (cctv-load-module "M03_mline_converter.lsp")) (setq all-ok nil))
  (if (null (cctv-load-module "M04_duplicate_remover.lsp")) (setq all-ok nil))
  (if (null (cctv-load-module "M05_break_lines.lsp")) (setq all-ok nil))

  ;; Phase 3: Utilities (no dependencies)
  (princ "\n\n[Phase 3] Utilities:")
  (if (null (cctv-load-module "M06_block_utils.lsp")) (setq all-ok nil))
  (if (null (cctv-load-module "M10_parameter_io.lsp")) (setq all-ok nil))

  ;; Phase 4: Application (depends on M01, M02, M06)
  (princ "\n\n[Phase 4] Application:")
  (if (null (cctv-load-module "M07_device_projection.lsp")) (setq all-ok nil))
  (if (null (cctv-load-module "M08_equivalent_points.lsp")) (setq all-ok nil))
  (if (null (cctv-load-module "M09_system_diagram.lsp")) (setq all-ok nil))

  ;; Phase 5: Integration (depends on all above)
  (princ "\n\n[Phase 5] Integration:")
  (if (null (cctv-load-module "M12_main.lsp")) (setq all-ok nil))
  (if (null (cctv-load-module "M11_gui_handlers.lsp")) (setq all-ok nil))

  ;; Phase 6: Test suite (optional)
  (princ "\n\n[Phase 6] Test suite:")
  (cctv-load-module "M13_test_suite.lsp")

  ;; Summary
  (princ "\n\n========================================")
  (if all-ok
    (progn
      (princ "\n  All modules loaded successfully!")
      (princ "\n  Type CCTV-Help for available commands")
    )
    (progn
      (princ "\n  WARNING: Some modules failed to load.")
      (princ "\n  Check file paths and try again.")
    )
  )
  (princ "\n========================================\n")

  all-ok
)

;;;---------------------------------------------------------------
;;;  Auto-load on file load
;;;---------------------------------------------------------------
(cctv-load-all)

(princ)
