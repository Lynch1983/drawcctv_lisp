;;;===============================================================
;;;  M13 - Test Suite Module
;;;  Comprehensive test suite for all modules
;;;===============================================================
;;;  ENCODING: ANSI (ASCII only, no Chinese characters)
;;;  DEPENDENCIES: M01-M12
;;;===============================================================

;;;---------------------------------------------------------------
;;;  Test Results Storage
;;;---------------------------------------------------------------
(setq *test-results* nil)
(setq *test-total-passed* 0)
(setq *test-total-failed* 0)

;;;---------------------------------------------------------------
;;;  test-log
;;;  Log a test result
;;;---------------------------------------------------------------
(defun test-log (module passed failed)
  (setq *test-results* (cons (list module passed failed) *test-results*))
  (setq *test-total-passed* (+ *test-total-passed* passed))
  (setq *test-total-failed* (+ *test-total-failed* failed))
)

;;;---------------------------------------------------------------
;;;  test-assert-equal
;;;  Assert two values are equal
;;;---------------------------------------------------------------
(defun test-assert-equal (expected actual msg / result)
  (setq result (< (abs (- expected actual)) 0.0001))
  (if result
    (princ (strcat "\n  [PASS] " msg))
    (princ (strcat "\n  [FAIL] " msg " - Expected: " (rtos expected 2 4) 
                   " Got: " (rtos actual 2 4)))
  )
  result
)

;;;---------------------------------------------------------------
;;;  test-assert-true
;;;  Assert value is true
;;;---------------------------------------------------------------
(defun test-assert-true (value msg)
  (if value
    (progn (princ (strcat "\n  [PASS] " msg)) T)
    (progn (princ (strcat "\n  [FAIL] " msg)) nil)
  )
)

;;;---------------------------------------------------------------
;;;  test-assert-not-nil
;;;  Assert value is not nil
;;;---------------------------------------------------------------
(defun test-assert-not-nil (value msg)
  (if value
    (progn (princ (strcat "\n  [PASS] " msg)) T)
    (progn (princ (strcat "\n  [FAIL] " msg " - Value is nil")) nil)
  )
)

;;;---------------------------------------------------------------
;;;  test-module
;;;  Run tests for a specific module
;;;---------------------------------------------------------------
(defun test-module (module-name / result)
  (princ (strcat "\n\n=== Testing " module-name " ==="))
  (setq result
    (cond
      ((= module-name "M01") (test-M01-graph-algorithm))
      ((= module-name "M02") (test-M02-line-utils))
      ((= module-name "M03") (test-M03-mline-converter))
      ((= module-name "M04") (test-M04-duplicate-remover))
      ((= module-name "M05") (test-M05-break-lines))
      ((= module-name "M06") (test-M06-block-utils))
      ((= module-name "M07") (test-M07-device-projection))
      ((= module-name "M08") (test-M08-equivalent-points))
      ((= module-name "M09") (test-M09-system-diagram))
      ((= module-name "M10") (test-M10-parameter-io))
      (T (princ "\nUnknown module") nil)
    ))
  (if result
    (test-log module-name (car result) (cadr result))
  )
  result
)

;;;---------------------------------------------------------------
;;;  test-all
;;;  Run all module tests
;;;---------------------------------------------------------------
(defun test-all (/ old-cmdecho)
  (setq old-cmdecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  
  (setq *test-results* nil)
  (setq *test-total-passed* 0)
  (setq *test-total-failed* 0)
  
  (princ "\n\n========================================")
  (princ "\n    CCTV System Test Suite")
  (princ "\n========================================")
  
  ;; Run all module tests
  (test-module "M01")
  (test-module "M02")
  (test-module "M03")
  (test-module "M04")
  (test-module "M05")
  (test-module "M06")
  (test-module "M07")
  (test-module "M08")
  (test-module "M09")
  (test-module "M10")
  
  ;; Summary
  (test-report)
  
  (setvar "cmdecho" old-cmdecho)
  (list *test-total-passed* *test-total-failed*)
)

;;;---------------------------------------------------------------
;;;  test-report
;;;  Generate test report
;;;---------------------------------------------------------------
(defun test-report ()
  (princ "\n\n========================================")
  (princ "\n    Test Summary")
  (princ "\n========================================")
  
  (foreach result *test-results*
    (princ (strcat "\n" (car result) ": "
                   (itoa (cadr result)) " passed, "
                   (itoa (caddr result)) " failed"))
  )
  
  (princ "\n----------------------------------------")
  (princ (strcat "\nTOTAL: " (itoa *test-total-passed*) " passed, "
                 (itoa *test-total-failed*) " failed"))
  (princ "\n========================================\n")
)

;;;---------------------------------------------------------------
;;;  c:CCTV-Test
;;;  Run test suite command
;;;---------------------------------------------------------------
(defun c:CCTV-Test ()
  (test-all)
  (princ)
)

;;;===============================================================
;;;  Provide module info
;;;===============================================================
(princ "\n[M13] test_suite.lsp loaded.")
(princ "\n  Commands: CCTV-Test")
(princ "\n  Functions: test-all, test-module, test-report")
(princ "\n  Usage: (test-all) or command: CCTV-Test")
(princ)
