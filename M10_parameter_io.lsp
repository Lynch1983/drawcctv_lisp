;;;===============================================================
;;;  M10 - Parameter I/O Module
;;;  Read and write configuration files
;;;===============================================================
;;;  ENCODING: ANSI (ASCII only, no Chinese characters)
;;;  DEPENDENCIES: None
;;;===============================================================

;;;---------------------------------------------------------------
;;;  Global Parameters
;;;---------------------------------------------------------------
;;;  *param-default-file* - Default parameter file path
(setq *param-default-file* "CCTV_params.txt")

;;;---------------------------------------------------------------
;;;  param-string-split
;;;  Split a string by delimiter
;;;  Args: str       - string to split
;;;         delimiter - delimiter character (default ",")
;;;  Returns: list of substrings
;;;---------------------------------------------------------------
(defun param-string-split (str delimiter / result pos)
  (if (null delimiter) (setq delimiter ","))
  (setq result nil)
  (while (setq pos (vl-string-search delimiter str))
    (setq result (cons (substr str 1 pos) result))
    (setq str (substr str (+ pos 2)))
  )
  (setq result (cons str result))
  (reverse result)
)

;;;---------------------------------------------------------------
;;;  param-string-join
;;;  Join a list of strings with delimiter
;;;  Args: lst       - list of strings
;;;         delimiter - delimiter string (default ",")
;;;  Returns: joined string
;;;---------------------------------------------------------------
(defun param-string-join (lst delimiter / result)
  (if (null delimiter) (setq delimiter ","))
  (setq result "")
  (foreach item lst
    (if (= result "")
      (setq result item)
      (setq result (strcat result delimiter item))
    )
  )
  result
)

;;;---------------------------------------------------------------
;;;  param-string-trim
;;;  Trim whitespace from both ends of a string
;;;  Args: str - string to trim
;;;  Returns: trimmed string
;;;---------------------------------------------------------------
(defun param-string-trim (str / start end)
  (setq start 1)
  (setq end (strlen str))
  ;; Trim from start
  (while (and (<= start end)
              (wcmatch (substr str start 1) "[ ]"))
    (setq start (1+ start))
  )
  ;; Trim from end
  (while (and (>= end start)
              (wcmatch (substr str end 1) "[ ]"))
    (setq end (1- end))
  )
  (if (<= start end)
    (substr str start (- end start -1))
    ""
  )
)

;;;---------------------------------------------------------------
;;;  param-parse-line
;;;  Parse a parameter line "name=value1,value2,..."
;;;  Args: line - input line
;;;  Returns: (name (value1 value2 ...)) or nil
;;;---------------------------------------------------------------
(defun param-parse-line (line / pos name values)
  (setq line (param-string-trim line))
  ;; Skip empty lines and comments
  (if (or (= line "") (= (substr line 1 1) ";"))
    nil
    (progn
      (setq pos (vl-string-search "=" line))
      (if pos
        (progn
          (setq name (param-string-trim (substr line 1 pos)))
          (setq values (param-string-split (substr line (+ pos 2)) ","))
          (list name values)
        )
        nil
      )
    )
  )
)

;;;---------------------------------------------------------------
;;;  param-format-line
;;;  Format a parameter as "name=value1,value2,..."
;;;  Args: name   - parameter name
;;;         values - list of values
;;;  Returns: formatted string
;;;---------------------------------------------------------------
(defun param-format-line (name values)
  (strcat name "=" (param-string-join values ","))
)

;;;---------------------------------------------------------------
;;;  param-load
;;;  Load parameters from file
;;;  Args: filename - file path (nil = use default)
;;;  Returns: association list of parameters or nil
;;;---------------------------------------------------------------
(defun param-load (filename / fp line params parsed)
  (if (null filename)
    (setq filename *param-default-file*)
  )
  (setq params nil)
  (setq fp (open filename "r"))
  (if fp
    (progn
      (princ (strcat "\n[param] Loading from " filename "..."))
      (while (setq line (read-line fp))
        (setq parsed (param-parse-line line))
        (if parsed
          (setq params (cons parsed params))
        )
      )
      (close fp)
      (princ (strcat "\n[param] Loaded " (itoa (length params)) " parameters."))
      (reverse params)
    )
    (progn
      (princ (strcat "\n[param] File not found: " filename))
      nil
    )
  )
)

;;;---------------------------------------------------------------
;;;  param-save
;;;  Save parameters to file
;;;  Args: params   - association list of parameters
;;;         filename - file path (nil = use default)
;;;  Returns: T on success, nil on failure
;;;---------------------------------------------------------------
(defun param-save (params filename / fp)
  (if (null filename)
    (setq filename *param-default-file*)
  )
  (setq fp (open filename "w"))
  (if fp
    (progn
      (princ (strcat "\n[param] Saving to " filename "..."))
      ;; Write header
      (princ "; CCTV System Parameters\n" fp)
      (princ "; Auto-generated file\n" fp)
      (princ "\n" fp)
      ;; Write parameters
      (foreach param params
        (princ (param-format-line (car param) (cadr param)) fp)
        (princ "\n" fp)
      )
      (close fp)
      (princ (strcat "\n[param] Saved " (itoa (length params)) " parameters."))
      T
    )
    (progn
      (princ (strcat "\n[param] Cannot write to: " filename))
      nil
    )
  )
)

;;;---------------------------------------------------------------
;;;  param-get
;;;  Get a parameter value by name
;;;  Args: params - parameter list
;;;         name   - parameter name
;;;  Returns: list of values or nil
;;;---------------------------------------------------------------
(defun param-get (params name / entry)
  (setq entry (assoc name params))
  (if entry (cadr entry) nil)
)

;;;---------------------------------------------------------------
;;;  param-set
;;;  Set a parameter value
;;;  Args: params - parameter list
;;;         name   - parameter name
;;;         values - list of values
;;;  Returns: updated parameter list
;;;---------------------------------------------------------------
(defun param-set (params name values / entry)
  (setq entry (assoc name params))
  (if entry
    (subst (list name values) entry params)
    (cons (list name values) params)
  )
)

;;;---------------------------------------------------------------
;;;  param-remove
;;;  Remove a parameter
;;;  Args: params - parameter list
;;;         name   - parameter name
;;;  Returns: updated parameter list
;;;---------------------------------------------------------------
(defun param-remove (params name / entry)
  (setq entry (assoc name params))
  (if entry
    (vl-remove entry params)
    params
  )
)

;;;---------------------------------------------------------------
;;;  param-value-to-string
;;;  Convert a value to string
;;;  Args: val - value (number, string, point, etc.)
;;;  Returns: string representation
;;;---------------------------------------------------------------
(defun param-value-to-string (val)
  (cond
    ((null val) "")
    ((numberp val) (rtos val 2 6))
    ((listp val)
     (param-string-join (mapcar 'param-value-to-string val) ";")
    )
    (T (strcat "" val))
  )
)

;;;---------------------------------------------------------------
;;;  param-string-to-value
;;;  Convert a string to appropriate type
;;;  Args: str - input string
;;;  Returns: number, string, or list
;;;---------------------------------------------------------------
(defun param-string-to-value (str / num is-num)
  (setq str (param-string-trim str))
  (if (= str "")
    ""
    (progn
      ;; Check if string represents a number
      (setq is-num T)
      (foreach ch (vl-string->list str)
        (if (and (/= ch 45) (/= ch 46) (< ch 48) (> ch 57))
          (setq is-num nil)
        )
      )
      (if is-num
        (atof str)
        str
      )
    )
  )
)

;;;---------------------------------------------------------------
;;;  param-list-to-string
;;;  Convert a list to comma-separated string
;;;  Args: lst - list of values
;;;  Returns: string
;;;---------------------------------------------------------------
(defun param-list-to-string (lst)
  (param-string-join (mapcar 'param-value-to-string lst) ",")
)

;;;---------------------------------------------------------------
;;;  param-string-to-list
;;;  Convert comma-separated string to list
;;;  Args: str - input string
;;;  Returns: list of values
;;;---------------------------------------------------------------
(defun param-string-to-list (str)
  (mapcar 'param-string-to-value (param-string-split str ","))
)

;;;---------------------------------------------------------------
;;;  param-set-default-file
;;;  Set the default parameter file path
;;;---------------------------------------------------------------
(defun param-set-default-file (path)
  (setq *param-default-file* path)
)

;;;---------------------------------------------------------------
;;;  param-get-default-file
;;;  Get the default parameter file path
;;;---------------------------------------------------------------
(defun param-get-default-file ()
  *param-default-file*
)

;;;===============================================================
;;;  TEST FUNCTIONS
;;;===============================================================

(defun test-M10-parameter-io (/ passed failed params loaded)
  (setq passed 0 failed 0)

  (princ "\n\n=== M10 Parameter I/O Tests ===")

  ;; Test 1: param-string-split
  (princ "\n[Test 1] param-string-split...")
  (setq result (param-string-split "a,b,c,d" ","))
  (if (and (= (length result) 4)
           (= (car result) "a")
           (= (cadddr result) "d"))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 2: param-string-join
  (princ "\n[Test 2] param-string-join...")
  (setq result (param-string-join '("x" "y" "z") ";"))
  (if (= result "x;y;z")
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 3: param-string-trim
  (princ "\n[Test 3] param-string-trim...")
  (setq result (param-string-trim "  hello world  "))
  (if (= result "hello world")
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 4: param-parse-line
  (princ "\n[Test 4] param-parse-line...")
  (setq result (param-parse-line "camera_blocks=CAM1,CAM2,CAM3"))
  (if (and result
           (= (car result) "camera_blocks")
           (= (length (cadr result)) 3)
           (= (car (cadr result)) "CAM1"))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 5: param-format-line
  (princ "\n[Test 5] param-format-line...")
  (setq result (param-format-line "junction_blocks" '("GJX1" "GJX2")))
  (if (= result "junction_blocks=GJX1,GJX2")
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 6: param-set and param-get
  (princ "\n[Test 6] param-set and param-get...")
  (setq params nil)
  (setq params (param-set params "layer1" '("value1" "value2")))
  (setq params (param-set params "layer2" '("value3")))
  (setq val (param-get params "layer1"))
  (if (and (= (length val) 2)
           (= (car val) "value1"))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 7: param-remove
  (princ "\n[Test 7] param-remove...")
  (setq params (param-remove params "layer1"))
  (setq val (param-get params "layer1"))
  (if (null val)
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 8: param-value-to-string
  (princ "\n[Test 8] param-value-to-string...")
  (setq result (param-value-to-string 3.14159))
  (if (and result (wcmatch result "3.14159*"))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 9: param-string-to-value
  (princ "\n[Test 9] param-string-to-value...")
  (setq result (param-string-to-value "123.456"))
  (if (and (numberp result) (< (abs (- result 123.456)) 0.001))
    (progn (princ " PASS") (setq passed (1+ passed)))
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 10: param-save and param-load (file I/O)
  (princ "\n[Test 10] param-save and param-load...")
  (setq test-file "test_params.txt")
  (setq test-params
    (list
      (list "camera_blocks" '("CAM-A" "CAM-B" "CAM-C"))
      (list "junction_blocks" '("GJX-01" "GJX-02"))
      (list "cable_tray_layer" '("CABLE_TRAY"))
      (list "coefficient" '("1.2"))
    ))
  (param-save test-params test-file)
  (setq loaded (param-load test-file))
  (if (and loaded
           (= (length loaded) 4)
           (param-get loaded "camera_blocks"))
    (progn 
      (princ " PASS") 
      (setq passed (1+ passed))
      ;; Cleanup
      (vl-file-delete test-file)
    )
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Test 11: default file get/set
  (princ "\n[Test 11] default file get/set...")
  (setq old-file (param-get-default-file))
  (param-set-default-file "new_params.txt")
  (if (= (param-get-default-file) "new_params.txt")
    (progn 
      (princ " PASS") 
      (setq passed (1+ passed))
      (param-set-default-file old-file)
    )
    (progn (princ " FAIL") (setq failed (1+ failed)))
  )

  ;; Summary
  (princ (strcat "\n\n=== M10 Test Summary: "
                 (itoa passed) " passed, "
                 (itoa failed) " failed ===\n"))

  (list passed failed)
)

;;;---------------------------------------------------------------
;;;  Provide module info
;;;---------------------------------------------------------------
(princ "\n[M10] parameter_io.lsp loaded.")
(princ (strcat "  Functions: param-load, param-save, param-get, param-set, "
               "param-string-split, param-string-join"))
(princ (strcat "\n  Default file: " *param-default-file*))
(princ "\n  Test: (test-M10-parameter-io)")
(princ)
