;;; Code:

(defface midje-work-todo-face
  '((((class color) (background light))
     :background "yellow1")
    (((class color) (background dark))
     :background "yellow4"))
  "Face for future facts."
  :group 'midje
  :package-version '(midje . "0.1.0"))

(defun midje-render-test-summary (summary)
  (cider-insert "Test Summary" 'bold t)
  (nrepl-dbind-response summary (error fail ns pass test skip)
    (insert (format "Tested %d namespaces\n" ns))
    (insert (format "Ran %d assertions.\n" test))
    (unless (zerop fail)
      (cider-insert (format "%d failures" fail) 'cider-test-failure-face t))
    (unless (zerop error)
      (cider-insert (format "%d errors" error) 'cider-test-error-face t))
    (unless (zerop skip)
      (cider-insert (format "%d to do" skip) 'midje-work-todo-face t))
    (when (zerop (+ fail error))
      (cider-insert (format "%d passed" pass) 'cider-test-success-face t))))

(defun midje-render-test-results (results)
  )

(defun midje-get-test-report-buffer ()
  (or (get-buffer cider-test-report-buffer)
      (cider-popup-buffer cider-test-report-buffer
                          cider-auto-select-test-report-buffer)))

(defun midje-render-test-report (results summary)
  (with-current-buffer (midje-get-test-report-buffer)
    (cider-test-report-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (midje-render-test-summary summary))))

(provide 'midje-test-report)
