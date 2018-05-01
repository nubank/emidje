;;; emidje.el --- Test result viewer for Midje (https://github.com/marick/Midje) -*- lexical-binding: t -*-

;;; Code:

(require 'cider)

(defface emidje-work-todo-face
  '((((class color) (background light))
     :background "yellow1")
    (((class color) (background dark))
     :background "yellow4"))
  "Face for future facts."
  :group 'midje
  :package-version '(emidje . "0.1.0"))

(defconst midje-nrepl-version "0.1.0-SNAPSHOT")

(defun emidje-inject-jack-in-dependencies ()
  (add-to-list 'cider-jack-in-lein-plugins `("midje-nrepl" ,midje-nrepl-version) t))

;;;###autoload
(eval-after-load 'cider
  '(emidje-inject-jack-in-dependencies))

(defun emidje-render-one-test-result (result)
  (nrepl-dbind-response result (context expected actual error message type)
    (cl-flet ((insert-label (s)
                            (cider-insert (format "%8s: " s) 'font-lock-comment-face))
              (insert-align-label (s)
                                  (insert (format "%12s" s)))
              (insert-rect (s)
                           (insert-rectangle (thread-first s
                                               cider-font-lock-as-clojure
                                               (split-string "\n")))
                           (beginning-of-line)))
      (cider-propertize-region (cider-intern-keys (cdr result))
        (let ((beg (point))
              (type-face (cider-test-type-simple-face type))
              (bg `(:background ,cider-test-items-background-color)))
          (cider-insert (capitalize type) type-face nil " in ")
          (dolist (text context)
            (cider-insert text 'font-lock-doc-face t))
          (when expected
            (insert-label "expected")
            (insert-rect expected)
            (insert "\n"))
          (when actual
            (insert-label "actual")
            (insert-rect actual))
          (unless (seq-empty-p message)
            (dolist (text message)
              (cider-insert text 'font-lock-doc-string-face t)))
          (when error
            (insert-label "error")
            (insert-text-button error
                                'follow-link t
                                'action '(lambda (_button) (cider-test-stacktrace))
                                'help-echo "View causes and stacktrace"))
          (overlay-put (make-overlay beg (point)) 'font-lock-face bg))
        (insert "\n\n")))))

(defun emidje-count-non-passing-tests (results)
  (seq-count (lambda (result)
               (let* ((type (nrepl-dict-get result "type")))
                 (and (not (equal type "error"))
                      (not (equal type "fail"))))) results))

(defun emidje-get-displayable-results (results)
  (seq-filter (lambda (result)
                (not (equal (nrepl-dict-get result "type") "pass")))
              results))

(defun emidje-render-test-results (results-dict)
  (cider-insert "Results" 'bold t "\n")
  (nrepl-dict-map (lambda (ns results)
                    (let* ((displayable-results (emidje-get-displayable-results results))
                           (problems (emidje-count-non-passing-tests displayable-results)))
                      (when (> problems 0)
                        (insert (format "%s\n%d non-passing tests:\n\n"
                                        (cider-propertize ns 'ns) problems)))
                      (dolist (result displayable-results)
                        (emidje-render-one-test-result result)))
                    ) results-dict))

(defun emidje-render-list-of-namespaces (results)
  (dolist (namespace (nrepl-dict-keys results))
    (insert (cider-propertize namespace 'ns) "\n")
    (insert "\n")))

(defun emidje-render-test-summary (summary)
  (nrepl-dbind-response summary (error fact fail ns pass test skip)
    (insert (format "Tested %d namespaces\n" ns))
    (insert (format "Ran %d assertions from %d facts\n" test fact))
    (unless (zerop fail)
      (cider-insert (format "%d failures" fail) 'cider-test-failure-face t))
    (unless (zerop error)
      (cider-insert (format "%d errors" error) 'cider-test-error-face t))
    (unless (zerop skip)
      (cider-insert (format "%d to do" skip) 'midje-work-todo-face t))
    (when (zerop (+ fail error))
      (cider-insert (format "%d passed" pass) 'cider-test-success-face t))
    (insert "\n")))

(defun emidje-get-test-report-buffer ()
  (or (get-buffer cider-test-report-buffer)
      (cider-popup-buffer cider-test-report-buffer
                          cider-auto-select-test-report-buffer)))

(defun emidje-render-test-report (results summary)
  (with-current-buffer (emidje-get-test-report-buffer)
    (cider-test-report-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (cider-insert "Test Summary" 'bold t "\n")
      (emidje-render-list-of-namespaces results)
      (emidje-render-test-summary summary)
      (emidje-render-test-results results)
      (goto-char (point-min)))))

(defvar emidje-supported-operations
  '((:ns . "midje-test-ns")
    (:test-at-point . "midje-test")
    (:retest . "midje-retest")))

(defun emidje-send-test-request (operation-type &rest params)
  (let* ((op (cdr (assq operation-type emidje-supported-operations)))
         (message (apply 'list (append `("op" ,op) params))))
    (cider-nrepl-send-request message
                              (lambda (response)
                                (nrepl-dbind-response response (results summary)
                                  (when results
                                    (emidje-render-test-report results summary)))))))

(defun emidje-run-ns-tests ()
  (interactive)
  (if-let* ((namespace (cider-current-ns t)))
      (emidje-send-test-request :ns "ns" namespace)
    (message "No namespace to be tested in the current context")))

(defun emidje-re-run-failed-tests ()
  (interactive)
  (emidje-send-test-request :retest))

(provide 'emidje)
