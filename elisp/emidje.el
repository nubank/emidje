;;; emidje.el --- Test result viewer for Midje (https://github.com/marick/Midje) -*- lexical-binding: t -*-

;;; Code:

(require 'cider)
(require 'midje-test-report)

(defface emidje-work-todo-face
  '((((class color) (background light))
     :background "yellow1")
    (((class color) (background dark))
     :background "yellow4"))
  "Face for future facts."
  :group 'midje
  :package-version '(emidje . "0.1.0"))

(defconst emidje-nrepl-version "0.1.0-SNAPSHOT")

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
      (cider-insert (format "%d passed" pass) 'cider-test-success-face t))))

(defun emidje-render-list-of-namespaces (results)
  (dolist (namespace (nrepl-dict-keys results))
    (insert (cider-propertize namespace 'ns) "\n")))

(defun emidje-get-test-report-buffer ()
  (or (get-buffer cider-test-report-buffer)
      (cider-popup-buffer cider-test-report-buffer
                          cider-auto-select-test-report-buffer)))

(defun emidje-render-test-report (results summary)
  (with-current-buffer (emidje-get-test-report-buffer)
    (cider-test-report-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (cider-insert "Test Summary" 'bold t)
      (emidje-render-list-of-namespaces results)
      (emidje-render-test-summary summary))))

(defun emidje-inject-jack-in-dependencies ()
  (add-to-list 'cider-jack-in-lein-plugins `("midje-nrepl" ,midje-nrepl-version) t))

;;;###autoload
(eval-after-load 'cider
  '(emidje-inject-jack-in-dependencies))

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

(provide 'midje)
