;;; emidje.el --- Test runner and report viewer for Midje -*- lexical-binding: t -*-

;; Author: Alan Ghelardi <alan.ghelardi@nubank.com.br>
;; Maintainer: Alan Ghelardi <alan.ghelardi@nubank.com.br>
;; Version: 0.1.0-SNAPSHOT
;; Package-Requires: ((cider "0.17.0"))
;; Homepage: https://github.com/alan-ghelardi/emidje
;; Keywords: Cider, Clojure, tests

;;; Commentary:

;; Emidje is a Cider plugin that provides support to run Midje tests within Emacs.

;;; Code:

(require 'cider)

(defface emidje-failure-face
  '((((class color) (background light))
     :background "orange red")
    (((class color) (background dark))
     :background "firebrick"))
  "Face for failed tests."
  :group 'emidje
  :package-version '(emidje . "0.1.0"))

(defface emidje-error-face
  '((((class color) (background light))
     :background "orange1")
    (((class color) (background dark))
     :background "orange4"))
  "Face for erring tests."
  :group 'emidje
  :package-version '(emidje . "0.1.0"))

(defface emidje-success-face
  '((((class color) (background light))
     :foreground "black"
     :background "green")
    (((class color) (background dark))
     :foreground "black"
     :background "green"))
  "Face for passing tests."
  :group 'emidje
  :package-version '(emidje . "0.1.0"))

(defface emidje-work-todo-face
  '((((class color) (background light))
     :background "yellow1")
    (((class color) (background dark))
     :background "yellow4"))
  "Face for future facts."
  :group 'emidje
  :package-version '(emidje . "0.1.0"))

(defcustom emidje-load-facts-on-eval nil
  "When set to nil (the default value), Midje facts won't be loaded on operations that cause the evaluation of Clojure forms like eval and load-file"
  :type 'boolean
  :group 'emidje
  :package-version '(emidje . "0.1.0"))

(defcustom emidje-infer-test-ns-function 'emidje-default-infer-test-ns-function
  "Function to infer the test namespace"
  :type 'symbol
  :group 'emidje
  :package-version '(emidje . "0.1.0"))

(defun emidje-default-infer-test-ns-function (current-ns)
  (let ((suffix "-test"))
    (if (string-suffix-p suffix current-ns)
        current-ns
      (concat current-ns suffix))))

(defcustom emidje-suppress-middleware-warnings nil
  "When set to t, no middleware warnings are shown on the REPL."
  :type 'boolean
  :group 'emidje
  :package-version '(emidje . "0.1.0"))

(defconst emidje-evaluation-operations (list "eval" "load-file")
  "List of nREPL operations that cause the evaluation of Clojure forms.")

(defconst emidje-report-buffer "*midje-test-report*")

(defvar emidje-supported-operations
  '((:version . "midje-nrepl-version")
    (:format-tabular . "midje-format-tabular")
    (:project . "midje-test-all")
    (:ns . "midje-test-ns")
    (:test-at-point . "midje-test")
    (:retest . "midje-retest")
    (:test-stacktrace . "midje-test-stacktrace")))

(defun emidje-render-stacktrace (causes)
  "Renders the Cider error buffer with the given causes."
  (cider-stacktrace-render
   (cider-popup-buffer cider-error-buffer
                       cider-auto-select-error-buffer
                       #'cider-stacktrace-mode)
   causes))

(defun emidje-handle-error-response (response)
  (nrepl-dbind-response response (error-message exception status)
    (cond
     (error-message (user-error error-message)
                    (exception (emidje-render-stacktrace exception))
                    (t (user-error "midje-nrepl returned the following status: %st" (mapconcat #'identity status ", ")))))))

(defun emidje-handle-nrepl-response (handler-function response)
  (nrepl-dbind-response response (status)
    (if (seq-contains status "error")
        (emidje-handle-error-response response)
      (apply handler-function (list response)))))

(defun emidje-send-request (operation-type &optional params callback)
  (cider-ensure-connected)
  (let* ((op (cdr (assq operation-type emidje-supported-operations)))
         (message (thread-last (or params ())
                    (seq-map (lambda (value)
                               (if (symbolp value)
                                   (symbol-name value)
                                 value)))
                    (append `("op" ,op)))))
    (if callback
        (cider-nrepl-send-request message (apply-partially #'emidje-handle-nrepl-response callback))
      (thread-last (cider-nrepl-send-sync-request message)
        (emidje-handle-nrepl-response #'identity)))))

(defun emidje-package-version ()
  "Gets Emidje's current version from the package header."
  (let ((version-regex "^\\([0-9]+\.[0-9]+\.[0-9]+\\)\\(.*\\)$")
        (version (pkg-info-version-info 'emidje)))
    (if (not (string-match version-regex version))
        version
      (concat (match-string 1 version) "-" (upcase (match-string 2 version))))))

(defun emidje-show-warning-on-repl (message &rest args)
  "If emidje-suppress-middleware-warnings isn't set to t, shows the message on the Cider's REPL buffer."
  (unless emidje-suppress-middleware-warnings
    (cider-repl-emit-interactive-stderr
     (apply #'format (concat "WARNING: " message
                             "\nYou can mute this warning by changing the variable emidje-suppress-middleware-warnings to t.")
            args))))

(defun emidje-check-midje-nrepl-version ()
  "Checks whether midje-nrepl is available on the project's classpath and its version matches Emidje's version.
Shows warning messages on Cider's REPL when applicable."
  (let ((emidje-version (emidje-package-version))
        (midje-nrepl-version (nrepl-dict-get-in (emidje-send-request :version) `("midje-nrepl" "version-string"))))
    (cond
     ((not midje-nrepl-version)
      (emidje-show-warning-on-repl "midje-nrepl isn't in your classpath; Emidje keybindings won't work.
 You can either start this REPL via cider-jack-in or add midje-nrepl to your profile.clj dependencies."))
     ((not (string-equal emidje-version midje-nrepl-version))
      (emidje-show-warning-on-repl "Emidje and midje-nrepl are out of sync (things will break).
Their versions are %s and %s, respectively.
Please, consider updating the midje-nrepl version in your profile.clj to %s or start the REPL via cider-jack-in." emidje-version midje-nrepl-version emidje-version)))))

(defun emidje-inject-jack-in-dependencies ()
  (add-to-list 'cider-jack-in-lein-plugins `("midje-nrepl" ,(emidje-package-version)) t))

;;;###autoload
(eval-after-load 'cider
  '(emidje-inject-jack-in-dependencies))

(add-hook 'cider-connected-hook #'emidje-check-midje-nrepl-version)

(defun emidje-show-test-stacktrace-at (ns index)
  "Shows the stacktrace for the error whose location within the report map is given by the ns and index."
  (let ((causes (list)))
    (emidje-send-request :test-stacktrace `(ns ,ns
                                               index ,index
                                               print-fn "clojure.lang/println")
                         (lambda (response)
                           (nrepl-dbind-response response (class status)
                             (cond (class  (setq causes (cons response causes)))
                                   (status (when causes
                                             (emidje-render-stacktrace (reverse causes))))))))))

(defun emidje-show-test-stacktrace ()
  "Shows the stacktrace for the erring test at point."
  (interactive)
  (let ((ns    (get-text-property (point) 'ns))
        (index (get-text-property (point) 'index))
        (error (get-text-property (point) 'error)))
    (if (and error ns index)
        (emidje-show-test-stacktrace-at ns index)
      (message "No test error at point"))))

(defun emidje-insert-section (content)
  (let* ((lines (if (stringp content)
                    (split-string content "\n")
                  (append content '("\n")))))
    (thread-last lines
      (seq-map                         #'cider-font-lock-as-clojure)
      insert-rectangle)
    (beginning-of-line)))

(defun emidje-render-one-test-result (result)
  (nrepl-dbind-response result (context expected actual error message type)
    (cl-flet ((insert-label (s)
                            (cider-insert (format "%8s: " s) 'font-lock-comment-face))
              (insert-align-label (s)
                                  (insert (format "%12s" s))))
      (cider-propertize-region (cider-intern-keys (cdr result))
        (let ((beg (point))
              (type-face (cider-test-type-simple-face type))
              (bg `(:background ,cider-test-items-background-color)))
          (if (equal type "skip")
              (cider-insert "Work To Do " 'emidje-work-todo-face nil)
            (cider-insert (capitalize type) type-face nil " in "))
          (dolist (text context)
            (cider-insert text 'font-lock-doc-face t))
          (insert "\n")
          (when expected
            (insert-label "expected")
            (emidje-insert-section expected)
            (insert "\n"))
          (when actual
            (insert-label "actual")
            (emidje-insert-section actual)
            (insert "\n"))
          (unless (seq-empty-p message)
            (insert-label "Message")
            (emidje-insert-section message))
          (when error
            (insert-label "error")
            (insert-text-button error
                                'follow-link t
                                'action '(lambda (_button) (emidje-show-test-stacktrace))
                                'help-echo "View causes and stacktrace")
            (insert "\n\n"))
          (overlay-put (make-overlay beg (point)) 'font-lock-face bg))))))

(defun emidje-count-non-passing-tests (results)
  (seq-count (lambda (result)
               (let* ((type (nrepl-dict-get result "type")))
                 (or (equal type "error")
                     (equal type "fail")))) results))

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
      (cider-insert (format "%d failures" fail) 'emidje-failure-face t))
    (unless (zerop error)
      (cider-insert (format "%d errors" error) 'emidje-error-face t))
    (unless (zerop skip)
      (cider-insert (format "%d to do" skip) 'emidje-work-todo-face t))
    (when (zerop (+ fail error))
      (cider-insert (format "%d passed" pass) 'emidje-success-face t))
    (insert "\n")))

(defun emidje-get-test-report-buffer ()
  (or (get-buffer emidje-report-buffer)
      (cider-popup-buffer emidje-report-buffer t)))

(defun emidje-kill-test-report-buffer ()
  "Kills the test report buffer if it exists."
  (when-let ((buffer (get-buffer emidje-report-buffer)))
    (kill-buffer buffer)))

(defun emidje-tests-passed-p (summary)
  "Returns t if all tests passed."
  (nrepl-dbind-response summary (fail error)
    (zerop (+ fail error))))

(defun emidje-render-test-report (results summary)
  "Renders the test report if there are erring and/or failing tests.
If the tests were successful and there's a test report buffer rendered, kills it."
  (if (emidje-tests-passed-p summary)
      (emidje-kill-test-report-buffer)
    (with-current-buffer (emidje-get-test-report-buffer)
      (emidje-report-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (cider-insert "Test Summary" 'bold t "\n")
        (emidje-render-list-of-namespaces results)
        (emidje-render-test-summary summary)
        (emidje-render-test-results results)
        (goto-char (point-min))))))

(defun emidje-echo-summary (summary)
  (nrepl-dbind-response summary (error fact fail ns pass test skip)
    (if (zerop test)
        (message (propertize "No tests were run. Is that what you wanted?"
                             'face 'emidje-error-face))
      (let ((face (cond
                   ((not (zerop error)) 'emidje-error-face)
                   ((not (zerop fail)) 'emidje-failure-face)
                   (t 'emidje-success-face))))
        (message (propertize
                  (format "Tested %d namespace(s). Ran %d assertions from %d facts. %d failures, %d errors, %d to do." ns test fact fail error skip) 'face face))))))

(defun emidje-maybe-get-test-description (sexp)
  (let ((description (thread-last (or sexp "()")
                       read-from-string
                       car
                       (nth 1))))
    (if (stringp description)
        (format "\"%s\" " description)
      "")))

(defun emidje-echo-running-tests (op-type args)
  (let* ((ns (plist-get args 'ns))
         (test-description (emidje-maybe-get-test-description (plist-get args 'test-forms))))
    (pcase op-type
      (:project (message "Running tests in all project namespaces..."))
      (:ns (message "Running tests in %s..." (cider-propertize ns 'ns)))
      (:test-at-point (message "Running test %sin %s..." (cider-propertize test-description 'bold) (cider-propertize ns 'ns)))
      (      :retest (message "Re-running non-passing tests...")))))

(defun emidje-send-test-request (operation-type &optional message)
  "Sends the test message asynchronously and shows the test report when applicable."
  (emidje-echo-running-tests operation-type message)
  (emidje-send-request operation-type message
                       (lambda (response)
                         (nrepl-dbind-response response (results summary)
                           (when (and results summary)
                             (emidje-echo-summary summary)
                             (emidje-render-test-report results summary))))))

(defun emidje-run-all-tests ()
  (interactive)
  (emidje-send-test-request :project))

(defun emidje-namespace-to-be-tested ()
  (let ((current-ns (cider-current-ns t)))
    (if (string-equal current-ns "user")
        (user-error "No namespace to be tested in the current context")
      (funcall emidje-infer-test-ns-function current-ns))))

(defun emidje-run-ns-tests ()
  (interactive)
  (let ((namespace (emidje-namespace-to-be-tested)))
    (emidje-send-test-request :ns `(ns ,namespace))))

(defun emidje-run-test-at-point ()
  (interactive)
  (let* ((ns (cider-current-ns t))
         (sexp (cider-sexp-at-point))
         (line-number (line-number-at-pos)))
    (emidje-send-test-request :test-at-point `(ns ,ns
                                                  test-forms ,sexp
                                                  line ,line-number))))

(defun emidje-re-run-failed-tests ()
  (interactive)
  (emidje-send-test-request :retest))

(defun emidje-jump-to-test-definition (&optional arg)
  (interactive "p")
  (let* ((file (or (get-text-property (point) 'file)
                   (user-error "Nothing to be visited here")))
         (line (or (get-text-property (point) 'line) 1))
         (buffer (cider--find-buffer-for-file file))
         (other-window nil))
    (if buffer
        (cider-jump-to buffer (cons line 1) other-window)
      (error "No source location"))))

(defun emidje-send-format-request (sexpr)
  (thread-first
      (emidje-send-request :format-tabular `(code ,sexpr))
    (nrepl-dict-get "formatted-code")))

(defun emidje-format-tabular ()
  (interactive)
  (save-excursion
    (mark-sexp)
    (cider--format-region (region-beginning) (region-end) #'emidje-send-format-request)))

(defun emidje-instrumented-nrepl-send-request (original-function request &rest args)
  "Instruments nrepl-send-request and nrepl-send-sync-request functions by appending the parameter load-tests? to the request when applicable"
  (let* ((op (thread-last request
               (seq-drop-while (lambda (candidate)
                                 (not (equal candidate "op"))))
               cdr
               car))
         (request (if (and emidje-load-facts-on-eval (seq-contains emidje-evaluation-operations op))
                      (append request `("load-tests?" "true"))
                    request)))
    (apply original-function request args)))

;; Adivice functions
(advice-add'nrepl-send-request :around #'emidje-instrumented-nrepl-send-request)
(advice-add 'nrepl-send-sync-request :around #'emidje-instrumented-nrepl-send-request)

(defun emidje-toggle-load-facts-on-eval ()
  "Toggles the value of emidje-load-facts-on-eval"
  (interactive)
  (setq emidje-load-facts-on-eval (not emidje-load-facts-on-eval)))

(defvar emidje-report-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'emidje-jump-to-test-definition)
    map))

(defvar emidje-commands-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-j f") #'emidje-format-tabular)
    (define-key map (kbd "C-c C-j p") #'emidje-run-all-tests)
    (define-key map (kbd "C-c C-j n") #'emidje-run-ns-tests)
    (define-key map (kbd "C-c C-j t") #'emidje-run-test-at-point)
    (define-key map (kbd "C-c C-j r") #'emidje-re-run-failed-tests)
    map))

(define-derived-mode emidje-report-mode special-mode "Test Report"
  "Major mode for presenting Midje test results.

\\{emidje-report-mode-map}"
  (when cider-special-mode-truncate-lines
    (setq-local truncate-lines t))
  (setq-local electric-indent-chars nil))

(define-minor-mode emidje-mode
  "Provides a set of keybindings for interacting with Midje tests.

With a prefix argument ARG, enable emidje-mode if ARG
is positive, and disable it otherwise.  If called from Lisp,
enable the mode if ARG is omitted or nil.

\\{emidje-commands-map}"
  :lighter "emidje"
  :keymap emidje-commands-map)

(when (fboundp 'clojure-mode)
  (add-hook 'clojure-mode-hook #'emidje-mode t))

(provide 'emidje)

;;; emidje.el ends here
