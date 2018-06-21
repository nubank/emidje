;;; emidje.el --- Test runner and report viewer for Midje -*- lexical-binding: t -*-

;; Author: Alan Ghelardi <alan.ghelardi@nubank.com.br>
;; Maintainer: Alan Ghelardi <alan.ghelardi@nubank.com.br>
;; Version: 0.1.0-snapshot
;; Package-Requires: ((cider "0.17.0-snapshot"))
;; Homepage: https://github.com/alan-ghelardi/emidje
;; Keywords: cider, clojure, midje, test

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary: Emidje is a Cider plugin that provides support to run Midje tests within Emacs.

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

(defconst emidje-report-buffer "*emidje-test-report*")

(defconst midje-nrepl-version "0.1.0-SNAPSHOT")

(defun emidje-inject-jack-in-dependencies ()
  (add-to-list 'cider-jack-in-lein-plugins `("midje-nrepl" ,midje-nrepl-version) t))

;;;###autoload
(eval-after-load 'cider
  '(emidje-inject-jack-in-dependencies))

(defvar emidje-supported-operations
  '((:ns . "midje-test-ns")
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

(defun emidje-send-request (operation-type params callback)
  (let* ((op (cdr (assq operation-type emidje-supported-operations)))
         (message (thread-last params
                    (seq-map (lambda (value)
                               (if (symbolp value)
                                   (symbol-name value)
                                 value)))
                    (append `("op" ,op)))))
    (cider-nrepl-send-request message
                              callback)))

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
              (cider-insert "Work To Do " 'midje-work-todo-face nil)
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
      (cider-popup-buffer emidje-report-buffer t)))

(defun emidje-render-test-report (results summary)
  (with-current-buffer (emidje-get-test-report-buffer)
    (emidje-report-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (cider-insert "Test Summary" 'bold t "\n")
      (emidje-render-list-of-namespaces results)
      (emidje-render-test-summary summary)
      (emidje-render-test-results results)
      (goto-char (point-min)))))

(defun emidje-echo-summary (summary)
  (nrepl-dbind-response summary (error fact fail ns pass test skip)
    (if (zerop test)
        (message (propertize "No tests were run. Is that what you wanted?"
                             'face 'cider-test-error-face))
      (let ((face (cond
                   ((not (zerop error)) 'cider-test-error-face)
                   ((not (zerop fail)) 'cider-test-failure-face)
                   'cider-test-success-face)))
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

(defun emidje-run-ns-tests ()
  (interactive)
  (if-let* ((namespace (cider-current-ns t)))
      (emidje-send-test-request :ns `(ns ,namespace))
    (message "No namespace to be tested in the current context")))

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

(defun emidje-format-tabular-sync (sexpr)
  (thread-first
      (cider-nrepl-send-sync-request `("op" "midje-format-tabular"
                                       "code" ,sexpr))
    (nrepl-dict-get "code")))

(defun emidje-format-tabular ()
  (interactive)
  (save-excursion
    (mark-sexp)
    (cider--format-region (region-beginning) (region-end) #'emidje-format-tabular-sync)))

(defvar emidje-report-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'emidje-jump-to-test-definition)
    map))

(defvar emidje-commands-map
  (let ((map (define-prefix-command 'emidje-commands-map)))
    (define-key map (kbd "C-c C-j") 'emidje-commands-map)
    (define-key map (kbd "n") #'emidje-run-ns-tests)
    (define-key map (kbd "t") #'emidje-run-test-at-point)
    (define-key map (kbd "r") #'emidje-re-run-failed-tests)
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
