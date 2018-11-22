;;; emidje-test-helpers.el --- Helpers for testing Emidje -*- lexical-binding: t -*-

;; Copyright © 2018 Nubank

;; This file is not part of GNU Emacs

;;; Commentary:

;; Helper functions for testing Emidje.

;;; Code:

(require 'emidje)
(require 'seq)

(defvar emidje-tests-op-alias nil)

(defvar emidje-tests-sent-request nil)

(defmacro emidje-tests-with-temp-buffer (content &rest body)
  "Insert CONTENT in a temporary buffer and evaluate BODY within the same."
  (declare (debug t))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     (switch-to-buffer (current-buffer))
     ,@body))

(defun emidje-tests-last-displayed-message (&optional n)
  "Return the last nth message displayed in the echo area.
N defaults to 1, meaning the last message shown."
  (with-current-buffer (get-buffer "*Messages*")
    (let ((content (buffer-string))
          (n (or n 1)))
      (thread-last (split-string content "\n")
        seq-reverse
        (seq-drop-while #'string-empty-p)
        (nth (- n 1))))))

(defun emidje-tests-report-content ()
  "Return the report buffer's content as a string with no properties."
  (when-let ((report-buffer (get-buffer emidje-test-report-buffer)))
    (with-current-buffer report-buffer
      (string-trim (buffer-substring-no-properties (point-min) (point-max))))))

(defun emidje-tests-fake-send-request-function (response)
  (lambda (op-alias request callback)
    (setq emidje-tests-op-alias op-alias)
    (setq emidje-tests-sent-request request)
    (funcall callback response)))

(provide 'emidje-test-helpers)

;;; emidje-test-helpers.el ends here
