;;; midje.el --- Test result viewer for Midje (https://github.com/marick/Midje) -*- lexical-binding: t -*-

;;; Code:

(require 'cider)
(require 'midje-test-report)

(defconst midje-nrepl-version "0.1.0-SNAPSHOT")

(defun midje-inject-jack-in-dependencies ()
  (add-to-list 'cider-jack-in-lein-plugins `("midje-nrepl" ,midje-nrepl-version) t))

;;;###autoload
(eval-after-load 'cider
  '(midje-inject-jack-in-dependencies))

(defvar midje-supported-operations
  '((:ns . "midje-test-ns")
    (:test-at-point . "midje-test")
    (:retest . "midje-retest")))

(defun midje-send-test-request (operation-type &rest params)
  (let* ((op (cdr (assq operation-type midje-supported-operations)))
         (message (apply 'list (append `("op" ,op) params))))
    (cider-nrepl-send-request message
                              (lambda (response)
                                (nrepl-dbind-response response (results summary)
                                  (when results
                                    (midje-render-test-report results summary)))))))

(defun midje-run-ns-tests ()
  (interactive)
  (if-let* ((namespace (cider-current-ns t)))
      (midje-send-test-request :ns "ns" namespace)
    (message "No namespace to be tested in the current context")))

(provide 'midje)
