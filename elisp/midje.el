;;; Code:

(require 'cider)

(defconst midje-nrepl-version "0.1.0-SNAPSHOT")

(defun midje-send-message-async (message &optional callback)
  (cider-nrepl-send-request message (or callback 'print)))

(defun midje-inject-jack-in-dependencies ()
  (add-to-list 'cider-jack-in-lein-plugins `("midje-nrepl" ,midje-nrepl-version) t))

;;;###autoload
(eval-after-load 'cider
  '(midje-inject-jack-in-dependencies))

(provide 'midje)
