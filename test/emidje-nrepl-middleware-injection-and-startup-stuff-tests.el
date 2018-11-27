;;; emidje-nrepl-middleware-injection-and-startup-stuff-tests --- Tests for Emidje -*- lexical-binding: t -*-

;; Copyright © 2018 Nubank

;; This file is not part of GNU Emacs

;;; Commentary:

;; Tests for Emidje.

;;; Code:

(require 'buttercup)
(require 'cider)
(require 'emidje)
(require 'emidje-test-helpers)

(describe "When I call `emidje-version'"
          (let ((source "/home/john-doe/.emacs.d/elpa/emidje-2a57c2b/emidje.el"))

            (it "returns the version defined in emidje.el"
                (spy-on 'pkg-info-library-source :and-return-value source)
                (spy-on 'lm-version :and-return-value "1.0.1")
                (expect (emidje-version) :to-equal "1.0.1")
                (expect 'pkg-info-library-source :to-have-been-called-with 'emidje)
                (expect 'lm-version :to-have-been-called-with source))

            (it "treats versions with qualifiers properly"
                (spy-on 'pkg-info-library-source :and-return-value source)
                (spy-on 'lm-version :and-return-value "1.0.1-SNAPSHOT")
                (expect (emidje-version) :to-equal "1.0.1-SNAPSHOT")
                (expect 'pkg-info-library-source :to-have-been-called-with 'emidje)
                (expect 'lm-version :to-have-been-called-with source))

            (it "throws an error when the version was mistakenly declared"
                (spy-on 'pkg-info-library-source :and-return-value source)
                (spy-on 'lm-version :and-return-value "1.0")
                (expect (emidje-version) :to-throw 'error))

            (it "validates that the current version was declared properly"
                (expect (emidje-version) :not :to-throw))))

(describe "When I call `emidje-show-warning-on-repl'"
          (before-each
           (spy-on 'cider-repl-emit-interactive-stderr))

          (describe "and `emidje-suppress-nrepl-middleware-warnings' is set to nil"
                    (before-all
                     (setq-local emidje-suppress-nrepl-middleware-warnings nil))

                    (it "shows a warning message in the Cider REPL buffer"
                        (emidje-show-warning-on-repl "Be careful!")
                        (expect 'cider-repl-emit-interactive-stderr :to-have-been-called-with
                                "WARNING: Be careful!
You can mute this warning by changing the variable emidje-suppress-nrepl-middleware-warnings to t."))

                    (it "can interpolate arbitrary arguments into the supplied message"
                        (emidje-show-warning-on-repl "%s, be careful!" "John Doe")
                        (expect 'cider-repl-emit-interactive-stderr :to-have-been-called-with
                                "WARNING: John Doe, be careful!
You can mute this warning by changing the variable emidje-suppress-nrepl-middleware-warnings to t.")))

          (describe "and `emidje-suppress-nrepl-middleware-warnings' is set to t"
                    (before-all
                     (setq-local emidje-suppress-nrepl-middleware-warnings t))

                    (it "doesn't show any message"
                        (emidje-show-warning-on-repl "Hey, how are you?")
                        (expect 'cider-repl-emit-interactive-stderr :not :to-have-been-called))))

(describe "When I call `emidje-check-nrepl-middleware-version'"
          (before-each
           (spy-on 'emidje-version :and-return-value "1.0.1"))

          (describe "and `midje-nrepl' isn't in the project's classpath"

                    (it "shows a warning message in the Cider REPL buffer"
                        (spy-on 'emidje-send-request :and-return-value (nrepl-dict "status" (list "done")))
                        (spy-on 'emidje-show-warning-on-repl)
                        (emidje-check-nrepl-middleware-version)
                        (expect 'emidje-send-request :to-have-been-called-with :version)
                        (expect 'emidje-show-warning-on-repl :to-have-been-called-with
                                "midje-nrepl isn't in your classpath; Emidje keybindings won't work!
 You can either start this REPL via cider-jack-in or add midje-nrepl to your profile.clj dependencies.")))

          (describe "and the versions of Emidje and `midje-nrepl' are out of sync"

                    (it "shows a warning message in the Cider REPL buffer"
                        (spy-on 'emidje-send-request :and-return-value (nrepl-dict "status" (list "done")
                                                                                   "midje-nrepl" (nrepl-dict
                                                                                                  "version-string" "1.0.0")))
                        (spy-on 'emidje-show-warning-on-repl)
                        (emidje-check-nrepl-middleware-version)
                        (expect 'emidje-send-request :to-have-been-called-with :version)
                        (expect 'emidje-show-warning-on-repl :to-have-been-called-with
                                "Emidje and midje-nrepl are out of sync. Things will break!
Their versions are %s and %s, respectively.
Please, consider updating the midje-nrepl version in your profile.clj to %s or start the REPL via cider-jack-in."
                                "1.0.1" "1.0.0" "1.0.1")))

          (describe "and the versions of Emidje and `midje-nrepl' are in sync"

                    (it "doesn't show any warning message"
                        (spy-on 'emidje-send-request :and-return-value (nrepl-dict "status" (list "done")
                                                                                   "midje-nrepl" (nrepl-dict
                                                                                                  "version-string" "1.0.1")))
                        (spy-on 'emidje-show-warning-on-repl)
                        (emidje-check-nrepl-middleware-version)
                        (expect 'emidje-send-request :to-have-been-called-with :version)
                        (expect 'emidje-show-warning-on-repl :not :to-have-been-called))))

(describe "When I call `emidje-enable-nrepl-middleware'"
          (before-each
           (setq-local cider-jack-in-lein-plugins nil))

          (it "adds `emidje-check-nrepl-middleware-version' to `cider-connected-hook'"
              (emidje-enable-nrepl-middleware)
              (expect cider-connected-hook :to-contain #'emidje-check-nrepl-middleware-version))

          (describe "and `emidje-inject-nrepl-middleware-at-jack-in' is set to t"
                    (before-all
                     (setq-local emidje-inject-nrepl-middleware-at-jack-in t))

                    (it "adds `midje-nrepl' to the list of Leiningen plugins injected by Cider at jack-in"
                        (spy-on 'emidje-version :and-return-value "1.0.1")
                        (emidje-enable-nrepl-middleware)
                        (expect cider-jack-in-lein-plugins :to-contain `("nubank/midje-nrepl" "1.0.1"))))

          (describe "and `emidje-inject-nrepl-middleware-at-jack-in' is set to nil"
                    (before-all
                     (setq-local emidje-inject-nrepl-middleware-at-jack-in nil))

                    (it "doesn't add `midje-nrepl' to the list of Leiningen plugins injected by Cider at jack-in"
                        (emidje-enable-nrepl-middleware)
                        (expect cider-jack-in-lein-plugins :to-be nil))))
