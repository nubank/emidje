;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((emacs-lisp-mode . ((indent-tabs-mode . nil)
                     (elisp-lint-ignored-validators . ("fill-column"))
                     (elisp-lint-indent-specs . ((cider-propertize-region . 1)
                                                 (cl-flet . 1)
                                                 (if-let . 2)
                                                 (nrepl-dbind-response . 2)
                                                 (thread-first . 1)
                                                 (thread-last . 1)
                                                 (when-let . 1)
                                                 )))))
