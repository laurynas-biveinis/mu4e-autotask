;; -*- mode: eask; lexical-binding: t -*-

(package "mu4e-autotask"
         "0.1"
         "Email automation for mu4e")

(author "Laurynas Biveinis" "laurynas.biveinis@gmail.com")
(license "GPLv3")
(website-url "https://github.com/laurynas-biveinis/mu4e-autotask")
(keywords "mail")

(package-file "mu4e-autotask.el")

(script "test" "eask test ert mu4e-autotask-test.el")

(script "org-lint"
        (concat "emacs -Q --batch "
                "--eval \"(require 'org)\" "
                "--eval \"(require 'org-lint)\" "
                "--eval \"(let ((all-checks-passed t) "
                        "(files '(\\\"README.org\\\"))) "
                    "(dolist (file files all-checks-passed) "
                      "(with-temp-buffer "
                        "(insert-file-contents file) "
                        "(org-mode) "
                        "(let ((results (org-lint))) "
                          "(when results "
                            "(message \\\"Found issues in %s: %S\\\" file results) "
                            "(setq all-checks-passed nil))))) "
                    "(unless all-checks-passed (kill-emacs 1)))\""))

(source "gnu")

;; Emacs 29.1+: the built-in calendar-invitation handling relies on
;; gnus-icalendar's uninvited-attendee ("party crasher") RSVP fallback, added
;; in Emacs 29.1.
(depends-on "emacs" "29.1")

;; mu4e is not an ELPA package; it ships with mu.  Locate its installed Lisp
;; directory and add it to `load-path' so byte-compilation, linting, and tests
;; can load it.  Override with the MU4E_LISP_DIR environment variable.
(let* ((brew-prefix
        (car (split-string
              (shell-command-to-string "brew --prefix mu 2>/dev/null")
              "\n" t)))
       (candidates
        (delq
         nil
         (list (getenv "MU4E_LISP_DIR")
               (and brew-prefix
                    (expand-file-name "share/emacs/site-lisp/mu/mu4e"
                                      brew-prefix))
               "/usr/share/emacs/site-lisp/mu4e"
               "/usr/share/emacs/site-lisp/mu/mu4e"
               ;; Ubuntu 24.04 (Noble) apt installs mu4e under a
               ;; version-stamped elpa-src directory.
               (car (file-expand-wildcards
                     "/usr/share/emacs/site-lisp/elpa-src/mu4e-*")))))
       (mu4e-dir (seq-find #'file-directory-p candidates)))
  (when mu4e-dir
    (load-paths mu4e-dir)))
