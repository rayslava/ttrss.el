(package "nnttrss"
         "0.1.0"
         "Gnus backend for Tiny Tiny RSS")

(website-url "https://github.com/rayslava/ttrss.el")
(keywords "news" "rss" "gnus")

(package-file "nnttrss.el")
(files "ttrss.el" "nnttrss.el" "nnttrss-storage.el" "nnttrss-sync.el")

(source "gnu")
(source "melpa")

(depends-on "emacs" "29.1")

(development
 (depends-on "dash")
 (depends-on "shut-up")
 (depends-on "ert-runner")
 (depends-on "undercover")
 (depends-on "elsa"))

(script "test" "eask test ert tests/*.el")
(script "coverage" "eask eval \"(load \\\"scripts/run-coverage.el\\\")\"")
(script "lint" "eask lint checkdoc && eask lint package")
(script "compile" "eask compile")
