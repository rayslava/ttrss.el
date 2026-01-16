;; -*- lexical-binding: t -*-
;; Elsa configuration for nnttrss

;; Register built-in extensions
(register-extensions
 builtin
 cl
 seq)

;; Add current directory to load-path for our custom extension
(add-to-list 'load-path default-directory)

;; Load our custom nnoo extension
(require 'elsa-extension-nnoo)
