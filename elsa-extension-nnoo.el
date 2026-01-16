;;; elsa-extension-nnoo.el --- Elsa extension for nnoo/Gnus macros -*- lexical-binding: t -*-

;;; Commentary:

;; This extension teaches Elsa about the nnoo macros used in Gnus backends:
;; - deffoo: defines a backend function (expands to defun)
;; - defvoo: defines a server-local variable (expands to defvar)
;; - nnoo-declare: declares a backend
;; - nnoo-define-basics: defines basic backend functions

;;; Code:

(require 'elsa-analyser)
(require 'elsa-types)

;; Analyse deffoo as defun
(defalias 'elsa--analyse:deffoo 'elsa--analyse:defun)

;; Analyse defvoo as defvar
(defalias 'elsa--analyse:defvoo 'elsa--analyse:defvar)

;; Analyse nnoo-declare - takes a symbol, returns nil
;; Don't analyse arguments to avoid "free variable" errors
(defun elsa--analyse:nnoo-declare (form _scope _state)
  "Analyse nnoo-declare FORM - just set type to nil."
  (oset form type (elsa-type-nil)))

;; Analyse nnoo-define-basics - takes a symbol, returns nil
(defun elsa--analyse:nnoo-define-basics (form _scope _state)
  "Analyse nnoo-define-basics FORM - just set type to nil."
  (oset form type (elsa-type-nil)))

;; Analyse gnus-declare-backend - takes string and symbols
(defun elsa--analyse:gnus-declare-backend (form _scope _state)
  "Analyse gnus-declare-backend FORM - just set type to nil."
  (oset form type (elsa-type-nil)))

;; Analyse nnoo-change-server - returns t
(defun elsa--analyse:nnoo-change-server (form scope state)
  "Analyse nnoo-change-server FORM."
  (elsa--analyse-function-call form scope state)
  (oset form type (elsa-type-t)))

(provide 'elsa-extension-nnoo)
;;; elsa-extension-nnoo.el ends here
