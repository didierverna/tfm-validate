;;; package.lisp --- TFM Validate package definition

;; Copyright (C) 2024 Didier Verna

;; Author: Didier Verna <didier@didierverna.net>

;; This file is part of TFM-VALIDATE.

;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


;;; Commentary:



;;; Code:

(in-package :cl-user)


(defpackage :net.didierverna.tfm-validate
  (:documentation "The TeX Font Metrics Validation package.")
  (:use :cl :net.didierverna.tfm-validate.setup)
  (:local-nicknames (:tfm :net.didierverna.tfm))
  (:export

    ;; From the :net.didierverna.tfm-validate.setup package:
    :*copyright-years*
    :*release-major-level*
    :*release-minor-level*
    :*release-status*
    :*release-status-level*
    :*release-name*
    :version

    ;; From package.lisp (this file):
    :nickname-package

    ;; From src/validate.lisp:
    :invalidate-font :invalidate-directory))


(in-package :net.didierverna.tfm-validate)

(defun nickname-package (&optional (nickname :tfm-validate))
  "Add NICKNAME to the :NET.DIDIERVERNA.TFM-VALIDATE package.
Nickname defaults to :TFM-VALIDATE."
  (rename-package :net.didierverna.tfm-validate
    (package-name :net.didierverna.tfm-validate)
    (adjoin nickname (package-nicknames :net.didierverna.tfm-validate)
	    :test #'string-equal)))

;;; package.lisp ends here
