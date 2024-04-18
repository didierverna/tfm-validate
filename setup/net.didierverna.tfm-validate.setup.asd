;;; net.didierverna.tfm-validate.setup.asd --- ASDF system definition

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

(defsystem :net.didierverna.tfm-validate.setup
  :long-name "Tex Font Metrics Validation Setup Library"
  :description "TFM Validate's preload setup library"
  :long-description "\
The TFM Validate Setup library provides support for various preload
configuration parameters and meta-utilities. For a more complete description
of TFM Validate, see the `net.didierverna.tfm-validate' system."
  :author "Didier Verna"
  :mailto "didier@didierverna.net"
  :homepage "http://www.lrde.epita.fr/~didier/software/lisp/typesetting.php#tfm-validate"
  :source-control "https://github.com/didierverna/tfm-validate"
  :license "BSD"
    :version (:read-file-line #p"../make/version.make"
	     :at (1 (lambda (str) (subseq str 19))))
  :depends-on (:named-readtables)
  :serial t
  :components ((:file "package")
	       (:module "src"
		:components ((:file "configuration")
			     (:file "readtable" :depends-on ("configuration"))
			     (:file "version" :depends-on ("readtable"))))))

;;; net.didierverna.tfm-validate.setup.asd ends here
