;;; net.didierverna.tfm-validate.asd --- ASDF system definition

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

(defsystem :net.didierverna.tfm-validate
  :long-name "Tex Font Metrics Validation Library"
  :description "A Common Lisp validation library for TeX Font Metrics"
  :long-description "\
TFM (for TeX Font Metrics) is the standard font description format used by
TeX. The TFM Validate library provides facilities for checking the
correctness of TFM files and generating non-compliance reports."
  :author "Didier Verna"
  :mailto "didier@didierverna.net"
  :homepage "http://www.lrde.epita.fr/~didier/software/lisp/typesetting.php#tfm-validate"
  :source-control "https://github.com/didierverna/tfm-validate"
  :license "BSD"
  :version (:read-file-line #p"make/version.make"
	     :at (1 (lambda (str) (subseq str 19))))
  :depends-on (:net.didierverna.tfm-validate.core)
  :components
  ((:module "share"
    :components ((:module "css"
		  :components
			  ((:static-file "document.css")
			   (:static-file "index.css")))
		 (:module "templates"
		  :components
			  ((:static-file "index-header.html")))))))

;;; net.didierverna.tfm-validate.asd ends here
