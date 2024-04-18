;;; texlive.lisp --- TeX Live Validation

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
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTUOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


;;; Commentary:



;;; Code:

(in-package :net.didierverna.tfm-validate)
(in-readtable :net.didierverna.tfm-validate)


(defun renew-directories (&rest directories)
  "Ensure DIRECTORIES exist and empty them."
  (dolist (directory directories)
    (uiop:delete-directory-tree directory
      :validate t :if-does-not-exist :ignore)
    (ensure-directories-exist directory))
  (values))

(defun copy-style-sheets (directory)
  "Copy TFM Validate'style sheets to DIRECTORY."
  (let ((style-sheets '(#p"document.css" #p"index.css")))
    (dolist (style-sheet style-sheets)
      (uiop:copy-file
       (merge-pathnames style-sheet *css-directory*)
       (merge-pathnames style-sheet directory))))
  (values))

(defun render-index-header (year total skipped caught)
  "Render index file's header to standard output."
  (format t (file-contents (merge-pathnames #p"index-header.html"
					    *templates-directory*))
    year total skipped caught (current-time-string)
    (version :long)
    (tfm:version :long)))

(defun build-index-file (year total skipped reports)
  "Build the TeX Live TFM compliance reports index file."
  (with-open-file (*standard-output* #p"~/tfm-validate/index.html"
		   :direction :output
		   :if-exists :supersede
		   :if-does-not-exist :create
		   :external-format :utf-8)
    (render-index-header year total skipped (length reports))
    (format t "    </table>~%  </body>~%</html>~%")))

(defun invalidate-texlive (year)
  "Evaluate TeX Live YEAR distribution's conformance to the TFM format."
  (multiple-value-bind (reports total)
      (invalidate-directory
       (format nil "/usr/local/texlive/~A/texmf-dist/fonts/tfm/" year))
    (let ((skipped 0))
      (loop :for report :in reports
	    :if (typep (second report) 'tfm:extended-tfm)
	      :do (incf skipped)
	    :else :collect report :into retained
	    :finally (setq reports retained))
      (renew-directories #p"~/tfm-validate/")
      (copy-style-sheets #p"~/tfm-validate/")
      (build-index-file year total skipped reports))))

;;; texlive.lisp ends here
