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

(defun reports-index-character (reports)
  "Return the next index character for REPORTS.
This is the first (upcased) letter of the first font file name
(sans directory) in REPORTS if it's alphabetic, or # otherwise."
  (when reports
    (let ((char (aref (pathname-name (car (first reports))) 0)))
      (if (alpha-char-p char) (char-upcase char) #\#))))

(defun render-report-index (report &aux (html (make-pathname :type "html")))
  "Render an index for REPORT. Rendering is done on *STANDARD-OUTPUT*."
  (format t "	<td></td><td><a href=\"~A\">~A</a></td>~%"
    (namestring (merge-pathnames html (car report)))
    (pathname-name (car report))))

(defun column-lengths (number)
  "Spread NUMBER of entries in 3 different columns as uniformly as possible.
Return the column lengths as 3 values."
  (multiple-value-bind (quotient remainder) (floor number 3)
    (case remainder
      (0 (values quotient quotient quotient))
      (1 (values (+ quotient 1) quotient quotient))
      (2 (values (+ quotient 1) (+ quotient 1) quotient)))))

(defun render-index-entry (character reports number)
  "Render index file's first NUMBER REPORTS indexes under CHARACTER.
This function arranges REPORTS indexes to be rendered in 3 columns, vertically
from left to right, but taking care of vertical justification as much as
possible: the heights of the 3 columns may only differ by one, meaning that
only the last row may have less than 3 entries.

Rendering is done on *STANDARD-OUTPUT*."
  (format t "~%      <tr><th><a name=\"~A\">~A</a></th></tr>~%"
    (if (char= character #\#) "other" character)
    character)
  (multiple-value-bind (l1 l2 l3) (column-lengths number)
    (loop :for reports-1 :on reports
	  :for reports-2 :on (nthcdr l1 reports)
	  :for reports-3 :on (nthcdr (+ l1 l2) reports)
	  ;; #### WARNING: do this last so that the report name pointers are
	  ;; correct in the :FINALLY clause, and even if the :DO clause is not
	  ;; executed.
	  :for lines :from 1 :upto l3
	  ;; #### FIXME: this call still be improved with FORMAT list
	  ;; arguments for multiple cells
	  :do (progn
		(format t "      <tr>~%")
		(render-report-index (car reports-1))
		(render-report-index (car reports-2))
		(render-report-index (car reports-3))
		(format t "      </tr>~%"))
	  :finally (cond ((> l1 l2)
			  (format t "      <tr>~%")
			  (render-report-index (car reports-1))
			  (format t "      </tr>~%"))
			 ((> l2 l3)
			  (format t "      <tr>~%")
			  (render-report-index (car reports-1))
			  (render-report-index (car reports-2))
			  (format t "      </tr>~%")))))
  (values))

(defun build-index-file (year total skipped reports)
  "Build the TeX Live TFM compliance reports index file."
  (with-open-file (*standard-output* #p"~/tfm-validate/index.html"
		   :direction :output
		   :if-exists :supersede
		   :if-does-not-exist :create
		   :external-format :utf-8)
    (render-index-header year total skipped (length reports))
    (loop :with index-character := (reports-index-character reports)
	  :with next-reports := (cdr reports)
	  :with length := 1
	  :while reports
	  :for next-index-character := (reports-index-character next-reports)
	  :if (and next-reports (char= index-character next-index-character))
	    :do (setq length (1+ length) next-reports (cdr next-reports))
	  :else
	    :do (progn
		  (render-index-entry index-character reports length)
		  (setq length 1
			reports next-reports
			index-character next-index-character
			next-reports (cdr reports))))
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
      (setq reports (sort reports #'string-lessp :key #'car))
      (setq reports (stable-sort reports #'string-lessp
				 :key (lambda (report)
					(pathname-name (car report)))))
      (renew-directories #p"~/tfm-validate/")
      (copy-style-sheets #p"~/tfm-validate/")
      (build-index-file year total skipped reports))))

;;; texlive.lisp ends here
