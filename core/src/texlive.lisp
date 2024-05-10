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


;; ==========================================================================
;; Utilities
;; ==========================================================================

(defun capitalize (string)
  "Capitalize STRING and substitute dashes with spaces."
  (nsubstitute #\Space #\- (string-capitalize string)))

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




;; ==========================================================================
;; Index Rendering
;; ==========================================================================

(defun render-index-header (cts year total skipped caught warnings errors type)
  "Render index file's header to standard output."
  (format t (file-contents (merge-pathnames #p"index-header.html"
					    *templates-directory*))
    year total skipped caught warnings errors cts
    (version :long) (tfm:version :long)
    type))

(defun column-lengths (number)
  "Spread NUMBER of entries in 3 different columns as uniformly as possible.
Return the column lengths as 3 values."
  (multiple-value-bind (quotient remainder) (floor number 3)
    (case remainder
      (0 (values quotient quotient quotient))
      (1 (values (+ quotient 1) quotient quotient))
      (2 (values (+ quotient 1) (+ quotient 1) quotient)))))

(defun render-index-entry (character entries number renderer)
  "Render index file's first NUMBER ENTRIES indexes under CHARACTER.
This function arranges ENTRIES indexes to be rendered in 3 columns, vertically
from left to right, but taking care of vertical justification as much as
possible: the heights of the 3 columns may only differ by one, meaning that
only the last row may have less than 3 entries.

Rendering is done on *STANDARD-OUTPUT* by calling RENDERER for each entry."
  (format t "~%      <tr><th><a name=\"~A\">~A</a></th></tr>~%"
    (if (char= character #\#) "other" character)
    character)
  (multiple-value-bind (l1 l2 l3) (column-lengths number)
    (loop :for entries-1 :on entries
	  :for entries-2 :on (nthcdr l1 entries)
	  :for entries-3 :on (nthcdr (+ l1 l2) entries)
	  ;; #### WARNING: do this last so that the report name pointers are
	  ;; correct in the :FINALLY clause, and even if the :DO clause is not
	  ;; executed.
	  :for lines :from 1 :upto l3
	  ;; #### FIXME: this call still be improved with FORMAT list
	  ;; arguments for multiple cells
	  :do (progn
		(format t "      <tr>~%")
		(funcall renderer (car entries-1))
		(funcall renderer (car entries-2))
		(funcall renderer (car entries-3))
		(format t "      </tr>~%"))
	  :finally (cond ((> l1 l2)
			  (format t "      <tr>~%")
			  (funcall renderer (car entries-1))
			  (format t "      </tr>~%"))
			 ((> l2 l3)
			  (format t "      <tr>~%")
			  (funcall renderer (car entries-1))
			  (funcall renderer (car entries-2))
			  (format t "      </tr>~%")))))
  (values))


;; --------------------------------------------------------------------------
;; Report Index
;; --------------------------------------------------------------------------

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

;; #### NOTE: the reports are already sorted.
(defun build-font-index-file
    (cts year total skipped caught warnings errors reports)
  "Build the TeX Live TFM compliance reports font index file."
  (with-open-file (*standard-output* #p"~/tfm-validate/fonts.html"
		   :direction :output
		   :if-exists :supersede
		   :if-does-not-exist :create
		   :external-format :utf-8)
    (render-index-header cts year total skipped caught warnings errors "Font")
    (loop :with index-character := (reports-index-character reports)
	  :with next-reports := (cdr reports)
	  :with length := 1
	  :while reports
	  :for next-index-character := (reports-index-character next-reports)
	  :if (and next-reports (char= index-character next-index-character))
	    :do (setq length (1+ length) next-reports (cdr next-reports))
	  :else
	    :do (progn
		  (render-index-entry index-character reports length
				      #'render-report-index)
		  (setq length 1
			reports next-reports
			index-character next-index-character
			next-reports (cdr reports))))
    (format t "    </table>~%  </body>~%</html>~%")))


;; --------------------------------------------------------------------------
;; Issue Index
;; --------------------------------------------------------------------------

;; #### NOTE: condition type names have already been capitalized.
(defun conditions-index-character (conditions)
  "Return the next index character for CONDITIONS.
This is the first letter of the first condition type name in CONDITIONS."
  (when conditions (aref (car (first conditions)) 0)))

(defun render-condition-index (condition)
  "Render an index entry for CONDITION.
CONDITION should be a list of the form (TYPE-NAME REPORT-NAME...).
Rendering is done on *STANDARD-OUTPUT*."
  (format t "        <td></td>
	<td class=\"author-entry\">
	  <table>
	    <tr><td class=\"author-name\">~A</td></tr>~%"
    (car condition))
  (mapc (lambda (report-name &aux (html (make-pathname :type "html")))
	  (format t "<tr><td><a href=\"~A\">~A</a></td></tr>~%"
	    (namestring (merge-pathnames html report-name))
	    (pathname-name report-name)))
    (cdr condition))
  (format t "        </table>~%	</td>~%"))

;; #### NOTE: the reports are already sorted.
(defun build-issue-index-file
    (cts year total skipped caught warnings errors conditions)
  "Build the TeX Live TFM compliance reports issue index file."
  (with-open-file (*standard-output* #p"~/tfm-validate/issues.html"
		   :direction :output
		   :if-exists :supersede
		   :if-does-not-exist :create
		   :external-format :utf-8)
    (render-index-header cts year total skipped caught warnings errors "Issue")
    (loop :with index-character := (conditions-index-character conditions)
	  :with next-conditions := (cdr conditions)
	  :with length := 1
	  :while conditions
	  :for next-index-character
	    := (conditions-index-character next-conditions)
	  :if (and next-conditions (char= index-character next-index-character))
	    :do (setq length (1+ length) next-conditions (cdr next-conditions))
	  :else
	    :do (progn
		  (render-index-entry index-character conditions length
				      #'render-condition-index)
		  (setq length 1
			conditions next-conditions
			index-character next-index-character
			next-conditions (cdr conditions))))
    (format t "    </table>~%  </body>~%</html>~%")))




;; ==========================================================================
;; Report Rendering
;; ==========================================================================

(defun render-report (report cts)
  "Generate HTML file for REPORT."
  (let* ((file (merge-pathnames
		(merge-pathnames (make-pathname :type "html") (car report))
		#p"~/tfm-validate/"))
	 (root (apply #'concatenate 'string
		      (make-list (count #\/ (car report))
				 :initial-element "../")))
	 warnings errors)
    (loop :for condition :in (cdr report)
	  :if (typep condition 'tfm:tfm-compliance-warning)
	    :collect condition :into warns
	  :else
	    :collect condition :into errs
	  :finally (setq warnings warns errors errs))
    (ensure-directories-exist file)
    (with-open-file (*standard-output* file
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create
		     :external-format :utf-8)
      (format t (file-contents (merge-pathnames #p"report-header.html"
						*templates-directory*))
	(pathname-name (car report)) root (pathname-name (car report)) root
	(namestring (car report))
	(length warnings) (length errors)
	cts (version :long) (tfm:version :long))
      (flet ((report-condition (condition)
	       (format t "      <h3>~A</h3>~%"
		 (capitalize (symbol-name (type-of condition))))
	       (format t "<pre>~%")
	       (let (*print-escape*) (print-object condition *standard-output*))
	       (format t "</pre>~%")))
	(when warnings
	  (format t "    <h2>Warnings</h2>~%")
	  (mapc #'report-condition warnings))
	(when errors
	  (format t "    <h2>Errors</h2>~%")
	  (mapc #'report-condition errors)))
      (format t "  </body>~%</html>~%"))))




;; ==========================================================================
;; Entry Point
;; ==========================================================================

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
      (with-open-file (*standard-output* "~/tfm-validate/index.html"
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create
		       :external-format :utf-8)
	(format t "~
<meta http-equiv=\"refresh\" content=\"0;url=fonts.html\">~%"))
      ;; #### NOTE: we're creating the data for the issue index here, because
      ;; it allows us to display all the statistics in both index files, as a
      ;; general summary..
      (let ((cts (current-time-string))
	    (conditions (make-hash-table :test #'eq))
	    (warnings 0)
	    (errors 0))
	(mapc (lambda (report)
		(mapc (lambda (condition)
			(setf (gethash (type-of condition) conditions)
			      (pushnew (car report)
				       (gethash (type-of condition)
						conditions))))
		  (cdr report)))
	  reports)
	(setq conditions
	      (sort (loop :for key :being :the :hash-keys :in conditions
			    :using (hash-value value)
			  :if (subtypep key 'tfm:tfm-compliance-warning)
			    :do (incf warnings)
			  :else
			    :do (incf errors)
			  :collect (cons (capitalize (symbol-name key))
					 (nreverse value)))
		  #'string-lessp
		:key #'car))
	(let ((caught (length reports)))
	  (build-font-index-file cts year total skipped caught warnings errors
				 reports)
	  (build-issue-index-file cts year total skipped caught warnings errors
				  conditions))
	(mapc (lambda (report) (render-report report cts)) reports)))))

;;; texlive.lisp ends here
