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

;; #### NOTE: the entries are already sorted.
(defun build-index-file
    (type output-directory
     title cts year total skipped caught warnings errors
     entries index-character-getter index-entry-renderer)
  "Build the TeX Live TFM compliance reports TYPE index file."
  (with-open-file (*standard-output*
		   (merge-pathnames
		    (make-pathname :name (concatenate 'string type "s")
				   :type "html")
		    output-directory)
		   :direction :output
		   :if-exists :supersede
		   :if-does-not-exist :create
		   :external-format :utf-8)
    (render-index-header cts year total skipped caught warnings errors title)
    (loop :with index-character := (funcall index-character-getter entries)
	  :with next-entries := (cdr entries)
	  :with length := 1
	  :while entries
	  :for next-index-character
	    := (funcall index-character-getter next-entries)
	  :if (and next-entries (char= index-character next-index-character))
	    :do (setq length (1+ length) next-entries (cdr next-entries))
	  :else
	    :do (progn
		  (render-index-entry index-character entries length
				      index-entry-renderer)
		  (setq length 1
			entries next-entries
			index-character next-index-character
			next-entries (cdr entries))))
    (format t "    </table>~%  </body>~%</html>~%")))


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




;; ==========================================================================
;; Report Rendering
;; ==========================================================================

(defun render-report (report output cts)
  "Generate HTML file for REPORT."
  (let* ((file (merge-pathnames
		   (merge-pathnames (make-pathname :type "html") (car report))
		 output))
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

(defun invalidate-texlive-directory (directory output header)
  "Evaluate TeXlive DIRECTORY's conformance to the TFM format.
Generate a compliance reports website in OUTPUT directory.
Advertise TeXlive information with HEADER in index files.
The fonts are found in DIRECTORY/fonts/tfm/."
  (multiple-value-bind (reports total)
      (invalidate-directory (merge-pathnames #p"fonts/tfm/" directory))
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
      (renew-directories output)
      (copy-style-sheets output)
      (with-open-file (*standard-output*
		       (merge-pathnames
			   (make-pathname :name "index" :type "html")
			 output)
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
	  (build-index-file
	   "font" output
	   "Non Compliant Fonts" cts header total skipped caught warnings errors
	   reports #'reports-index-character #'render-report-index)
	  (build-index-file
	   "issue" output
	   "Issues" cts header total skipped caught warnings errors
	   conditions #'conditions-index-character #'render-condition-index))
	(mapc (lambda (report) (render-report report output cts))
	  reports)))))

(defun check-dirname (string)
  "Make sure that STRING ends with a slash. Return checked STRING."
  (check-type string string)
  (unless (char= (aref string (1- (length string))) #\/)
    (setq string (concatenate 'string string "/")))
  string)

(defun invalidate-texlive
    (&key (root nil rootp)
	  (fonts :dist)
	  ;; Used in FORMAT: conveniently, may be a string or a number.
	  (year (multiple-value-bind (result ignore status)
		    (uiop:run-program "kpsewhich -var-value SELFAUTOPARENT"
		    :output :string :ignore-error-status t)
		  (declare (ignore ignore))
		  (if (zerop status)
		    ;; There's a newline at the end.
		    (subseq result (1+ (position #\/ result :from-end t))
		      (1- (length result)))
		    (multiple-value-bind (s mi h d mo year)
			(decode-universal-time (get-universal-time))
		      (declare (ignore s mi h d mo))
		      year))))
	  (directory nil directoryp)
	  (output
	   (merge-pathnames #p"tfm-validate/" (user-homedir-pathname))
	   outputp)
     &aux header)
  "Evaluate a TeXlive installation's conformance to the TFM format.
Generate a compliance reports website in OUTPUT directory
(~/tfm-validate/ by default).

The fonts checked by this function must be located in DIRECTORY/fonts/tfm/.
If DIRECTORY is not provided, the location is determined as follows.

- The TeXlive installation's root directory may be specified by ROOT.
  Otherwise, it is determined by calling 'kpsewhich', and if that fails, it
  defaults to /usr/local/texlive/.
- The TeXlive installation's year directory may be specified by YEAR.
  Otherwise, it is determined by calling 'kpsewhich', and if that fails, it
  defaults to the current year.
- The specific fonts to check is specified by FONTS, as follows.
  * A value of :local means ROOT/texmf-local/.
  * A value of :var means ROOT/YEAR/texmf-var/.
  * A value of :dist (the default) means ROOT/YEAR/texmf-dist/."
  (when outputp (setq output (check-dirname output)))
  (cond
    (directoryp
     (setq directory (check-dirname directory))
     (setq header directory))
    (t
     (cond
       (rootp
	(setq root (check-dirname root)))
       (t
	(setq root
	      (multiple-value-bind (result ignore status)
		  (uiop:run-program "kpsewhich -var-value SELFAUTOGRANDPARENT"
		    :output :string :ignore-error-status t)
		(declare (ignore ignore))
		(if (zerop status)
		  ;; There's a newline at the end, but no slash.
		  (nsubstitute #\/ #\Newline result)
		  "/usr/local/texlive/")))))
     (setq header (ecase fonts
		    (:local "texmf-local/")
		    (:var (format nil "~A/texmf-var/" year))
		    (:dist (format nil "~A/texmf-dist/" year))))
     (setq directory (format nil "~A~A" root header))))
  (invalidate-texlive-directory directory output header))

;;; texlive.lisp ends here
