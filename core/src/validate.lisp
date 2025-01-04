;;; validate.lisp --- Validation

;; Copyright (C) 2024, 2025 Didier Verna

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

(in-package :net.didierverna.tfm-validate)
(in-readtable :net.didierverna.tfm-validate)


(defun invalidate-font (file &aux conditions)
  "Evaluate FILE's conformance to the TeX Font Metrics format.
If FILE is loaded without any warnings or errors, return NIL.
Otherwise, return a list of conditions raised during loading."
  (labels ((collect (condition) (push condition conditions))
	   (collect-and-muffle (condition)
	     (collect condition)
	     (invoke-restart 'muffle-warning))
	   (collect-and-cancel (condition)
	     (collect condition)
	     (invoke-restart 'tfm:cancel-loading))
	   (collect-and-set-to-zero (condition)
	     (collect condition)
	     (invoke-restart 'tfm:set-to-zero))
	   (collect-and-discard-ligature (condition)
	     (collect condition)
	     (invoke-restart 'tfm:discard-ligature))
	   (collect-and-discard-string (condition)
	     (collect condition)
	     (invoke-restart 'tfm:discard-string)))
    (macrolet ((collect-and-restart-with (restart)
		 `(lambda (condition)
		    (collect condition)
		    (invoke-restart ',restart))))
      (handler-bind
	  ;; Warnings.
	  ((tfm:extended-tfm #'collect-and-muffle)
	   (tfm:tfm-compliance-warning #'collect-and-muffle)
	   ;; Non-recoverable errors.
	   (tfm:file-underflow #'collect-and-cancel)
	   (tfm:u16-overflow #'collect-and-cancel)
	   (tfm:invalid-header-length #'collect-and-cancel)
	   (tfm:invalid-character-range #'collect-and-cancel)
	   (tfm:invalid-table-length #'collect-and-cancel)
	   (tfm:invalid-section-lengths #'collect-and-cancel)
	   ;; Single context recovery (non ambiguous).
	   (tfm:invalid-table-start #'collect-and-set-to-zero)
	   (tfm:fix-word-overflow #'collect-and-set-to-zero)
	   (tfm:character-list-cycle
	     (collect-and-restart-with tfm:discard-next-character))
	   (tfm:invalid-ligature-opcode #'collect-and-discard-ligature)
	   (tfm:ligature-cycle #'collect-and-discard-ligature)
	   (tfm:invalid-design-size (collect-and-restart-with tfm:set-to-ten))
	   (tfm:invalid-padded-string #'collect-and-discard-string)
	   (tfm:invalid-padded-string-length #'collect-and-discard-string)
	   (tfm:no-boundary-character
	     (collect-and-restart-with tfm:abort-lig/kern-program))
	   ;; Ambiguous errors.
	   (tfm:invalid-table-index
	     (lambda (condition)
	       (let ((restart
		       (or (find-restart 'tfm:abort-lig/kern-program condition)
			   (find-restart 'tfm:discard-extension-recipe condition)
			   (find-restart 'tfm:discard-kern condition)
			   (find-restart 'tfm:set-to-zero condition))))
		 (collect condition)
		 (invoke-restart restart))))
	   (tfm:invalid-character-code
	     (lambda (condition)
	       (let ((restart
		       (or (find-restart 'tfm:discard-ligature condition)
			   (find-restart 'tfm:discard-next-character condition)
			   (find-restart 'tfm:discard-kern condition)
			   (find-restart 'tfm:discard-extension-recipe condition)
			   (find-restart 'tfm:cancel-loading condition))))
		 (collect condition)
		 (invoke-restart restart)))))
	(tfm:load-font file))))
  (nreverse conditions))

(defun invalidate-directory (directory &aux reports (total 0))
  "Evaluate DIRECTORY's conformance to the TeX Font Metrics format.
Call INVALIDATE-FONT on every TFM file recursively found in DIRECTORY.
Return two values:
- a list of the form ((FILE CONDITIONS...) (FILE CONDITIONS...) ...)
  where FILE is a namestring relative to DIRECTORY, and CONDITIONS are those
  collected conditions while parsing the font,
- the total number of checked fonts.

If all files are compliant, the first value is NIL."
  (uiop:collect-sub*directories directory #'identity #'identity
    (lambda (sub-directory)
      (mapc (lambda (file &aux (conditions (invalidate-font file)))
	      (incf total)
	      (when conditions
		(push (cons (enough-namestring file directory) conditions)
		      reports)))
	(uiop:directory-files sub-directory "*.tfm"))))
  (values reports total))

;;; validate.lisp ends here
