;;; util.lisp --- Common utilities

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


;;; Code:

(in-package :net.didierverna.tfm-validate)
(in-readtable :net.didierverna.tfm-validate)


;; ==========================================================================
;; Distribution Utilities
;; ==========================================================================

(defvar *top-directory*
  (asdf:system-source-directory :net.didierverna.tfm-validate)
  "TFM Validate's top directory.")

(defvar *share-directory* (merge-pathnames #p"share/" *top-directory*)
  "TFM Validate's share directory.")

(defvar *css-directory* (merge-pathnames #p"css/" *share-directory*)
  "TFM Validate's CSS directory.")

(defvar *templates-directory* (merge-pathnames #p"templates/" *share-directory*)
  "TFM Validate's templates directory.")




;; ==========================================================================
;; General
;; ==========================================================================

(defun decoded-time-string
    (second minute hour date month year day-of-week dst-p tz)
  "Return decoded time as a string."
  (declare (ignore dst-p))
  (format nil "~A ~A ~2,'0D ~2,'0D:~2,'0D:~2,'0D ~D GMT~@D"
    (nth day-of-week '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
    (nth (1- month) '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug"
		      "Sep" "Oct" "Nov" "Dec"))
    date
    hour
    minute
    second
    year
    (- tz)))

(defun universal-time-string (&optional time)
  "Return universal TIME (current time by default) as a string."
  (apply #'decoded-time-string
    (multiple-value-list
     (decode-universal-time (or time (get-universal-time))))))




;; ==========================================================================
;; File System Utilities
;; ==========================================================================

(defun file-contents (file)
  "Attempt to safely read FILE into a string and return it.
Safely means try to detect a proper encoding."
  (handler-case (uiop:read-file-string file)
    (error ()
      (handler-case (uiop:read-file-string file :external-format :latin-1)
	(error ()
	  (uiop:read-file-string file
	     :external-format '(:utf-8 :replacement #\?)))))))

;;; util.lisp ends here
