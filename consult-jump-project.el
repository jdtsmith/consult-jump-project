;;; consult-jump-project.el --- Quickly jump between projects, their files and buffers with consult -*- lexical-binding: t -*-

;; Copyright (C) 2022  J.D. Smith

;; Author: J.D. Smith
;; Maintainer:  J.D. Smith <mail@daniel-mendler.de>
;; Created: 2022
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (consult "0.18"))
;; Homepage: https://github.com/jdtsmith/consult-jump-project

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Consult-jump-project provides a single interactive command of the
;; same name which provides a narrowable list of project-related
;; targets, ala consult-buffer (which it uses):
;; 
;;   - project files (f)
;;   - project buffers (b)
;;   - other projects (p)
;;
;; If the file from which consult-jump-project is invoked is not in
;; any project, you are prompted only with the full list of projects
;; known to Emacs (via project.el).  While selecting projects, a
;; preview is provided in the form of a dired buffer visiting its
;; root.  Information on the number of recent files and buffers, and
;; the abbreviated age of the newest of these is shown.
;;
;; Upon selecting a project, the interface is re-entered, allowing you
;; to select one of its recent files or open buffers, or to switch to
;; yet another project.
;;
;; Usage: simply bind `consult-jump-project` to a convenient key, e.g. C-x p j
;;
;; Inspiration from: consult, consult-project-extra.

;;; Code:
(require 'consult)
(require 'vc-annotate)
(require 'seq)

(defgroup consult-jump-project nil
  "Setup for consult jump project."
  :group 'convenience
  :prefix "consult-jump-project")

(defcustom consult-jump-direct-jump-modes nil
  "Commands from which consult-jump-project should directly jump to dired buffer."
  :type '(list symbol))

(defvar consult-jump-project--projects
  `(:narrow (?p . "Projects")
    :hidden nil
    :category project
    :face consult-file
    :state ,#'consult--file-state
    :enabled  ,(lambda () consult-project-function)
    :annotate ,#'consult-jump-project--annotate
    :action ,#'consult-jump-project--action
    :items ,#'consult-jump-project--projects)
  "Consult-buffer source for project roots.")

(defvar consult-jump-project--age-spec
  `((?Y "year"   "years"   ,(round (* 60 60 24 365.2425)))
    (?M "month"  "months"  ,(round (* 60 60 24 30.436875)))
    (?w "week"   "weeks"   ,(* 60 60 24 7))
    (?d "day"    "days"    ,(* 60 60 24))
    (?h "hour"   "hours"   ,(* 60 60))
    (?m "minute" "minutes" 60)
    (?s "second" "seconds" 1))
  "Age specification. See `magit--age-spec', which this duplicates.")

(defun consult-jump-project--action (cand)
  "Consult buffer action."
  (when (and cand
	     consult-jump-project--original-buffer
	     (with-current-buffer
		 consult-jump-project--original-buffer
	       (cl-notany
		(lambda (x) (derived-mode-p x))
		consult-jump-direct-jump-modes)))
    (consult-jump-project)))

(defun consult-jump-project--details (root)
  "Return details for the given project ROOT.
The returned list contains:
  (root
   #buffers
   #recent files
   time (seconds) elapsed since last modification of recent project files)."
  (let* ((ht (consult--buffer-file-hash))
	 (path (expand-file-name root))
	 (bcount (length (consult--buffer-query :directory path)))
	 (fcount 0) (mod nil))
    (seq-doseq (x recentf-list)
      (when (string-prefix-p path x)	; in this project
	(unless mod (setq mod (float-time
			       (file-attribute-modification-time
				(file-attributes x)))))
	(unless (gethash x ht) (cl-incf fcount))))
    (list root bcount fcount (if mod (- (float-time) mod)))))

(defun consult-jump-project--age (age &optional abbreviate)
  "Adapted from `magit--age'."
  (cl-labels ((fn (age spec)
                  (pcase-let ((`(,char ,unit ,units ,weight) (car spec)))
                    (let ((cnt (round (/ age weight 1.0))))
                      (if (or (not (cdr spec))
                              (>= (/ age weight) 1))
                          (list cnt (cond (abbreviate char)
                                          ((= cnt 1) unit)
                                          (t units)))
                        (fn age (cdr spec)))))))
    (fn age consult-jump-project--age-spec)))

(defvar consult-jump-project--details nil)
(defvar consult-jump-project--max-age nil)
(defun consult-jump-project--projects ()
  "Return list of (other) project roots.
The list is sorted by last file mod date among recently saved
files. Save details."
  (when-let ((projects (seq-filter
			(lambda (dir)
			  (not (string-prefix-p
				(expand-file-name dir) default-directory)))
			(project-known-project-roots)))
	     (details (seq-map #'consult-jump-project--details projects)))
    (setq consult-jump-project--max-age
	  (seq-max (delq nil (seq-map (lambda (x) (nth 3 x)) details))))
    (seq-map #'car
	     (setq consult-jump-project--details
		   (sort details (lambda (a b)
				   (or (not (nth 3 b)) (not (nth 3 a))
				       (< (nth 3 a) (nth 3 b)))))))))

(defconst consult-jump-project--oldest
  (vc-annotate-oldest-in-map vc-annotate-color-map))

(defun consult-jump-project--annotate (root)
  (pcase-let ((`(_ ,bcount ,fcount ,age)
	       (assoc root consult-jump-project--details)))
    (format "%2s recent file%s %2s buffer%s%s"
	    (propertize (format "%d" fcount) 'face 'consult-key)
	    (if (= fcount 1) " " "s")
	    (propertize (format "%d" bcount) 'face 'consult-key)
	    (if (= bcount 1) " " "s")
	    (if age
		(pcase-let* ((`(,cnt ,unit) (consult-jump-project--age age))
			     (ago (format "[%d %s]" cnt unit))
			     (color (or (cdr (vc-annotate-compcar
					      (* (/ age consult-jump-project--max-age)
						 consult-jump-project--oldest)
					      vc-annotate-color-map))
					vc-annotate-very-old-color)))
		  (format " %12s"
			  (propertize ago 'face
				      `(:foreground ,color
						    :background ,vc-annotate-background))))
	      ""))))

(defvar consult-jump-project--original-buffer nil)

;;;###autoload
(defun consult-jump-project (&optional arg)
  "Jump between projects, project files, and project buffers with consult.
Essentially consult-buffer's project buffers & files, plus an
always-present list of projects with age and buffer/file count.
Call with a prefix argument to disable display of project files
and buffers, and display only (other) projects."
  (interactive "P")
  (let ((consult-jump-project--original-buffer (current-buffer)))
    (consult-buffer
     `(,@(unless arg consult-project-buffer-sources)
       (:name ,(concat (if (consult--project-root) "Other ") "Projects")
	      ,@consult-jump-project--projects)))))

(provide 'consult-jump-project)

;;; consult-jump-project.el ends here
