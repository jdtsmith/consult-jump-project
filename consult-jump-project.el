;;; consult-jump-project.el --- Quickly jump between projects files and buffers -*- lexical-binding: t -*-

;; Copyright (C) 2022  J.D. Smith

;; Author: J.D. Smith
;; Maintainer:  J.D. Smith <mail@daniel-mendler.de>
;; Created: 2022-2023
;; Version: 0.1.1
;; Package-Requires: ((emacs "28.1") (consult "0.18"))
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
;; preview is provided in the form of a Dired buffer visiting its
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
(require 'recentf)
(require 'tramp)

(defgroup consult-jump-project nil
  "Setup for consult jump project."
  :group 'convenience
  :prefix "consult-jump-project")

(defcustom consult-jump-project-direct-jump-modes nil
  "Commands from which `consult-jump-project' should directly jump to Dired buffer."
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
  "Age specification.  See `magit--age-spec', which this duplicates.")

(defvar consult-jump-project--original-buffer nil)

(defun consult-jump-project--action (cand)
  "Consult buffer action for candidate CAND."
  (if (and cand
	     consult-jump-project--original-buffer
	     (with-current-buffer
		 consult-jump-project--original-buffer
	       (cl-notany
		(lambda (x) (derived-mode-p x))
		consult-jump-project-direct-jump-modes)))
    (consult-jump-project)))

(defun consult-jump-project--details (root ht)
  "Return details for the given project ROOT and consult hash table HT.
The returned list contains:
  (root
   #buffers
   #recent files
   time (seconds) elapsed since last modification of recent project files)."
  (let* ((path (expand-file-name root))
	 (bcount (length (consult--buffer-query :directory path)))
	 (fcount 0) (mod nil))
    (seq-doseq (x recentf-list)
      (when
	  (string-prefix-p path (expand-file-name x))	; in this project
	(unless mod (setq mod (float-time
			       (file-attribute-modification-time
				(file-attributes x)))))
	(unless (gethash x ht) (cl-incf fcount))))
    (list root bcount fcount (if mod (- (float-time) mod)))))

(defun consult-jump-project--age (age &optional abbreviate)
  "Summarize AGE and possibly ABBREVIATE.
Adapted from `magit--age'."
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
files.  Save details."
  (when-let ((ht (consult--buffer-file-hash))
              (all-active-remote-projects (cl-map 'list (lambda (p)
                                                          (substring-no-properties (tramp-make-tramp-file-name p)))
                                                  (tramp-list-connections)))
	      (projects (seq-filter
		         (lambda (dir)
		           (not (string-prefix-p
			         (if (file-remote-p dir) dir (expand-file-name dir)) default-directory)))
		         (seq-filter (lambda (p)
                                       ;; otherwise tramp will try to connect to remote projects that we might not have connected to yet
                                       (or (not (file-remote-p p))
                                           (cl-find-if (lambda (remote-proj) (string-prefix-p remote-proj p))
                                                       all-active-remote-projects)))
                                     (project-known-project-roots))))
	      (details (seq-map (lambda (x)
                                  (consult-jump-project--details x ht))
                                projects)))
    (setq consult-jump-project--max-age
	  (when-let ((ages (delq nil (seq-map (lambda (x) (nth 3 x)) details))))
	    (seq-max ages)))
    (seq-map #'car
	     (setq consult-jump-project--details
		   (sort details (lambda (a b)
				   (cond ((not (nth 3 a)) nil)
					 ((not (nth 3 b)) t)
					 (t (< (nth 3 a) (nth 3 b))))))))))

(defconst consult-jump-project--oldest
  (vc-annotate-oldest-in-map vc-annotate-color-map))

(defun consult-jump-project--annotate (root)
  "Annotate ROOT project for display."
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

;;;###autoload
(defun consult-jump-project (&optional arg)
  "Jump between projects, project files, and project buffers with consult.
Essentially consult-buffer's project buffers & files, plus an
always-present list of projects with age and buffer/file count.
Call with a prefix argument ARG to disable display of project
files and buffers, and display only (other) projects."
  (interactive "P")
  (let ((consult-jump-project--original-buffer (current-buffer)))
    (consult-buffer
     `(,@(unless arg consult-project-buffer-sources)
       (:name ,(concat (if (consult--project-root) "Other ") "Projects")
	      ,@consult-jump-project--projects)))))

(provide 'consult-jump-project)

;;; consult-jump-project.el ends here
