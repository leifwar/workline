;;; workline-core.el -- Base mode for Workline.   -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Leif Warland <leif.warland@gmail.com>

;; WorkLine is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; WorkLine is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with WorkLine.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

(require 'magit-mode)

(defgroup workline-faces nil
  "Faces used by Pipeline."
  :group 'workline
  :group 'faces)

(defface workline-success
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "sea green")
    (((class color) (background  dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "sea green"))
  "Face for highlighting the current section."
  :group 'workline-faces)

(defface workline-failed
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "tomato")
    (((class color) (background  dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "tomato"))
  "Face for highlighting the current section."
  :group 'workline-faces)

(defface workline-running
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "yellow")
    (((class color) (background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "LightYellow"))
  "Face for highlighting the current section."
  :group 'workline-faces)

(defface workline-grey
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "grey")
    (((class color) (background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "grey"))
  "Face for highlighting the current section."
  :group 'workline-faces)

(defun workline-format-status (string status)
  "Apply colour to STRING given STATUS."
  (propertize string 'font-lock-face (cond ((string= status "SUCCESS") 'workline-success)
					   ((string= status "FAILURE") 'workline-failed)
					   ((string= status "FAILED") 'workline-failed)
					   ((string= status "RUNNING") 'workline-running)
					   (t 'workline-grey))))

(defun workline-section-apply (fun)
  "Apply FUN to current section."
  (let ((repo (car (oref magit-root-section value)))
	(value (oref magit-root-section value)))
    (if (forge-gitlab-repository--eieio-childp repo)
	(apply (cdr (assoc 'gitlab fun)) value)
      (apply (cdr (assoc 'github fun)) value))))


(defun workline-section-fun-at-point (fun)
  "Use FUN to trace section job at point."
  (if (not (magit-section-match 'project))
      (let ((job-id (oref (magit-current-section) value))
	    (repo (car (oref magit-root-section value))))
	(when (and repo job-id)
	  (if (forge-gitlab-repository--eieio-childp repo)
   	      (apply (cdr (assoc 'gitlab fun)) (list repo job-id))
   	    (apply (cdr (assoc 'github fun)) job-id))))))

(provide 'workline-core)

;;; workline-core.el ends here
