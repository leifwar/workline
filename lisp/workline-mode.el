;;; workline-mode.el -- Base mode for Workline.   -*- lexical-binding: t; -*-

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
(require 'ansi-color)

(require 'workline-core)
(require 'workline-github)
(require 'workline-gitlab)

(defun workline-show-refresh ()
  "Refresh pipeline."
  (interactive)
  (workline-section-apply
   (list (cons 'gitlab 'workline-gitlab-section)
	 (cons 'github 'workline-github-section))))


(defun workline-job-trace-at-point ()
  "Trace job at point."
  (interactive)
  (workline-section-fun-at-point
   (list (cons 'gitlab 'workline-job-trace-at-point-gitlab)
	 (cons 'github 'workline-job-trace-at-point-github))))

(defun workline-retry-job-at-point ()
  "Retry job at point."
  (interactive)
  (workline-section-fun-at-point
   (list (cons 'gitlab 'workline-retry-job-at-point-gitlab)
	 (cons 'github 'workline-retry-job-at-point-github))))

(defun workline-cancel-job-at-point ()
  "Cancel job at point."
  (interactive)
  (workline-section-fun-at-point
   (list (cons 'gitlab 'workline-cancel-job-at-point-gitlab)
	 (cons 'github 'workline-cancel-job-at-point-github))))


(defun workline-branch-option (&optional force-branch-option)
  "Return branch point if it exists.

Current branch only if optional FORCE-BRANCH-OPTION is given."
  (if-let ((remote-branch (magit-remote-branch-at-point)))
      (if (or force-branch-option
	      (transient-arg-value "--branch" (transient-args 'workline)))
	  (string-join (cdr (split-string remote-branch "/")) "/"))))

(defun workline-show-sha ()
  "Workline show sha at point (support magit buffer)."
  (interactive)
  (if-let ((repo (forge-get-repository :valid?)))
      (if (forge-gitlab-repository--eieio-childp repo)
	  (workline-gitlab-section
	   repo
	   (magit-rev-parse (magit-commit-at-point))
	   (workline-branch-option))
	(workline-github-section repo (workline-branch-option t)))))

(defun workline-trigger-pipeline ()
  "Workline trigger pipeline."
  (interactive)
  (if-let ((repo (forge-get-repository :valid?)))
      (if (forge-gitlab-repository--eieio-childp repo)
	  (workline-gitlab-trigger-pipeline
	   repo
	   (workline-branch-option t))
	(message "only available for gitlab repos"))))

;;; Key Bindings
(defvar-keymap workline-mode-map
  :doc "Keymap for pipeline"
  :parent magit-section-mode-map
  "RET" 'workline-job-trace-at-point
  "v" 'workline-job-trace-at-point
  "R" 'workline-show-refresh
  "r" 'workline-retry-job-at-point
  "c" 'workline-cancel-job-at-point
  )

(define-derived-mode workline-mode magit-mode "Pipeline"
  "Base mode for Pipelines."
  :group 'workline
)

(transient-define-prefix workline ()
  ["Arguments"
   ("a" "Show all branches (with history)" "--all")
   ("e" "Environment variables (for a trigger)" "--env=")
   ("b" "Current branch only" "--branch")
   ]
  ["Actions"
   ("r" "Get pipeline(s) for current SHA" workline-show-sha)
   ("t" "Trigger a pipeline" workline-trigger-pipeline)
   ]
  )

;forge-gitlab-repository--eieio-childp
(provide 'workline-mode)

;;; workline-mode.el ends here
