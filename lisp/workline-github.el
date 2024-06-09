;;; workline-github.el -- Base mode for Github Workflow.   -*- lexical-binding: t; -*-

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

(defun workline-github-nodes (data)
  (cdr (assoc 'nodes (cdr (assoc 'checkSuites (cdr (assoc 'target (cdr (assoc 'ref (cdr (assoc 'repository (cdr (assoc 'data data)))))))))))))

(defun workline-github-section (repo ref)
  "Build workflow section for REPO given REF."
  (let ((host (oref repo apihost))
	(name (oref repo name))
	(owner (oref repo owner)))
    (with-current-buffer (get-buffer-create (format "*Workflow:%s:%s:%s:%s" host owner name ref))
      (workline-mode)
      (let ((inhibit-read-only t)
	    (nodes (workline-github-nodes (workline-workflow-from-ref host owner name ref))))
	(erase-buffer)
	(magit-insert-section (project (list repo ref) t)
	  (magit-insert-heading (format "%s workflows" name))
	  (magit-insert-section-body
	    (seq-doseq (node nodes)
	      (if-let ((conclusion (cdr (assoc 'conclusion node)))
		       (flow_name (cdr (assoc 'name (cdr (assoc 'workflow (cdr (assoc 'workflowRun node)))))))
		       (resource-path (cdr (assoc 'resourcePath (cdr (assoc 'workflowRun node)))))
		       )
		  (magit-insert-section (workline_branch nil)
		    (make-directory (format "logs/%s" resource-path) t)
		    (ghub-get (format "repos%s/logs" resource-path) nil
			      :host (oref repo apihost)
			      :reader 'ghub--decode-payload
			      :callback (lambda (value _headers _status _req)
					  (let ((fname (format "logs/log%s.zip" (replace-regexp-in-string "/" "-" resource-path))))
					    (with-temp-file fname
					      (insert value))
					    (call-process "unzip" nil 0 nil "-d" (format "logs/%s" resource-path) "-u" fname)
					    )))
		    (magit-insert-heading
		      (format "  %s %s"
			      (propertize flow_name 'font-lock-face 'magit-section-heading)
			      (workline-format-status conclusion conclusion)))
		    (magit-insert-section-body
		      (seq-doseq (run (cdr (assoc 'nodes (cdr (assoc 'checkRuns node)))))
			(let ((run-name (cdr (assoc 'name run)))
			      (run-conclusion (cdr (assoc 'conclusion run))))
			  (magit-insert-section (workline_branch (list "0" nil resource-path run-name) t)
			    (magit-insert-heading
			      (format "  - %s %s"
				      (propertize run-name 'font-lock-face 'magit-section-heading)
				      (workline-format-status run-conclusion run-conclusion)))
			    (magit-insert-section-body
			      (seq-doseq (step (cdr (assoc 'nodes (cdr (assoc 'steps run)))))
				(let ((step-name (cdr (assoc 'name step)))
				      (step-conclusion (cdr (assoc 'conclusion step)))
				      (step-number (cdr (assoc 'number step))))
				  (magit-insert-section (step (list step-number step-name resource-path run-name) t)
				    (magit-insert-heading
				      (format "   %s : %s %s"
					      (propertize (format "%2d" step-number) 'font-lock-face 'workline-grey)
					      (workline-format-status run-conclusion step-conclusion)
					      (propertize step-name 'font-lock-face 'magit-section-secondary-heading)
					      ))))))))))))))))
      (pop-to-buffer (current-buffer)))))

(defun workline-github-log-fname (step job-name resource-path run-name)
  ""
  (if job-name
      (format "logs%s/%s/%s_%s.txt"
		     resource-path
		     (replace-regexp-in-string "/" "" run-name)
		     step
		     (replace-regexp-in-string "/" "" job-name))
    (format "logs%s/%s_%s.txt"
		   resource-path
		   step
		   (replace-regexp-in-string "/" "" run-name))))

(defun workline-job-trace-at-point-github (step job-name resource-path run-name)
  "Workflow job trace at point."
  (with-current-buffer (get-buffer-create (format "*Workflow:%s" resource-path))
    (erase-buffer)
    (insert-file-contents (workline-github-log-fname step job-name resource-path run-name))
    (goto-char (point-min))
    (while (re-search-forward "" nil t)
      (replace-match "\n" nil nil))
    (ansi-color-apply-on-region (point-min) (point-max))
    (switch-to-buffer (current-buffer))
    (view-mode)))

(defun workline-workflow-from-ref (host owner name ref)
  "Get Github workflows from REF"
  (ghub-graphql
   `(query
     (repository
      [(owner $owner String!) (name $name String!)]
      (ref
       [(qualifiedName $ref String!)]
       (target
	(\...\ on\ Commit
	 (oid)
	 (checkSuites
	  [(last 6)]
	  (nodes
	   (conclusion)
	   (workflowRun
	    (resourcePath)
	    (workflow
	     (name)))
	   (checkRuns
	    [(last 100) (:filterBy (checkType LATEST CheckType!))]
	    (nodes
	     (detailsUrl)
	     (name)
	     (status)
	     (conclusion)
	     (detailsUrl)
	     (steps
	      [(first 15)]
	      (nodes
	       (name)
	       (conclusion)
	       (number))))))))))))
   `((owner . ,owner)
     (name . ,name)
     (ref . ,ref))
   :auth 'forge
   :host host
   )
  )

(provide 'workline-github)

;;; workline-github.el ends here
