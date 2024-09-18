;;; workline-gitlab.el -- Base mode for GitLab Workflow.   -*- lexical-binding: t; -*-

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

(require 'glab)
(require 'magit-mode)

(defun workline--jobid (job-id)
  "Extract job id as integer from JOB-ID."
  (if (string-match "gid://gitlab/Ci::\\(Build\\|Bridge\\|Pipeline\\)/\\([0-9]+\\)" job-id)
      (match-string 2 job-id)))

(defun workline-retry-job-at-point-gitlab (repo job-id)
  "Retry job at point with REPO and JOB-ID."
  (workline-post-job-at-point-gitlab repo job-id "retry"))

(defun workline-cancel-job-at-point-gitlab (repo job-id)
  "Cancel job at point with REPO and JOB-ID."
  (workline-post-job-at-point-gitlab repo job-id "cancel"))

(defun workline-post-job-at-point-gitlab (repo job-id command)
  ""
  (let ((owner (oref repo owner))
	(name (oref repo name)))
    (glab-post (format "projects/%s/jobs/%s/%s" (url-hexify-string (format "%s/%s" owner name)) job-id command) nil
	       :host (oref repo apihost))))

(defun workline-get-ref (pipeline)
  (cdr (assoc 'ref pipeline)))

(defun workline-gitlab-section-jobs-debug (ref main-id main-status jobs indent)
  (seq-doseq (job jobs)
    (let ((status (cdr (assoc 'status job)))
	  (job-id (workline--jobid (cdr (assoc 'id job))))
	  (job-name (cdr (assoc 'name job)))
	  (downstream-pipeline (cdr (assoc 'downstreamPipeline job))))
      (if-let ((downstream-id (cdr (assoc 'id downstream-pipeline)))
	       (downstream-status (cdr (assoc 'status downstream-pipeline)))
	       (downstream-jobs (cdr (assoc 'jobs downstream-pipeline))))
	  (workline-gitlab-section-jobs-debug
	   (format "downstream %s" job-name)
	   (workline--jobid downstream-id) downstream-status downstream-jobs "   ")
	(message "hei")
	)
      )
    )
  )


(defun workline-gitlab-section-jobs (ref main-id main-status jobs indent)
  (magit-insert-section (branch nil)
    (magit-insert-heading (format "%s %s %s %s"
				  indent
				  (propertize ref 'font-lock-face 'magit-section-heading)
				  (propertize main-id 'font-lock-face 'workline-grey)
				  (workline-format-status main-status main-status)))
    (magit-insert-section-body
      (seq-doseq (job jobs)
	(let ((status (cdr (assoc 'status job)))
	      (job-id (workline--jobid (cdr (assoc 'id job))))
	      (job-name (cdr (assoc 'name job)))
	      (downstream-pipeline (cdr (assoc 'downstreamPipeline job))))
	  (if-let ((downstream-id (cdr (assoc 'id downstream-pipeline)))
		   (downstream-status (cdr (assoc 'status downstream-pipeline)))
		   (downstream-jobs (cdr (assoc 'nodes (cdr (assoc 'jobs downstream-pipeline))))))
	      (workline-gitlab-section-jobs
	       (format "downstream::%s" job-name)
	       (workline--jobid downstream-id) downstream-status downstream-jobs "   ")
	    (magit-insert-section (job-id job-id t)
	      (magit-insert-heading
		(format "%s  %s %s" indent (propertize job-id 'font-lock-face 'workline-grey)
			(workline-format-status (format "[%-7s] %s"  status job-name) status))
		)))))))
  )

(defun workline-gitlab-section-pipeline (pipelines indent)
  (seq-doseq (pipeline (sort pipelines :key 'workline-get-ref))
    (let ((ref (workline-get-ref pipeline))
	  (main-id (workline--jobid (cdr (assoc 'id pipeline))))
	  (main-status (cdr (assoc 'status pipeline)))
	  (jobs (cdr (assoc 'nodes (cdr (assoc 'jobs pipeline))))))
      (workline-gitlab-section-jobs ref main-id main-status jobs indent))))

(defun workline-gitlab-section (repo sha &optional bref)
  ""
  (let ((host (oref repo githost))
	(apihost (oref repo apihost))
	(project-id (format "%s/%s" (oref repo owner) (oref repo name))))
    (with-current-buffer (get-buffer-create (format "*Pipeline:%s:/projects/%s/pipelines?sha=%s" host project-id sha))
      (workline-mode)
      (let ((inhibit-read-only t)
	    (project (cdr (assoc 'project (cdr (assoc 'data
						      (if sha
							  (workline-piplines-from-sha apihost project-id sha bref)
							(workline-piplines-all apihost project-id))))))))
	(erase-buffer)
	(let ((pipelines (cdr (assoc 'nodes (cdr (assoc 'pipelines project))))))
	  (if pipelines
	      (magit-insert-section (project (list repo sha) t)
		(magit-insert-heading (format "%s-pipeline" (cdr (assoc 'name project))))
		(magit-insert-section-body
		  (workline-gitlab-section-pipeline pipelines " "))
		(pop-to-buffer (current-buffer)))
	    (progn
	      (kill-buffer (current-buffer))
	      (if sha
		  (workline-gitlab-section repo nil)))))))))

;;;###autoload
(defun workline-job-trace-at-point-gitlab (repo job-id)
  "Pipeline job trace at point."
  (let ((workline-buffer (format "*Pipeline:%s:%s" (oref repo githost) job-id))
	(owner (oref repo owner))
	(name (oref repo name)))
    (ignore-errors (kill-buffer workline-buffer))
    (with-current-buffer (get-buffer-create workline-buffer)
      (erase-buffer)
      (insert (glab-get
	       (format "projects/%s/jobs/%s/trace" (url-hexify-string (format "%s/%s" owner name)) job-id)
	       nil :host (oref repo apihost) :reader 'ghub--decode-payload))
      (goto-char (point-min))
      (while (re-search-forward "" nil t)
        (replace-match "\n" nil nil))
      (ansi-color-apply-on-region (point-min) (point-max))
      (switch-to-buffer (current-buffer))
      (view-mode))))

;;;###autoload
(defun workline-piplines-all (host projectid)
  "Get pipelines from HOST (Gitlab) using PROJECTID."
  (glab-graphql
   `(query
     (project [(fullPath $projectid ID!)]
	    (name)
	    (pipelines
	     (nodes
	      (id)
	      (ref)
	      (status)
	      (jobs
	       (nodes
		(id)
		(status)
		(name)
		(downstreamPipeline
		 (id)
		 (status)
		 (jobs
		  (nodes
		   (id)
		   (status)
		   (name)
		   )))
		))))))
   `((projectid . ,projectid))
   :auth 'forge
   :host host))

(defun workline-piplines-from-sha (host projectid sha &optional ref)
  "Get Gitlab pipelines from sha"
  (glab-graphql
   (if (not (null ref))
       `(query
	 (project [(fullPath $projectid ID!)]
		  (name)
		  (pipelines
		   [(sha $sha String!) (ref $ref String!)]
		   (nodes
		    (id)
		    (ref)
		    (status)
		    (jobs
		     (nodes
		      (id)
		      (status)
		      (name)
		      (downstreamPipeline
		       (id)
		       (status)
		       (jobs
			(nodes
			 (id)
			 (name)
			 (status))))
		      ))))))
     `(query
       (project [(fullPath $projectid ID!)]
		(name)
		(pipelines
		 [(sha $sha String!)]
		 (nodes
		  (id)
		  (ref)
		  (status)
		  (jobs
		   (nodes
		    (id)
		    (status)
		    (name)
		    (downstreamPipeline
		     (id)
		     (status)
		     (jobs
		      (nodes
		       (id)
		       (name)
		       (status)
		       )))
		    )))))))
   `((projectid . ,projectid)
     (sha . ,sha)
     (ref . ,ref))
   :auth 'forge
   :host host))

(provide 'workline-gitlab)

;;; workline-gitlab.el ends here
