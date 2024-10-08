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
(require 'workline-mode)

(defun workline--jobid (job-id)
  "Extract job id as integer from JOB-ID."
  (if (string-match "gid://gitlab/Ci::\\(Build\\|Bridge\\|Pipeline\\|JobArtifact\\)/\\([0-9]+\\)" job-id)
      (match-string 2 job-id)))

(defun workline-gitlab-job-artifacts (value)
  (seq-filter (lambda (elt) (not (string= (cdr (assoc 'fileType elt)) "TRACE"))) value))

(defun workline-retry-job-at-point-gitlab (repo value)
  "Retry job at point using REPO and VALUE."
  (workline-post-job-at-point-gitlab repo value "retry"))

(defun workline-cancel-job-at-point-gitlab (repo value)
  "Cancel job at point using REPO and VALUE."
  (workline-post-job-at-point-gitlab repo value "cancel"))

(defun workline-delete-pipeline-at-point-gitlab (repo value)
  "Cancel pipeline at point using REPO and VALUE."
  (let ((owner (oref repo owner))
	(name (oref repo name))
	(apihost (oref repo apihost)))
    (glab-request "DELETE"
     (format "projects/%s/pipeline/%s" (url-hexify-string (format "%s/%s" owner name)) (cdr (assoc 'job-id value))) nil
     :host apihost :auth 'workline-mode)))

(defun workline-delete-at-point-gitlab (repo value)
  "Cancel job at point using REPO and VALUE."
  (cond
   ((magit-section-match 'job-id) (workline-post-job-at-point-gitlab repo value "erase")
    (magit-section-match 'main-id) (workline-delete-pipeline-at-point-gitlab repo value))))

(defun workline-post-job-at-point-gitlab (repo value command)
  "Post job COMMAND at current point using REPO and VALUE."
  (let ((owner (oref repo owner))
	(name (oref repo name))
	(apihost (oref repo apihost)))
    (glab-post
     (format "projects/%s/jobs/%s/%s" (url-hexify-string (format "%s/%s" owner name)) (cdr (assoc 'job-id value)) command) nil
     :host apihost :auth 'workline-mode)))

(defun workline-environment-variables ()
  "Get workline environment variables from workline transient arg."
  (if-let (env (transient-arg-value "--env=" (transient-args 'workline-gitlab)))
      (let ((variables
	     (mapcar (lambda (arg)
		       (if-let ((env (split-string arg "=")))
			   (let ((key (cons "key" (car env)))
				 (value (cons "value" (car (cdr env)))))
			     (list key value))))
		     (split-string env "\\,"))))
	(if variables
	    (list (cons "variables" variables))))))

(defun workline-gitlab-trigger-pipeline (repo ref)
  "Workline trigger gitlab pipeline using REPO and REF."
  (let ((owner (oref repo owner))
	(name (oref repo name))
	(apihost (oref repo apihost)))
    (glab-post (format "projects/%s/pipeline?ref=%s" (url-hexify-string (format "%s/%s" owner name)) ref) nil
	       :host apihost
	       :auth 'workline-mode
	       :payload (workline-environment-variables)
	       :callback (lambda (value _headers _status _req))
	       )))

(defun workline-get-ref (pipeline)
  (cdr (assoc 'ref pipeline)))


(defun workline-gitlab-section-jobs (ref main-id main-status jobs indent)
  (magit-insert-section (main-id (list (cons 'job-id main-id) t))
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
	      (full-path (cdr (assoc 'fullPath (cdr (assoc 'project job)))))
	      (downstream-pipeline (cdr (assoc 'downstreamPipeline job)))
	      (artifacts (cdr (assoc 'nodes (cdr (assoc 'artifacts job))))))
	  (if-let ((downstream-id (cdr (assoc 'id downstream-pipeline)))
		   (downstream-status (cdr (assoc 'status downstream-pipeline)))
		   (downstream-jobs (cdr (assoc 'nodes (cdr (assoc 'jobs downstream-pipeline))))))
	      (workline-gitlab-section-jobs
	       (format "downstream::%s" job-name)
	       (workline--jobid downstream-id) downstream-status downstream-jobs "   ")
	    (magit-insert-section (job-id (list (cons 'job-id job-id) (cons 'full-path full-path) (cons 'artifacts artifacts) t))
	      (magit-insert-heading
		(format "%s  %s %s" indent (propertize job-id 'font-lock-face 'workline-grey)
			(workline-format-status (format "[%-7s] %s"  status job-name) status))
		)
	      (if (transient-arg-value "--artifacts" (transient-args 'workline-gitlab))
		  (magit-insert-section-body
		    (seq-doseq (artifact (workline-gitlab-job-artifacts artifacts))
		      (magit-insert-section (artifact artifact t)
			(magit-insert-heading
			  (propertize (format "  %s   artifact: %s" indent (cdr (assoc 'name artifact))) 'font-lock-face 'magit-section-secondary-heading))
			)))))))))))

(defun workline-gitlab-section-pipeline (pipelines indent)
  (seq-doseq (pipeline (sort pipelines :key 'workline-get-ref))
    (let ((ref (workline-get-ref pipeline))
	  (main-id (workline--jobid (cdr (assoc 'id pipeline))))
	  (main-status (cdr (assoc 'status pipeline)))
	  (jobs (cdr (assoc 'nodes (cdr (assoc 'jobs pipeline))))))
      (workline-gitlab-section-jobs ref main-id main-status jobs indent))))

(defun workline-gitlab-section (repo sha &optional bref ignore-sha username first last)
  ""
  (let ((host (oref repo githost))
	(apihost (oref repo apihost))
	(project-id (format "%s/%s" (oref repo owner) (oref repo name))))
    (with-current-buffer (get-buffer-create (format "*Pipeline:%s:/projects/%s/pipelines?sha=%s" host project-id sha))
      (workline-mode)
      (let ((inhibit-read-only t)
	    (project (cdr (assoc 'project (cdr (assoc 'data
						      (if sha
							  (workline-pipelines-from-sha apihost project-id sha bref ignore-sha username first last))))))))
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

(defun workline-job-trace-artifact-at-point-gitlab (repo artifact)
  "Workline get artifacts for job at point using REPO and ARTIFACT."
  (let ((file-path (format ".cache/artifacts/%s" (workline--jobid (cdr (assoc 'id artifact)))))
	(name (cdr (assoc 'name artifact)))
	(download-path (cdr (assoc 'downloadPath artifact))))
    (if (not (file-exists-p (format "%s/%s" file-path name)))
	(progn
	  (make-directory file-path t)
	  (url-copy-file (format "https://%s/%s" (oref repo githost) download-path) (format "%s/%s" file-path name) t)))
    (find-file (format "%s/%s" file-path name))
    (if (not (string= (cdr (assoc 'fileType artifact)) "ARCHIVE"))
	(view-mode))))

(defun workline-job-web-trace-at-point-gitlab (repo value)
  "Workline job web trace at point using REPO and VALUE."
  (if (magit-section-match 'artifact)
      (workline-job-trace-artifact-at-point-gitlab repo value)
    (if-let ((job-id (cdr (assoc 'job-id value)))
	     (host (oref repo githost))
	     (owner (oref repo owner))
	     (name (oref repo name)))
	(cond ((magit-section-match 'job-id) (browse-url (format "https://%s/%s/%s/-/jobs/%s" host owner name job-id)))
	      ((magit-section-match 'main-id) (browse-url (format "https://%s/%s/%s/-/pipelines/%s" host owner name job-id)))
	      ))))

;;;###autoload
(defun workline-job-trace-at-point-gitlab (repo value)
  "Workline job trace at point using REPO and VALUE."
  ;; Get download-path to determine if job have TRACE. It is not used to download the
  ;; actual trace as it is faster to use the API.
  (if (magit-section-match 'artifact)
      (workline-job-trace-artifact-at-point-gitlab repo value)
    (if (magit-section-match 'job-id)
	(let* ((job-id (cdr (assoc 'job-id value)))
	       (workline-buffer (format "*Pipeline:%s:%s" (oref repo githost) job-id))
	       (full-path (cdr (assoc 'full-path value))))
	  (ignore-errors (kill-buffer workline-buffer))
	  (with-current-buffer (get-buffer-create workline-buffer)
	    (erase-buffer)
	    (insert (glab-get
		     (format "projects/%s/jobs/%s/trace" (url-hexify-string full-path) job-id)
		     nil
		     :host (oref repo apihost)
		     :reader 'ghub--decode-payload
		     :auth 'workline-mode
		     ))
	    (goto-char (point-min))
	    (while (re-search-forward "" nil t)
              (replace-match "\n" nil nil))
	    (ansi-color-apply-on-region (point-min) (point-max))
	    (switch-to-buffer (current-buffer))
	    (view-mode))))))

(defun workline-pipeline-args (sha ref no-sha username first last)
  "Provide pipelines (sparql) arguments.

Limit to provided SHA, if not NO-SHA is given, and REF if defined"
  (let ((args (vector)))
    (if (and (not (null sha)) (null no-sha))
	(setq args (vconcat args (vector '(sha $sha String!)))))
    (if (not (null ref))
	(setq args (vconcat args (vector '(ref $ref String!)))))
    (if (not (null username))
	(setq args (vconcat args (vector '(username $username String!)))))
    (if (not (null first))
	(setq args (vconcat args (vector '(first $first Int!)))))
    (if (not (null last))
	(setq args (vconcat args (vector '(last $last Int!)))))
    (if (not (eq args (vector)))
	(cons args ()))))

(defun workline-pipelines-from-sha (host projectid &optional sha ref no-sha username first last)
  "Get Gitlab pipelines from sha."
  (glab-graphql
   `(query
     (project [(fullPath $projectid ID!)]
	      (name)
	      (pipelines
	       ,@(workline-pipeline-args sha ref no-sha username first last)
	       (nodes
		(id)
		(ref)
		(status)
		(jobs
		 (nodes
		  (id)
		  (status)
		  (name)
		  (project
		   (fullPath)
		   )
		  (artifacts
		   (nodes
		    (name)
		    (downloadPath)
		    (fileType)
		    (id)
		    ))
		  (downstreamPipeline
		   (id)
		   (status)
		   (jobs
		    (nodes
		     (id)
		     (name)
		     (status)
		     (project
		      (fullPath)
		      )
		     (artifacts
		      (nodes
		       (name)
		       (downloadPath)
		       (fileType)
		       (id)
		       )
		      )
		     )))
		  ))))))
   `((projectid . ,projectid)
     (sha . ,sha)
     (ref . ,ref)
     (username . ,username)
     (first . ,first)
     (last . ,last)
     )
   :host host
   :auth 'workline-mode
   ))

(provide 'workline-gitlab)

;;; workline-gitlab.el ends here
