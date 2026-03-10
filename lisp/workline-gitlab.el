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

(require 'magit-mode)
(require 'workline-mode)

(defun workline--jobid (job-id)
  "Extract job id as integer from JOB-ID."
  (if (string-match "gid://gitlab/.*/\\([0-9]+\\)$" job-id)
      (match-string 1 job-id)))

(defun workline-gitlab-job-artifacts (value)
  (seq-filter (lambda (elt) (not (string= (cdr (assoc 'fileType elt)) "TRACE"))) value))

(defun workline-job-at-point-gitlab (repo value command)
  (cond
   ((magit-section-match 'job-id)
    (workline-post-at-point-gitlab repo value command "jobs"))
   ((magit-section-match 'main-id)
    (workline-post-at-point-gitlab repo value command "pipelines"))))

(defun workline-retry-job-at-point-gitlab (repo value)
  "Retry job at point using REPO and VALUE."
  (workline-job-at-point-gitlab repo value "retry"))

(defun workline-cancel-job-at-point-gitlab (repo value)
  "Cancel job at point using REPO and VALUE."
  (workline-job-at-point-gitlab repo value "cancel"))

(defun workline-delete-pipeline-at-point-gitlab (repo value)
  "Cancel pipeline at point using REPO and VALUE."
  (let ((apihost (oref repo apihost)))
    (ghub-request
     "DELETE"
     (format "projects/%s/pipelines/%s"
             (url-hexify-string (cdr (assoc 'full-path value)))
             (cdr (assoc 'job-id value)))
     nil
     :forge 'gitlab
     :host apihost
     :auth 'workline-mode
     :callback (lambda (_value _headers _status _req)))))

(defun workline-delete-at-point-gitlab (repo value)
  "Cancel job at point using REPO and VALUE."
  (cond
   ((magit-section-match 'job-id)
    (workline-post-at-point-gitlab repo value "erase" "jobs"))
   ((magit-section-match 'main-id)
    (workline-delete-pipeline-at-point-gitlab repo value))))

(defun workline-post-at-point-gitlab (repo value command command_type)
  "Post COMMAND_TYPE COMMAND at current point using REPO and VALUE."
  (let ((apihost (oref repo apihost)))
    (ghub-request
     "POST"
     (format "projects/%s/%s/%s/%s"
             (url-hexify-string (cdr (assoc 'full-path value)))
             command_type
             (cdr (assoc 'job-id value))
             command)
     nil
     :forge 'gitlab
     :host apihost
     :auth 'workline-mode)))

(defun workline-environment-variables ()
  "Get workline environment variables from workline transient arg."
  (if-let (env
           (transient-arg-value "--env=" (transient-args 'workline-gitlab)))
    (let ((variables
           (mapcar
            (lambda (arg)
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
    (ghub-request
     "POST"
     (format "projects/%s/pipeline?ref=%s" (url-hexify-string (format "%s/%s" owner name)) ref)
     nil
     :forge 'gitlab
     :host apihost
     :auth 'workline-mode
     :payload (workline-environment-variables)
     :callback (lambda (_value _headers _status _req)))))

(defun workline-get-ref (pipeline)
  (cdr (assoc 'ref pipeline)))

(defun workline-gitlab-check-warning (status allow-failure)
  "Change STATUS to WARNING if FAILED and ALLOW-FAILURE."
  (if (and allow-failure (string= status "FAILED"))
      "WARNING"
    status))

(defun workline-gitlab-section-jobs (ref main-id main-status full-path stages indent)
  (magit-insert-section
   (main-id
    (list (cons 'job-id main-id) (cons 'full-path full-path) t)
    (string-equal main-status "SUCCESS"))
   (magit-insert-heading
    (format "%s %s %s %s"
            indent
            (propertize ref 'font-lock-face 'magit-section-heading)
            (propertize main-id 'font-lock-face 'workline-grey)
            (workline-format-status (downcase main-status) main-status)))
   (magit-insert-section-body
    (seq-doseq (stage stages)
      (let ((stage-id (cdr (assoc 'id stage)))
            (stage-name (cdr (assoc 'name stage)))
            (stage-status (cdr (assoc 'status stage))))
        (magit-insert-section
         (stage-id
          (list (cons 'stage-id stage-id) (cons 'full-path full-path) t)
          (string-equal stage-status "success"))
         (magit-insert-heading
          (format " %s %s %s"
                  indent
                  (propertize stage-name 'font-lock-face 'workline-stage)
                  (workline-format-status stage-status (upcase stage-status))))
         (magit-insert-section-body
          (seq-doseq (job (cdr (assoc 'nodes (cdr (assoc 'jobs stage)))))
            (let ((status
                   (workline-gitlab-check-warning
                    (cdr (assoc 'status job)) (cdr (assoc 'allowFailure job))))
                  (job-id (workline--jobid (cdr (assoc 'id job))))
                  (job-name (cdr (assoc 'name job)))
                  (downstream-pipeline (cdr (assoc 'downstreamPipeline job)))
                  (artifacts (cdr (assoc 'nodes (cdr (assoc 'artifacts job))))))
              (if-let ((downstream-id (cdr (assoc 'id downstream-pipeline)))
                       (downstream-status
                        (workline-gitlab-check-warning
                         (cdr (assoc 'status downstream-pipeline))
                         (cdr (assoc 'allowFailure downstream-pipeline))))
                       (downstream-full-path
                        (cdr (assoc 'fullPath (cdr (assoc 'project downstream-pipeline)))))
                       (downstream-stages
                        (cdr (assoc 'nodes (cdr (assoc 'stages downstream-pipeline))))))
                (workline-gitlab-section-jobs
                 (format "downstream::%s" job-name)
                 (workline--jobid downstream-id)
                 downstream-status
                 downstream-full-path
                 downstream-stages
                 "   ")
                (magit-insert-section
                 (job-id
                  (list
                   (cons 'job-id job-id) (cons 'full-path full-path) (cons 'artifacts artifacts) t))
                 (magit-insert-heading
                  (format "%s   %s %s"
                          indent
                          (propertize job-id 'font-lock-face 'workline-grey)
                          (workline-format-status (format "[%-7s] %s" status job-name) status)))
                 (if (transient-arg-value "--artifacts" (transient-args 'workline-gitlab))
                     (magit-insert-section-body
                      (seq-doseq (artifact (workline-gitlab-job-artifacts artifacts))
                        (magit-insert-section
                         (artifact artifact t)
                         (magit-insert-heading
                          (propertize
                           (format "  %s   artifact: %s" indent (cdr (assoc 'name artifact)))
                           'font-lock-face 'magit-section-secondary-heading)))))))))))))))))


(defun workline-gitlab-section-pipeline (full-path pipelines indent)
  (seq-doseq (pipeline (sort pipelines :key 'workline-get-ref))
    (let ((ref (workline-get-ref pipeline))
          (main-id (workline--jobid (cdr (assoc 'id pipeline))))
          (main-status (cdr (assoc 'status pipeline)))
          (stages (cdr (assoc 'nodes (cdr (assoc 'stages pipeline))))))
      (workline-gitlab-section-jobs ref main-id main-status full-path stages indent))))

(defun workline-gitlab-section (repo sha &optional bref ignore-sha username first last)
  ""
  (let ((host (oref repo githost))
        (apihost (oref repo apihost))
        (project-id (format "%s/%s" (oref repo owner) (oref repo name))))
    (with-current-buffer (get-buffer-create
                          (format "*Pipeline:%s:/projects/%s/pipelines?sha=%s" host project-id sha))
      (workline-mode)
      (let ((inhibit-read-only t)
            (project
             (cdr
              (assoc
               'project
               (cdr
                (assoc
                 'data
                 (if sha
                     (workline-pipelines-from-sha apihost project-id
                                                  sha
                                                  bref
                                                  ignore-sha
                                                  username
                                                  first
                                                  last))))))))
        (erase-buffer)
        (let ((pipelines (cdr (assoc 'nodes (cdr (assoc 'pipelines project)))))
              (full-path (cdr (assoc 'fullPath project))))
          (if pipelines
              (magit-insert-section
               (project (list repo sha bref ignore-sha username first last) t)
               (magit-insert-heading (format "%s-pipeline" (cdr (assoc 'name project))))
               (magit-insert-section-body
                (workline-gitlab-section-pipeline full-path pipelines " "))
               (pop-to-buffer (current-buffer)))
            (progn
              (kill-buffer (current-buffer))
              (if sha
                  (workline-gitlab-section repo nil))))))
      (let ((magit-section-cache-visibility nil))
        (magit-section-show magit-root-section)))))

(defun workline-job-trace-artifact-at-point-gitlab (repo artifact)
  "Workline get artifacts for job at point using REPO and ARTIFACT."
  (let ((file-path (format ".cache/artifacts/%s" (workline--jobid (cdr (assoc 'id artifact)))))
        (name (cdr (assoc 'name artifact)))
        (download-path (cdr (assoc 'downloadPath artifact))))
    (unless (file-exists-p (format "%s/%s" file-path name))
      (make-directory file-path t)
      (url-copy-file
       (format "https://%s/%s" (oref repo githost) download-path)
       (format "%s/%s" file-path name)
       t))
    (find-file (format "%s/%s" file-path name))
    (if (not (string= (cdr (assoc 'fileType artifact)) "ARCHIVE"))
        (view-mode))))

(defun workline-job-web-trace-at-point-gitlab (repo value)
  "Workline job web trace at point using REPO and VALUE."
  (if (magit-section-match 'artifact)
      (workline-job-trace-artifact-at-point-gitlab repo value)
    (if-let ((job-id (cdr (assoc 'job-id value)))
             (full-path (cdr (assoc 'full-path value)))
             (host (oref repo githost)))
      (cond
       ((magit-section-match 'job-id)
        (browse-url (format "https://%s/%s/-/jobs/%s" host full-path job-id)))
       ((magit-section-match 'main-id)
        (browse-url (format "https://%s/%s/-/pipelines/%s" host full-path job-id)))))))

(defun workline-build-trace-buffer (workline-buffer host path)
  ""
  (ignore-errors
    (kill-buffer workline-buffer))
  (with-current-buffer (get-buffer-create workline-buffer)
    (erase-buffer)
    (insert
     (ghub-request
      "GET"
      path
      nil
      :forge 'gitlab
      :host host
      :reader 'ghub--decode-payload
      :auth 'workline-mode))
    (goto-char (point-min))
    (while (re-search-forward "" nil t)
      (replace-match "\n" nil nil))
    (ansi-color-apply-on-region (point-min) (point-max))
    (switch-to-buffer (current-buffer))
    (view-mode)
    (goto-char (point-max))
    (local-set-key
     (kbd "R")
     (lambda ()
       (interactive)
       (workline-build-trace-buffer workline-buffer host path)))))

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
          (workline-build-trace-buffer
           workline-buffer
           (oref repo apihost)
           (format "projects/%s/jobs/%s/trace" (url-hexify-string full-path) job-id))))))

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
  (ghub-graphql
   `(query
     (project
      [(fullPath $projectid ID!)] (name) (fullPath)
      (pipelines
       ,@ (workline-pipeline-args sha ref no-sha username first last)
       (nodes
        (id) (ref) (status)
        (stages
         (nodes
          (name) (id) (status)
          (jobs
           (nodes
            (id)
            (status)
            (allowFailure)
            (name)
            (artifacts (nodes (name) (downloadPath) (fileType) (id)))
            (downstreamPipeline
             (project (fullPath)) (id) (status)
             (stages
              (nodes
               (name) (id) (status) (jobs (nodes (id) (name) (status) (allowFailure))))))))))))))
   `((projectid . ,projectid)
     (sha . ,sha)
     (ref . ,ref)
     (username . ,username)
     (first . ,first)
     (last . ,last))
   :host host
   :auth 'workline-mode
   :forge 'gitlab))

(provide 'workline-gitlab)

;;; workline-gitlab.el ends here
