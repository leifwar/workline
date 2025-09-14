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
(require 'workline-mode)

(defun workline-github-nodes (data)
  "Extract the check suite results from sparql query DATA."
  (cdr
   (assoc
    'nodes
    (cdr
     (assoc
      'checkSuites (cdr (assoc 'object (cdr (assoc 'repository (cdr (assoc 'data data)))))))))))

(defun workline-github-artifacts (repo run-id)
  "Fetch list of artifacts for REPO and RUN-ID."
  (let ((host (oref repo apihost))
        (name (oref repo name))
        (owner (oref repo owner)))
    (if (transient-arg-value "--artifacts" (transient-args 'workline-github))
        (let ((artifacts
               (ghub-get
                (format "repos/%s/%s/actions/runs/%d/artifacts" owner name run-id) nil
                :host host
                :auth 'workline-mode)))
          (if (>= (cdr (assoc 'total_count artifacts)) 1)
              (magit-insert-section
               (artifacts artifacts t) (magit-insert-heading "  Artifacts")
               (magit-insert-section-body
                (seq-doseq (artifact (cdr (assoc 'artifacts artifacts)))
                  (magit-insert-section
                   (artifact (list artifact nil nil nil repo) t)
                   (magit-insert-heading
                    (propertize (format "    %s" (cdr (assoc 'name artifact)))
                                'font-lock-face
                                (if (cdr (assoc 'expired artifact))
                                    'workline-gey
                                  'magit-section-secondary-heading))))))))))))

(defun workline-github-section (repo sha &optional bref ignore-sha)
  "Build workflow section for REPO given SHA or BREF (ignore sha's from IGNORE-SHA if given)."
  (let ((host (oref repo apihost))
        (name (oref repo name))
        (owner (oref repo owner))
        (ref
         (if (and bref (not ignore-sha))
             bref
           sha)))
    (with-current-buffer (get-buffer-create (format "*Workflow:%s:%s:%s:%s" host owner name ref))
      (workline-mode)
      (let ((inhibit-read-only t)
            (nodes (workline-github-nodes (workline-workflow-from-ref host owner name ref))))
        (erase-buffer)
        (magit-insert-section
         (project (list repo ref) t) (magit-insert-heading (format "%s workflows" name))
         (magit-insert-section-body
          (seq-doseq (node nodes)
            (when-let ((conclusion (cdr (assoc 'conclusion node)))
                       (flow_name
                        (cdr (assoc 'name (cdr (assoc 'workflow (cdr (assoc 'workflowRun node)))))))
                       (run-id (cdr (assoc 'databaseId (cdr (assoc 'workflowRun node)))))
                       (resource-path (cdr (assoc 'resourcePath (cdr (assoc 'workflowRun node))))))
              (workline-github-artifacts repo run-id)
              (magit-insert-section
               (workline_branch nil) (make-directory (format "logs/%s" resource-path) t)
               (ghub-get
                (format "repos%s/logs" resource-path) nil
                :host (oref repo apihost)
                :reader 'ghub--decode-payload
                :auth 'workline-mode
                :callback
                (lambda (value _headers _status _req)
                  (let ((fname
                         (format "logs/log%s.zip"
                                 (replace-regexp-in-string "/" "-" resource-path))))
                    (with-temp-file fname
                      (insert value))
                    (call-process "unzip"
                                  nil
                                  0
                                  nil
                                  "-d"
                                  (format "logs%s" resource-path)
                                  "-u"
                                  fname))))
               (magit-insert-heading
                (format "  %s %s"
                        (propertize flow_name
                                    'font-lock-face
                                    'magit-section-heading)
                        (workline-format-status conclusion conclusion)))
               (magit-insert-section-body
                (seq-doseq (run (cdr (assoc 'nodes (cdr (assoc 'checkRuns node)))))
                  (let ((run-name (cdr (assoc 'name run)))
                        (run-conclusion (cdr (assoc 'conclusion run))))
                    (magit-insert-section
                     (workline_branch (list "0" nil resource-path run-name repo) t)
                     (magit-insert-heading
                      (format "   %s %s"
                              (propertize run-name 'font-lock-face 'workline-stage)
                              (workline-format-status run-conclusion run-conclusion)))
                     (magit-insert-section-body
                      (seq-doseq (step (cdr (assoc 'nodes (cdr (assoc 'steps run)))))
                        (let ((step-name (cdr (assoc 'name step)))
                              (step-conclusion (cdr (assoc 'conclusion step)))
                              (step-number (cdr (assoc 'number step))))
                          (magit-insert-section
                           (step (list step-number step-name resource-path run-name repo) t)
                           (magit-insert-heading
                            (format
                             "    %s %s"
                             (propertize (format "%2d" step-number) 'font-lock-face 'workline-grey)
                             (workline-format-status
                              (format "[%s] %s" run-conclusion step-name)
                              step-conclusion)))))))))))))))))
      (pop-to-buffer (current-buffer))
      (let ((magit-section-cache-visibility nil))
        (magit-section-show magit-root-section)))))


(defun workline-github-log-fname (step job-name resource-path run-name)
  ""
  (if job-name
      (format "logs%s/%s/%s_%s.txt"
              resource-path
              (replace-regexp-in-string "/" "_" run-name)
              step
              (replace-regexp-in-string "/" "_" job-name))
    (format "logs%s/%s_%s.txt" resource-path step (replace-regexp-in-string "/" "" run-name))))

(defun workline-retry-job-at-point-github (_step job-name resource-path _run-name repo)
  (if (not job-name)
      (ghub-post
       (format "repos%s/rerun" resource-path)
       :host (oref repo apihost)
       :reader 'ghub--decode-payload
       :auth 'workline-mode
       :callback (lambda (_value _headers _status _req)))))

(defun workline-delete-job-at-point-github (_step job-name resource-path _run-name repo)
  (if (not job-name)
      (ghub-request
       "DELETE"
       (format "repos%s" resource-path)
       :host (oref repo apihost)
       :reader 'ghub--decode-payload
       :auth 'workline-mode
       :callback (lambda (_value _headers _status _req)))))

(defun workline-cancel-job-at-point-github (_step job-name resource-path _run-name repo)
  (if (not job-name)
      (ghub-post
       (format "repos%s/cancel" resource-path)
       :host (oref repo apihost)
       :reader 'ghub--decode-payload
       :auth 'workline-mode
       :callback (lambda (_value _headers _status _req)))))

(defun workline-job-web-trace-at-point-github (_step _job-name resource-path _run-name repo)
  ""
  (if resource-path
      (browse-url (format "https://%s%s" (oref repo githost) resource-path))))

(defun workline-job-trace-artifact-at-point-github (repo artifact)
  (if (not (cdr (assoc 'expired artifact)))
      (let ((host (oref repo apihost))
            (archive_download_url (cdr (assoc 'archive_download_url artifact)))
            (id (cdr (assoc 'id artifact))))
        (ghub-get
         (substring archive_download_url (string-match "repos/" archive_download_url)) nil
         :host host
         :reader 'ghub--decode-payload
         :auth 'workline-mode
         :callback
         (lambda (value _headers _status _req)
           (let* ((fname (format "artifacts/artifacts-%s.zip" id))
                  (folder (file-name-sans-extension fname)))
             (with-temp-file fname
               (insert value))
             (call-process "unzip" nil 0 nil "-d" folder "-u" fname)
             (dired folder)))
         :errorback (lambda (value _headers _status _req) (message "%S" value))))))


(defun workline-job-trace-at-point-github (step job-name resource-path run-name repo)
  "Workflow job trace at point."
  (if (magit-section-match 'artifact)
      (workline-job-trace-artifact-at-point-github repo step)
    (with-current-buffer (get-buffer-create (format "*Workflow:%s" resource-path))
      (erase-buffer)
      (insert-file-contents (workline-github-log-fname step job-name resource-path run-name))
      (goto-char (point-min))
      (while (re-search-forward "" nil t)
        (replace-match "\n" nil nil))
      (ansi-color-apply-on-region (point-min) (point-max))
      (switch-to-buffer (current-buffer))
      (view-mode))))

(defun workline-workflow-from-ref (host owner name ref)
  "Get Github workflows from REF"
  (ghub-graphql
   `(query
     (repository
      [(owner $owner String!) (name $name String!)]
      (object
       [(expression $ref String!)]
       (\...\ on\ Commit
        (checkSuites
         [(last 6)]
         (nodes
          (conclusion) (workflowRun (databaseId) (resourcePath) (workflow (name)))
          (checkRuns
           [(last 100) (:filterBy (checkType LATEST CheckType!))]
           (nodes
            (detailsUrl)
            (name)
            (status)
            (conclusion)
            (detailsUrl)
            (resourcePath)
            (steps [(first 15)] (nodes (name) (conclusion) (number)))))))))))
   `((owner . ,owner) (name . ,name) (ref . ,ref))
   :auth 'workline-mode
   :host host))

(provide 'workline-github)

;;; workline-github.el ends here
