;;; ob-restclient.el --- org-babel functions for restclient-mode -*- lexical-binding: t -*-

;; Copyright (C) Alf Lervåg

;; Author: Alf Lervåg
;; Keywords: literate programming, reproducible research
;; Homepage: https://github.com/alf/ob-restclient.el
;; Version: 0.03
;; Package-Requires: ((restclient "0"))

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; This is a very simple first iteration at integrating restclient.el
;; and org-mode.

;;; Requirements:
;; restclient.el

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
(require 'org-table)
(require 'restclient)

(defvar org-babel-default-header-args:restclient
  '((:results . "raw"))
  "Default arguments for evaluating a restclient block.")

(defcustom org-babel-restclient--jq-path "jq"
  "The path to `jq', for post-processing. Uses the PATH by default."
  :type '(string)
  :group 'org-babel)

(defun org-babel-restclient--extract-body (content-type)
  "Extract response body based on CONTENT-TYPE."
  (pcase (and content-type (downcase content-type))
    ((or "application/json" "application/ld+json")
     (org-babel-restclient--extract-json-body))
    ((or "application/xml" "text/xml" "application/atom+xml" "text/html" "application/rss+xml")
     (org-babel-restclient--extract-xml-body))
    (_
     (org-babel-restclient--extract-json-body)))) ; Fallback to JSON

(defun org-babel-restclient--extract-json-body ()
  "Extract the first JSON object or array in buffer, ignoring HTTP headers."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^[[:space:]]*\\({\\|\\[\\)" nil t)
        (let ((start (match-beginning 0)))
          (goto-char start)
          (let ((json-end
                 (condition-case nil
                     (progn
                       (forward-sexp)
                       (point))
                   (error (point-max)))))
            (string-trim (buffer-substring-no-properties start json-end))))
      "")))  ; No JSON found

(defun org-babel-restclient--extract-xml-body ()
  "Extract the first XML element in buffer, ignoring HTTP headers."
  (save-excursion
    (goto-char (point-min))
    ;; Skip headers to get to actual content
    (when (re-search-forward "^\\r?\\n" nil t)
      (goto-char (match-beginning 0)))
    ;; Look for any XML content (even minimal like <root/>)
    (if (re-search-forward "[][^>]*" nil t)
        (let ((start (match-beginning 0)))
          (goto-char start)
          ;; Find the end of this XML construct
          (re-search-forward "[^>]*[>]" nil t)
          (let ((end (match-end 0)))
            (string-trim (buffer-substring-no-properties start end))))
      "")))  ; No XML found

(defun org-babel-restclient--get-content-type ()
  "Extract Content-Type header from response."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^Content-Type:[ \t]*\\([^;\n]+\\)" nil t)
        (match-string 1)
      "application/json"))) ; Default to JSON

(defun org-babel-restclient--result-type (params)
  "Detect :results type from PARAMS."
  (let ((types (split-string (or (cdr (assoc :results params)) "output") " ")))
    (cond
     ((member "raw" types)   'raw)
     ((member "org" types)   'org)
     ((member "value" types) 'org)
     ((member "json" types)  'json)
     ((member "xml" types)   'xml)
     ((member "nxml" types)  'nxml)
     (t                      'output))))

;;;###autoload
(defun org-babel-execute:restclient (body params)
  "Execute Restclient block for Org Babel, supporting JSON, XML and nXML output."
  (message "executing Restclient source code block")
  (let* ((response-buffer-name (generate-new-buffer-name "*ob-restclient-response*"))
         (restclient-same-buffer-response t)
         (restclient-response-body-only nil)
         (restclient-same-buffer-response-name response-buffer-name)
         (raw-only t)
         (suppress-response-buffer (fboundp #'restclient-http-send-current-suppress-response-buffer))
         (display-buffer-alist
          (cons '("\\*temp\\*" display-buffer-no-window (allow-no-window . t))
                display-buffer-alist)))
    (with-temp-buffer
      (insert (org-babel-expand-body:generic body params
                                             (org-babel-variable-assignments:restclient params)))
      (goto-char (point-min))
      (delete-trailing-whitespace)
      (goto-char (point-min))
      (restclient-http-parse-current-and-do 'restclient-http-do raw-only t suppress-response-buffer))
    (while restclient-within-call (sleep-for 0.05))
    (let ((results-buffer (get-buffer response-buffer-name)))
      (unless (and results-buffer (buffer-live-p results-buffer))
        (error "Restclient did not produce a live response buffer"))
      (with-current-buffer results-buffer
        (goto-char (point-min))
        (when buffer-read-only (setq buffer-read-only nil))
        (when (zerop (buffer-size)) (error "Restclient encountered an error"))
        (let* ((content-type (org-babel-restclient--get-content-type))
               (body (progn
                       ;; Move past headers to find actual content
                       (goto-char (point-min))
                       (search-forward "\n\n" nil 't)
                       (string-trim (buffer-substring (point) (point-max)))))
               ;; Format the body if formatters are available
               (formatted-body
                (cond
                 ((string-match-p "xml" (downcase content-type))
                  (if (fboundp 'xml-format-region)
                      (with-temp-buffer
                        (insert body)
                        (goto-char (point-min))
                        (xml-format-region (point-min) (point-max))
                        (buffer-string))
                    body))
                 ((string-match-p "json" (downcase content-type))
                  (if (fboundp 'json-reformat-region)
                      (with-temp-buffer
                        (insert body)
                        (goto-char (point-min))
                        (json-reformat-region (point-min) (point-max))
                        (buffer-string))
                    body))
                 (t body)))
               (result-type (org-babel-restclient--result-type params))
               (result-lang
                (cond
                 ((string-match-p "xml" (downcase content-type)) "xml")
                 ((string-match-p "json" (downcase content-type)) "json")
                 (t "text")))
               (output
                (pcase result-type
                  (`raw (format "#+BEGIN_SRC %s\n%s%s#+END_SRC\n" result-lang formatted-body
                                (if (string-suffix-p "\n" formatted-body) "" "\n")))
                  (`org (format "#+BEGIN_SRC %s\n%s%s#+END_SRC\n" result-lang formatted-body
                                (if (string-suffix-p "\n" formatted-body) "" "\n")))
                  (`json (format "#+BEGIN_SRC json\n%s%s#+END_SRC\n" formatted-body
                                 (if (string-suffix-p "\n" formatted-body) "" "\n")))
                  (`xml (format "#+BEGIN_SRC xml\n%s%s#+END_SRC\n" formatted-body
                                (if (string-suffix-p "\n" formatted-body) "" "\n")))
                  (`nxml (format "#+BEGIN_SRC nxml\n%s%s#+END_SRC\n" formatted-body
                                 (if (string-suffix-p "\n" formatted-body) "" "\n")))
                  (_ formatted-body))))
          (kill-buffer results-buffer)
          output)))))

;;;###autoload
(defun org-babel-variable-assignments:restclient (params)
  "Return a list of statements assigning variables specified in PARAMS."
  (mapcar
   (lambda (pair)
     (let ((name (car pair))
           (value (cdr pair))
           (format-string ":%s = %s\n"))
       (when (string-match-p "\n" value)
         (setq format-string ":%s = <<\n%s\n#\n"))
       (format format-string name value)))
   (org-babel--get-vars params)))

(defun org-babel-restclient--wrap-result ()
  "Wrap the contents of the buffer in an `org-mode' src block."
  (let ((mode-name (substring (symbol-name major-mode) 0 -5)))
    (insert (format "#+BEGIN_SRC %s\n" mode-name))
    (goto-char (point-max))
    (unless (and (bolp) (eolp))
      (insert "\n"))
    (insert "#+END_SRC\n")))

(defun org-babel-restclient--should-hide-headers-p (params)
  "Return `t' if headers should be hidden."
  (or (org-babel-restclient--return-pure-payload-result-p params)
      (assq :noheaders params)
      (assq :jq params)))

(defun org-babel-restclient--return-pure-payload-result-p (params)
  "Return `t' if the `:results' key in PARAMS contains `value' or `table'."
  (let ((result-type (cdr (assoc :results params))))
    (when result-type
      (string-match "value\\|table" result-type))))

(defun org-babel-prep-session:restclient (_session _params)
  "Return an error because restclient does not support sessions."
  (error "Restclient does not support sessions"))

(defun org-babel-restclient--raw-payload-p (params)
  "Return t if the `:results' key in PARAMS contain `file'."
  (let ((result-type (cdr (assoc :results params))))
    (when result-type
      (and (not (org-babel-restclient--should-hide-headers-p params))
           (string-match "file" result-type)))))

(provide 'ob-restclient)
;;; ob-restclient.el ends here
