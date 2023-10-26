;;; ob-restclient.el --- org-babel functions for restclient-mode

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
(require 'restclient)

(defvar org-babel-default-header-args:restclient
  `((:results . "raw"))
  "Default arguments for evaluating a restclient block.")

(defcustom org-babel-restclient--jq-path "jq"
  "The path to `jq', for post-processing. Uses the PATH by default"
  :type '(string)
  :group 'org-babel)

;;;###autoload
(defun org-babel-execute:restclient (body params)
  "Execute a block of Restclient code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (message "executing Restclient source code block")
  (with-temp-buffer
    (let ((results-buffer (current-buffer))
          (restclient-same-buffer-response t)
          (restclient-response-body-only (org-babel-restclient--should-hide-headers-p params))
          (restclient-same-buffer-response-name (buffer-name))
          (display-buffer-alist
           (cons
            '("\\*temp\\*" display-buffer-no-window (allow-no-window . t))
            display-buffer-alist)))

      (insert (buffer-name))
      (with-temp-buffer
	(insert
	 (org-babel-expand-body:generic
	  body params
	  (org-babel-variable-assignments:restclient params)))
        (goto-char (point-min))
        (delete-trailing-whitespace)
        (goto-char (point-min))
        (if (fboundp #'restclient-http-send-current-suppress-response-buffer)
            (restclient-http-parse-current-and-do
             'restclient-http-do (org-babel-restclient--raw-payload-p params) t t)
          (restclient-http-parse-current-and-do
           'restclient-http-do (org-babel-restclient--raw-payload-p params) t)))

      (while restclient-within-call
        (sleep-for 0.05))

      (goto-char (point-min))
      (when (equal (buffer-name) (buffer-string))
        (error "Restclient encountered an error"))

      (when-let* ((jq-header (assoc :jq params))
                  (jq-path "jq")
		  (jq-args (or (cdr (assoc :jq-args params)) "")))
        (shell-command-on-region
         (point-min)
         (point-max)
         (format "%s %s--args %s" org-babel-restclient--jq-path
		 (if (assq :jq-args params) (format "%s " jq-args) "")
                 (shell-quote-argument (cdr jq-header)))
         (current-buffer)
         t))

      ;; widen if jq but not pure payload
      (when (and (assq :jq params)
                 (not (assq :noheaders params))
                 (not (org-babel-restclient--return-pure-payload-result-p params)))
        (widen))

      (if (member "table" (cdr (assoc :result-params params)))
          (let* ((pmax (point-max))
	         (separator '(4))
	         (result
	          (condition-case err
		      (let ((pmax (point-max)))
		        ;; If the buffer is empty, don't bother trying to
		        ;; convert the table.
		        (when (> pmax 1)
		          (org-table-convert-region (point-min) pmax separator)
		          (delq nil
			        (mapcar (lambda (row)
				          (and (not (eq row 'hline))
					       (mapcar #'org-babel-string-read row)))
				        (org-table-to-lisp)))))
		    (error
		     (display-warning 'org-babel
				      (format "Error reading results: %S" err)
				      :error)
		     nil))))
	    (pcase result
	      (`((,scalar)) scalar)
	      (`((,_ ,_ . ,_)) result)
	      (`(,scalar) scalar)
	      (_ result)))
        (when (not (org-babel-restclient--return-pure-payload-result-p params))
          (org-babel-restclient--wrap-result))
        (buffer-string)))))

;;;###autoload
(defun org-babel-variable-assignments:restclient (params)
  "Return a list of statements assigning variables specified in PARAMS."
  (mapcar
   (lambda (pair)
     (let ((name (car pair))
           (value (cdr pair)))
       (format ":%s = %s" name value)))
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
      (string-match "file" result-type))))

(provide 'ob-restclient)
;;; ob-restclient.el ends here
