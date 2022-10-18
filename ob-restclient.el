;;; ob-restclient.el --- org-babel functions for restclient-mode

;; Copyright (C) Alf Lervåg

;; Author: Alf Lervåg
;; Keywords: literate programming, reproducible research
;; Homepage: https://github.com/alf/ob-restclient.el
;; Version: 0.02
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
  "The path to `jq', for post-processing. Uses the PATH by default")

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
        (dolist (p params)
          (let ((key (car p))
                (value (cdr p)))
            (when (eql key :var)
              (insert (format ":%s = <<\n%s\n#\n" (car value) (cdr value))))))
        (insert body)
        (goto-char (point-min))
        (delete-trailing-whitespace)
        (goto-char (point-min))
        (restclient-http-parse-current-and-do
         'restclient-http-do (org-babel-restclient--raw-payload-p params) t))

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

      (when (not (org-babel-restclient--return-pure-payload-result-p params))
        (org-babel-restclient--wrap-result))

      (buffer-string))))

;;;###autoload
(defun org-babel-variable-assignments:restclient (params)
  "Return a list of restclient statements assigning the block's variables specified in PARAMS."
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


(defun org-babel-restclient--raw-payload-p (params)
  "Return t if the `:results' key in PARAMS contain `file'."
  (let ((result-type (cdr (assoc :results params))))
    (when result-type
      (string-match "file" result-type))))

(provide 'ob-restclient)
;;; ob-restclient.el ends here
