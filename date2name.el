;;; date2name.el --- Package to prepend ISO Timestamps to files  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Max Beutelspacher

;; Author: Max Beutelspacher 
;; Keywords: files, convenience
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'org)
(defvar date2name-date-regexp "^[0-9][0-9][0-9][0-9]-[0-1][0-9]-[0-9][0-9]_"
  "regexp for detecting date in front of filename")

(defvar date2name-date-format "%Y-%m-%d_"
  "format for formatting date in front of filename")

(defvar date2name-datetime-regexp "^[0-9][0-9][0-9][0-9]-[0-1][0-9]-[0-9][0-9]T[0-2][0-9].[0-5][0-9].[0-5][0-9]_"
  "regexp for detecting datetime in front of filename")


(defvar date2name-datetime-format "%Y-%m-%dT%H.%M.%S_"
  "format for formatting datetime in front of filename")

(defun date2name-remove-date (path)
  "remove potential prefix dates from PATH and return the new path"
  (let ((directory (file-name-directory path))
        (file-name (file-name-nondirectory path)))
    (concat directory
            (replace-regexp-in-string date2name-date-regexp
                                      ""
                                      (replace-regexp-in-string date2name-datetime-regexp
                                                                "" file-name)))))

(defun date2name-prepend-date (path time &optional withtime)
  "prepends TIME to PATH if WITHTIME is not nil also with time otherwise only date"
  (let ((directory (file-name-directory path))
        (filename (date2name-remove-date (file-name-nondirectory path)))
        (datestring (if withtime
                        (format-time-string date2name-datetime-format
                                            time)
                      (format-time-string date2name-date-format
                                          time))))
    (concat directory datestring filename)))


(defun date2name-prepend-date-write (path time &optional withtime)
  "prepends TIME to PATH and renames the file if WITHTIME is not nil also with time otherwise only date"
  (let ((new-path (date2name-prepend-date path time withtime)))
    (when (not (string= path new-path))
      (dired-rename-file path new-path nil))))

(defun date2name-add-date-to-name (arg &optional withtime)
  "apply to all marked files or if no files is marked apply to the file on point the following
   if ARG is nil get the modification time and prepend it
   if ARG is not nil prompt for the user to input time
   if withtime is notnil also prepend the times otherwise only the dates"
  (let ((filenames (if (dired-get-marked-files)
                       (dired-get-marked-files)
                     '((dired-get-filename)))))
    (dolist (filename filenames)
      (let ((date (if (not arg)
                      (date2name-file-attribute-modification-time (file-attributes filename))
                    (org-read-date withtime t))))
        (date2name-prepend-date-write filename date
                                      withtime)))))

(if (fboundp 'file-attribute-modification-time)
    (defalias 'date2name-file-attribute-modification-time 'file-attribute-modification-time)
  (defsubst date2name-file-attribute-modification-time (attributes)
    "extracts the modification time from ATTRIBUTES"
    (nth 5 attributes)))

(defun date2name-dired-add-date-to-name (arg)
  "apply to all marked files or if no files is marked apply to the file on point the following
   if ARG is nil get the modification date and prepend it
   if ARG is not nil prompt for the user to input date"
  (interactive "P")
  (date2name-add-date-to-name arg nil))

(defun date2name-dired-add-datetime-to-name (arg)
  "apply to all marked files or if no files is marked apply to the file on point the following
   if ARG is nil get the modification time and prepend it
   if ARG is not nil prompt for the user to input time"
  (interactive "P")
  (date2name-add-date-to-name arg t))

(provide 'date2name)
;;; date2name.el ends here
