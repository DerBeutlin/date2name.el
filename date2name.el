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
(defvar date2name-date-regexp "^[0-9][0-9][0-9][0-9]-[0-1][0-9]-[0-9][0-9]"
  "regexp for detecting date in front of filename")

(defvar date2name-date-format "%Y-%m-%d" "format for formatting date in front of filename")

(defvar date2name-datetime-regexp "^[0-9][0-9][0-9][0-9]-[0-1][0-9]-[0-9][0-9]T[0-2][0-9].[0-5][0-9].[0-5][0-9]"
  "regexp for detecting datetime in front of filename")


(defvar date2name-datetime-format "%Y-%m-%dT%H.%M.%S"
  "format for formatting datetime in front of filename")

(defvar date2name-default-separation-character
  "_" "character to use to seperate timestamp and filename")

(defvar date2name-default-separation-character-regexp
  "[ _]" "regexp for characters that can separate timestamp and filename")

(defcustom date2name-enable-smart-separation-character-chooser
  nil "if t then if the originial filename contains at least on space, a space is used for seperation,
   otherwise date2name-default-seperation-character is used

Example if t:

    foo_bar.txt -> 20180724T13.48.25_foo_bar.txt
    foo bar.txt -> 20180724T13.48.25 foo bar.txt
")


(defun date2name-choose-separation-character (path)
  (if date2name-enable-smart-separation-character-chooser
      (let ((filename (file-name-nondirectory path)))
        (if (string-match-p (regexp-quote " ")
                            filename)
            " "
          date2name-default-separation-character))
    date2name-default-separation-character))

(defun date2name-remove-date (path)
  "remove potential prefix dates from PATH and return the new path"
  (let ((directory (file-name-directory path))
        (file-name (file-name-nondirectory path)))
    (concat directory
            (replace-regexp-in-string (concat date2name-date-regexp date2name-default-separation-character-regexp)
                                      ""
                                      (replace-regexp-in-string (concat date2name-datetime-regexp date2name-default-separation-character-regexp)
                                                                ""
                                                                file-name)))))

(defun date2name-prepend-date (path time &optional withtime)
  "prepends TIME to PATH if WITHTIME is not nil also with time otherwise only date"
  (let ((directory (file-name-directory path))
        (filename (date2name-remove-date (file-name-nondirectory path)))
        (datestring (if withtime
                        (format-time-string (concat date2name-datetime-format
                                                    (date2name-choose-separation-character path))
                                            time)
                      (format-time-string (concat date2name-date-format
                                                  (date2name-choose-separation-character path))
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
   with one prefix arg prompt for the user to input time (once for each file)
   with two prefix args promt for the time once and use the same time for each file 
   if withtime is notnil also prepend the times otherwise only the dates"
  (let ((filenames (if (dired-get-marked-files)
                       (dired-get-marked-files)
                     '((dired-get-filename))))
        (save-date (when (equal (prefix-numeric-value arg) 16) (org-read-date withtime t))))
    (dolist (filename filenames)
      (let ((date (if (not save-date)
                      (if (not arg) (date2name-file-attribute-modification-time (file-attributes filename)) (org-read-date withtime t))
                    save-date)))
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
