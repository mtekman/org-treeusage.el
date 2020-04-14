;;; org-treeusage-cycle.el --- Cycle or toggle line formats -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://github.com/mtekman/org-treeusage.el
;; Keywords: outlines
;; Version: 0.2

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; See org-treeusage.el

;;; Code:
(require 'cl-lib)

(defcustom org-treeusage-cycle-formats
  '((bardiffpercname . "%1$-5s |%3$-5d|%2$5.1f%%|%4$s")
    (bardiffperc . "%1$-5s |%3$-5d|%2$5.1f%%")
    (bardiffname . "%1$s%3$-5d|%4$s")
    (bardiff . "%1$s%3$d")
    (barname . "%1$-5s |%4$s")
    (bar . "%1$-5s")
    (percname . "%2$5.1f%%|%4$s")
    (perc . "%2$5.1f%%")
    (diffname . "%3$d|%4$s")
    (diff . "%3$d"))
  "Specify different formats to represent the line or character density.
Some are given here as examples.  The first is the default used on startup.
The format takes 4 positional arguments:
 1. A string representing the percentage band as set in
    `org-treeusage-percentlevels'.
 2. A float showing the current percentage
 3. An integer showing the number of lines/chars under the headline.
 4. A string with the name of headline."
  :type 'alist
  :group 'org-treeusage)

(defvar org-treeusage-cycle--currentmode 'bar
  "Current line format.  Default is bar.")

(defvar org-treeusage-cycle--difftype 'lines
  "Current diff type.  Strictly either 'lines or 'chars.")

(defvar org-treeusage-cycle--publichook nil
  "Hook to run at the end of an interactive function.")

(defun org-treeusage-cycle--runpublichook ()
  "Run the public finish hook."
  (run-hooks 'org-treeusage-cycle--publichook))

(defun org-treeusage-cycle--usermodes (forw)
  "Cycle line formats forward if FORW, otherwise backwards."
  (let* ((oh-cm org-treeusage-cycle--currentmode)
         (oh-fm (mapcar #'car org-treeusage-cycle-formats))
         (direc (if forw 1 -1))
         (curr-index (cl-position oh-cm oh-fm))
         (next-index (mod (+ curr-index direc) (length oh-fm)))
         (next-umode (nth next-index oh-fm)))
    (setq org-treeusage-cycle--currentmode next-umode)
    (org-treeusage-cycle--runpublichook)
    (message "Mode: %s" next-umode)))

;;;###autoload
(defun org-treeusage-cycle-modeforward ()
  "Cycle line formats forwards."
  (interactive)
  (org-treeusage-cycle--usermodes t))

;;;###autoload
(defun org-treeusage-cycle-modebackward ()
  "Cycle line formats backwards."
  (interactive)
  (org-treeusage-cycle--usermodes nil))

;;;###autoload
(defun org-treeusage-cycle-toggletype ()
  "Toggle the diff type from characters to lines."
  (interactive)
  (let* ((cmode org-treeusage-cycle--difftype)
         (nmode (if (eq cmode 'lines) 'chars 'lines)))
    (setq org-treeusage-cycle--difftype nmode)
    (org-treeusage-cycle--runpublichook)
    (message "Type: %s" nmode)))


(provide 'org-treeusage-cycle)
;;; org-treeusage-cycle.el ends here
