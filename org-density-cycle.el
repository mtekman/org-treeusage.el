;;; org-density-cycle.el --- Cycle or toggle formats -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://github.com/mtekman/org-density.el
;; Keywords: outlines
;; Package-Requires: ((emacs "26.1") (dash "2.17.0") (org "9.1.6"))
;; Version: 0.1

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

;; See org-density.el

;;; Code:

;; Foreign -- setoverlays
(defcustom org-density-cycle-formats
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
  "Specify different formats to represent the density.
Some are given here as examples.  The first is the default used on startup.
The format takes 4 positional arguments:
 1. A string representing the percentage band as set in
    `org-density-percentlevels'.
 2. A float showing the current percentage
 3. An integer showing the number of lines/chars under the headline.
 4. A string with the name of headline."
  :type 'alist
  :group 'org-density)

(defvar org-density-cycle--currentmode 'bar)

(defvar org-density-cycle--difftype 'lines
  "Type is strictly either 'lines or 'chars.")

(defun org-density-cycle--usermodes (forw)
  "Cycle a user defined list of formats in direction FORW."
  (let ((oh-cm org-density-cycle--currentmode)
        (oh-fm (mapcar 'car org-density-cycle-formats))
        (direc (if forw 1 -1)))
    (let* ((curr-index (position oh-cm oh-fm))
           (next-index (mod (+ curr-index direc) (length oh-fm)))
           (next-umode (nth next-index oh-fm)))
      (setq org-density-cycle--currentmode next-umode)
      (org-density-overlay--setall) ;; TODO Find a way to turn this into an exit hook.
      (message "Mode: %s" next-umode))))

(defun org-density-cycle-modeforward ()
  "Cycle user modes forwards."
  (interactive)
  (org-density-cycle--usermodes t))

(defun org-density-cycle-modebackward ()
  "Cycle user modes backwards."
  (interactive)
  (org-density-cycle--usermodes nil))


(defun org-density-cycle-toggletype ()
  "Toggle the difference mode from characters to lines."
  (interactive)
  (let* ((cmode org-density-cycle--difftype)
         (nmode (if (eq cmode 'lines) 'chars 'lines)))
    (setq org-density-cycle--difftype nmode)
    (org-density-overlay--setall)
    (message "Type: %s" nmode)))


(provide 'org-density-cycle)
;;; org-density-cycle.el ends here
