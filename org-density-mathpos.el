;;; org-density-mathpos.el --- Math and Position functions for parsing -*- lexical-binding: t; -*-

;; Copright (C) 2020 Mehmet Tekman <mtekman89@gmail.com>

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
(defun org-density-mathpos--getlineinf (info keyf)
  "Get line number for specific INFO keyword KEYF."
  (line-number-at-pos (plist-get info keyf)))


(defun org-density-mathpos--calcperc (child par)
  "Calculate percentage of CHILD against PAR.
Percentages are not super accurate, but are a good gauge."
  (/ (float (* 100 child)) par))


(defun org-density-mathpos--calcnlines (info)
  "Calculate the number of lines from INFO."
  (- (org-density-mathpos--getlineinf info :end)
     (org-density-mathpos--getlineinf info :begin)))


(defun org-density-mathpos--calcnchars (info)
  "Calculate the number of chracters from INFO."
  (- (plist-get info :end)
     (plist-get info :begin)))


(defsubst org-density-mathpos--searchforward (header endmark)
  "Search forward for HEADER up to ENDMARK.
At first use shell argument, or failing that, without."
  (let ((res (search-forward-regexp
              (shell-quote-argument header)
              endmark t)))
    ;; no regex if it fails
    (or res (search-forward header endmark))))


(defun org-density-mathpos--gettitlebounds (info)
  "Get title and bounds from INFO."
  (let ((head (plist-get info :title))
        (bend (plist-get info :contents-begin))
        (bbeg (line-beginning-position)))
    (when head
      (save-excursion
        (goto-char bbeg) ;; important
        (let* ((end (org-density-mathpos--searchforward head bend))
               (beg (progn (search-backward-regexp "^\\*+ " bbeg)
                           (match-end 0))))
          `(,head . (,beg . ,end)))))))

(provide 'org-density-mathpos)
;;; org-density-mathpos.el ends here
