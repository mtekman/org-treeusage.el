;;; org-density.el --- Examine the density of org headings -*- lexical-binding: t; -*-

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

;; A minor mode to show the line or character density of org-mode files.
;; The main motivation was to help in the archiving and arrangement of
;; very large org files that might have some redundant data still in it.

;;; Code:
(require 'org-density-overlay)

(defgroup org-density nil
  "Customisation group for org-density."
  :group 'org)

(defun org-density--printstats ()
  "Print stats, mostly debugging."
  (let ((ntype (intern (format ":n%s" org-density-cycle--difftype)))
        (ptype (intern (format ":p%s" org-density-cycle--difftype))))
    (maphash
     (lambda (head info)
       (let ((indent (make-string (* 4 (car head)) ? ))
             (header (or (cdr head) "{root}"))
             (ndiffs (or (plist-get info ntype) 0))
             (percnt (or (plist-get info ptype) 100)))
         (insert
          (format "\n;;%s %3.0f -- %s {%d}"
                  indent percnt header ndiffs))))
     (with-current-buffer "lorum.org" (org-density-parse--processvisible)))))


(defvar org-density--modebind
  (let ((map (make-sparse-keymap)))
    ;; Do not inherit from parent, which would
    ;; be read-only-mode.
    (define-key map (kbd ",") 'org-density-cycle-modebackward)
    (define-key map (kbd ".") 'org-density-cycle-modeforward)
    (define-key map (kbd "l") 'org-density-cycle-toggletype)
    (define-key map (kbd "return") 'org-density-mode)
    map)
  "Keymap for minor mode.")

(define-minor-mode org-density-mode
  "The mode for org-density."
  nil
  " Ð"
  org-density--modebind
  (if org-density-mode
      (progn (add-hook 'org-cycle-hook 'org-density-overlay--setall)
             (read-only-mode t)
             (org-density-overlay--setall))
    (remove-hook 'org-cycle-hook 'org-density-overlay--setall)
    (org-density-overlay--clear)
    (read-only-mode -1)))


(add-hook 'org-density-cycle--publichook 'org-density-overlay--setall)

(provide 'org-density)
;;; org-density.el ends here
