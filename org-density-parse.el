;;; org-density-parse.el --- Main parsing library for org -*- lexical-binding: t; -*-

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
(require 'org-density-mathpos) ;; brings nil

(defvar org-density-parse--prntalist nil)

(defun org-density-parse--makekey (level header)
  "Generate key for hash using LEVEL and HEADER."
  (cons level header))

(defun org-density-parse--makevalues (line chars
                                     &optional pline pchars bounds)
  "Create a plist from from LINE and CHARS.
Use their percentages PLINE and PCHARS, and their BOUNDS."
  (list :nlines line :nchars chars
        :plines pline :pchars pchars
        :bounds bounds))


(defun org-density-parse--nextmatch (point1)
  "Get next match from current POINT1."
  (progn (org-next-visible-heading 1)
         (not (eq point1 (point)))))

(defun org-density-parse--makeroot (hasher)
  "Perform once.  Get full bounds, and put into HASHER."
  (let* ((pend (progn (goto-char (point-max))
                      (org-backward-sentence)
                      (point)))
         (pbeg (progn (goto-char 0)
                      (org-next-visible-heading 1)
                      (point))))
    (let ((dchar (- pend pbeg))
          (dline (- (line-number-at-pos pend)
                    (line-number-at-pos pbeg)))
          (dkey (org-density-parse--makekey 0 nil)))
      (move-beginning-of-line 0)
      (puthash dkey
               (org-density-parse--makevalues dline dchar)
               hasher)
      dkey)))


(defun org-density-parse--updateparents (lvl-now previousk)
  "Get or update parent into PRNT-ALIST based on PREVIOUSK.
From current level LVL-NOW."
  (let ((prev-lvl (car previousk))
        (prev-hdr (cdr previousk))
        (curr-parent (car org-density-parse--prntalist)))
    (cond ((not prev-lvl)
           curr-parent)
          ((> lvl-now prev-lvl)
           ;; Gone N level's deep, push the last
           ;; heading as the new parent at level
           (car (push
                 (org-density-parse--makekey prev-lvl prev-hdr)
                 org-density-parse--prntalist)))
          ;;
          ((< lvl-now prev-lvl)
           ;; Returned to a level up. Pop all levels up to.
           (while (>= (caar org-density-parse--prntalist) lvl-now)
             (pop org-density-parse--prntalist))
           (car org-density-parse--prntalist))
          (t curr-parent))))

(defun org-density-parse--processvisible ()
  "The idea is to get stats only for the visible portions of the buffer.
To investigate further, expand a heading. Updates `org-density--hashmap'."
  (save-excursion
    (setq org-density-parse--prntalist nil)
    (let ((hasher (make-hash-table :test 'equal))
          (prnt-curr nil)
          (prev-key nil))
      ;; Jump to beginning and parse headers
      (push (org-density-parse--makeroot hasher) org-density-parse--prntalist)
      ;;
      (while (org-density-parse--nextmatch (point))
        (let ((info (cadr (org-element-at-point))))
          (let ((level (plist-get info :level))
                (bound (org-density-mathpos--gettitlebounds info)))
            (let ((head (car bound))
                  (hrng (cdr bound)))
              (when head
                ;; Check and update parent
                (setq prnt-curr (org-density-parse--updateparents
                                 level prev-key))
                ;;
                (let ((dline (org-density-mathpos--calcnlines info))
                      (dchar (org-density-mathpos--calcnchars info))
                      (elkey (org-density-parse--makekey level
                                                     head))
                      (prnt-inf (gethash prnt-curr hasher)))
                  (let ((percline (org-density-mathpos--calcperc
                                   dline
                                   (plist-get prnt-inf :nlines)))
                        (percchar (org-density-mathpos--calcperc
                                   dchar
                                   (plist-get prnt-inf :nchars))))
                    (puthash elkey
                             (org-density-parse--makevalues dline
                                                      dchar
                                                      percline
                                                      percchar
                                                      hrng)
                             hasher)
                    (setq prev-key elkey))))))))
      (setq org-density--hashmap hasher))))


(provide 'org-density-parse)
;;; org-density-parse.el ends here
