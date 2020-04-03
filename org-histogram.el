;;; org-histogram.el --- Peek the density -*- lexical-binding: t; -*-

;; Copright (C) 2020 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://github.com/mtekman/org-histogram.el
;; Keywords: outlines
;; Package-Requires: ((emacs "24.3"))
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

;;

;;; Code:
(require 'org-element)

(defun org-histogram--makekey (level header)
  "Generate key for hash using LEVEL and HEADER."
  (cons level header))

(defun org-histogram--makevalues (line
                                  &optional perc bounds)
  "Create a value from LINE and optional PERC, BOUNDS set."
  (list :nlines line :percent perc :bounds bounds))

(defun org-histogram--getlineinf (info keyf)
  "Get line number for specific INFO keyword KEYF."
  (line-number-at-pos (plist-get info keyf)))

(defun org-histogram--calcperc (child par)
  "Calculate percentage of CHILD against PAR.
Percentages are not super accurate, but are a good gauge."
  (/ (float (* 100 child)) par))

(defun org-histogram--calcnlines (info)
  "Calculate the number of lines from INFO."
  (- (org-histogram--getlineinf info :end)
     (org-histogram--getlineinf info :begin)))

(defun org-histogram--nextmatch (point1)
  "Get next match from current POINT1."
  (progn (org-next-visible-heading 1)
         (not (eq point1 (point)))))

(defun org-histogram--makeroot (hasher)
  "Perform once.  Get full bounds, and put into HASHER."
  (let ((dline (- (progn (goto-char (point-max))
                         (org-backward-sentence)
                         (line-number-at-pos (point)))
                  (progn (goto-char 0)
                         (org-next-visible-heading 1)
                         (line-number-at-pos (point)))))
        (dkey (org-histogram--makekey 0 nil)))
    (move-beginning-of-line 0)
    (puthash dkey (org-histogram--makevalues dline) hasher)
    dkey))

(defun org-histogram--gettitlebounds (info)
  "Get title and bounds from INFO."
  (let ((head (plist-get info :title))
        (bend (plist-get info :contents-begin))
        (bbeg (line-beginning-position)))
    (when head
      (save-excursion
        (goto-char bbeg) ;; important
        (let* ((end (search-forward-regexp
                     ;; Capture: [1/1] User
                     (shell-quote-argument head) bend))
               (beg (+ 2 (search-backward "* " bbeg))))
          `(,head . (,beg . ,end)))))))

(defvar prnt-alist nil)


(defun org-histogram--updateparents (lvl-now previousk)
  "Get or update parent into PRNT-ALIST based on PREVIOUSK.
From current level LVL-NOW."
  (let ((prev-lvl (car previousk))
        (prev-hdr (cdr previousk))
        (curr-parent (car prnt-alist)))
    (cond ((not prev-lvl)
           curr-parent)
          ((> lvl-now prev-lvl)
           ;; Gone N level's deep, push the last
           ;; heading as the new parent at level
           (car (push
                 (org-histogram--makekey prev-lvl prev-hdr)
                 prnt-alist)))
          ;;
          ((< lvl-now prev-lvl)
           ;; Returned to a level up. Pop all levels up to.
           (while (>= (caar prnt-alist) lvl-now)
             (pop prnt-alist))
           (car prnt-alist))
          (t curr-parent))))

(defun org-histogram--processvisible ()
  "The idea is to get stats only for the visible portions of the buffer.
To investigate further, expand a heading."
  (save-excursion
    (setq prnt-alist nil)
    (let ((hasher (make-hash-table :test 'equal))
          (prnt-curr nil)
          (prev-key nil))
      ;; Jump to beginning and parse headers
      (push (org-histogram--makeroot hasher) prnt-alist)
      ;;
      (while (org-histogram--nextmatch (point))
        (let ((info (cadr (org-element-at-point))))
          (let ((level (plist-get info :level))
                (bound (org-histogram--gettitlebounds info)))
            (let ((head (car bound))
                  (hrng (cdr bound)))
              (when head
                ;; Check and update parent
                (setq prnt-curr (org-histogram--updateparents
                                 level prev-key))
                ;;
                (let ((dline (org-histogram--calcnlines info))
                      (elkey (org-histogram--makekey level
                                                     head))
                      (prnt-inf (gethash prnt-curr hasher)))
                  (let ((percent (org-histogram--calcperc
                                  dline
                                  (plist-get prnt-inf :nlines))))
                    (puthash elkey
                             (org-histogram--makevalues dline
                                                        percent
                                                        hrng)
                             hasher)
                    (setq prev-key elkey))))))))
      hasher)))

(defvar format-choices
  '((barlineperc . "%1$-5s |%3$-5d|%2$5.1f%%")
    (barline . "%1$s%3$d")
    (perc . "%2$5.1f%%")
    (line . "%3$d")
    (bar . "%1$-5s")
    (custom . nil)))

(defcustom org-histogram-percentlevels
  '(((-9 .  1)  . ▏)
    (( 2 . 10)  . ▎)
    ((11 . 20)  . ▋)
    ((21 . 30)  . █)
    ((31 . 40)  . █▋)
    ((41 . 50)  . ██)
    ((51 . 60)  . ██▋)
    ((61 . 70)  . ███)
    ((71 . 80)  . ███▋)
    ((81 . 90)  . ████)
    ((91 . 110) . ████▋))
  "Set the percentage lower and upper bands and  the corresponding symbol."
  :type 'alist
  :group 'org-histogram)

(defcustom org-histogram-customformat  "%1$-5s--%3$d"
  "Manually specify the format for each string.
The format takes 3 positional arguments:
 1. A string representing the percentage band as set in
    `org-histogram-percentlevels'.
 2. A float showing the current percentage
 3. An integer showing the number of lines in the section."
  :type 'string
  :group 'org-histogram)

(defcustom org-histogram-formatchoice 'barline
  "Choose the format line to express the density.
Choices are bar, line, perc, barline, barlineperc, or custom.
For custom, users should set the `org-histogram-customformat' variable."
  :options (mapcar 'car format-choices)
  :type 'symbol
  :group 'org-histogram)

(defun org-histogram--removeoverlays ()
  "Remove all overlays."
  (let ((ovs (overlays-in (point-min) (point-max))))
    (if (cl-loop for ov in ovs
                 thereis (overlay-get ov :org-histogram))
        (dolist (ov ovs)
          (when (overlay-get ov :org-histogram)
            (delete-overlay ov))))))


(defun org-histogram--getformatline ()
  "Get format line, if custom, then use custom format string."
  (or (alist-get org-histogram-formatchoice format-choices)
      org-histogram-customformat))


(defun org-histogram--setoverlays ()
  "Set the overlays."
  (org-histogram--removeoverlays)
  (let ((lineform (org-histogram--getformatline)))
    (maphash
     (lambda (head info)
       (let ((bounds (plist-get info :bounds))
             (nlines (plist-get info :nlines))
             (percer (plist-get info :percent))
             (leveln (car head)))
         (if percer
             (let ((oface (intern (format "org-level-%s" leveln)))
                   (ovner (make-overlay (car bounds) (cdr bounds)))
                   (p-bar (cdr (--first (<= (caar it) (truncate percer)
                                            (cdar it))
                                        org-histogram-percentlevels))))
               (overlay-put ovner :org-histogram t)
               (overlay-put ovner 'face oface)
               (overlay-put ovner 'display
                            (format lineform p-bar percer nlines))))))
     (org-histogram--processvisible))))

(defun org-histogram--printstats ()
  "Print stats."
  (maphash
   (lambda (head info)
     (let ((indent (make-string (* 4 (car head)) ? ))
           (header (or (cdr head) "{root}"))
           (nlines (or (plist-get info :nlines) 0))
           (percnt (or (plist-get info :percent) 100)))
       (insert
        (format "\n;;%s %3.0f -- %s {%d}"
                indent percnt header nlines))))
   (with-current-buffer "lorum.org" (org-histogram--processvisible))))

(provide 'org-histogram)
;;; org-histogram.el ends here
