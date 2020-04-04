;;; org-histogram.el --- Examine the density of org headings -*- lexical-binding: t; -*-

;; Copright (C) 2020 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://github.com/mtekman/org-histogram.el
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

;; A minor mode to show the line or word density of org-mode files.
;; The main motivation was to help in the archiving and arrangement
;; of very large org files that might have some redundant data still
;; in it.

;;; Code:
(require 'org-element)
(require 'dash)

(defvar org-histogram--prntalist nil)
(defvar org-histogram--currentmode 'bar)


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


(defvar org-histogram--backupformat "%1$-5s--%3$d"
  "Fallback in case an invalid format is chosen by the user.")

(defcustom org-histogram-formatchoices
  '((barlinepercname . "%1$-5s |%3$-5d|%2$5.1f%%|%4$s")
    (barlineperc . "%1$-5s |%3$-5d|%2$5.1f%%")
    (barlinename . "%1$s%3$-5d|%4$s")
    (barline . "%1$s%3$d")
    (barname . "%1$-5s |%4$s")
    (bar . "%1$-5s")
    (percname . "%2$5.1f%%|%4$s")
    (perc . "%2$5.1f%%")
    (linename . "%3$d|%4$s")
    (line . "%3$d"))
  "Specify different formats to represent the density.
Some are given here as examples.  The first is the default used on startup.
The format takes 4 positional arguments:
 1. A string representing the percentage band as set in
    `org-histogram-percentlevels'.
 2. A float showing the current percentage
 3. An integer showing the number of lines in the section.
 4. A string with the name of header."
  :type 'alist
  :group 'org-histogram)

(defun org-histogram--cycleusermodes (forw)
  "Cycle a user defined list of formats in direction FORW."
  (let ((oh-cm org-histogram--currentmode)
        (oh-fm (mapcar 'car org-histogram-formatchoices))
        (direc (if forw 1 -1)))
    (let* ((curr-index (position oh-cm oh-fm))
           (next-index (mod (+ curr-index direc) (length oh-fm)))
           (next-umode (nth next-index oh-fm)))
      (setq org-histogram--currentmode next-umode)
      (org-histogram--setoverlays)
      (message "Mode: %s" next-umode))))

(defun org-histogram-cycleusermodes-forw ()
  "Cycle user modes forwards."
  (interactive)
  (org-histogram--cycleusermodes t))

(defun org-histogram-cycleusermodes-back ()
  "Cycle user modes backwards."
  (interactive)
  (org-histogram--cycleusermodes nil))

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


(defsubst org-histogram--searchforward (header endmark)
  "Search forward for HEADER up to ENDMARK.
At first use shell argument, or failing that, without."
  (let ((res (search-forward-regexp
              (shell-quote-argument header)
              endmark t)))
    (or res (search-forward-regexp header endmark))))

(defun org-histogram--gettitlebounds (info)
  "Get title and bounds from INFO."
  (let ((head (plist-get info :title))
        (bend (plist-get info :contents-begin))
        (bbeg (line-beginning-position)))
    (when head
      (save-excursion
        (goto-char bbeg) ;; important
        (let* ((end (org-histogram--searchforward head bend))
               (beg (+ 2 (search-backward "* " bbeg))))
          `(,head . (,beg . ,end)))))))


(defun org-histogram--updateparents (lvl-now previousk)
  "Get or update parent into PRNT-ALIST based on PREVIOUSK.
From current level LVL-NOW."
  (let ((prev-lvl (car previousk))
        (prev-hdr (cdr previousk))
        (curr-parent (car org-histogram--prntalist)))
    (cond ((not prev-lvl)
           curr-parent)
          ((> lvl-now prev-lvl)
           ;; Gone N level's deep, push the last
           ;; heading as the new parent at level
           (car (push
                 (org-histogram--makekey prev-lvl prev-hdr)
                 org-histogram--prntalist)))
          ;;
          ((< lvl-now prev-lvl)
           ;; Returned to a level up. Pop all levels up to.
           (while (>= (caar org-histogram--prntalist) lvl-now)
             (pop org-histogram--prntalist))
           (car org-histogram--prntalist))
          (t curr-parent))))

(defun org-histogram--processvisible ()
  "The idea is to get stats only for the visible portions of the buffer.
To investigate further, expand a heading."
  (save-excursion
    (setq org-histogram--prntalist nil)
    (let ((hasher (make-hash-table :test 'equal))
          (prnt-curr nil)
          (prev-key nil))
      ;; Jump to beginning and parse headers
      (push (org-histogram--makeroot hasher) org-histogram--prntalist)
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
  (or (alist-get org-histogram--currentmode org-histogram-formatchoices)
      (progn (message "using backup format.")
             org-histogram--backupformat)))

(defun org-histogram--setoverlays (&optional event)
  "Set the overlays, and use optional EVENT data."
  (ignore event)
  (org-histogram--removeoverlays)
  (let ((lineform (org-histogram--getformatline)))
    (maphash
     (lambda (head info)
       (let ((bounds (plist-get info :bounds))
             (nlines (plist-get info :nlines))
             (percer (plist-get info :percent))
             (leveln (car head))
             (header (cdr head)))
         (if percer
             (let ((oface (intern (format "org-level-%s" leveln)))
                   (ovner (make-overlay (car bounds) (cdr bounds)))
                   (p-bar (cdr (--first (<= (caar it) (truncate percer)
                                            (cdar it))
                                        org-histogram-percentlevels))))
               (overlay-put ovner :org-histogram t)
               (overlay-put ovner 'face oface)
               (overlay-put ovner 'display
                            (format lineform p-bar percer
                                    nlines header))))))
     (org-histogram--processvisible))))


(defun org-histogram--printstats ()
  "Print stats, mostly debugging."
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



(defvar org-histogram--modebind
  (let ((map (make-sparse-keymap)))
    ;; Do not inherit from parent, which would
    ;; be read-only-mode.
    (define-key map (kbd ",") 'org-histogram-cycleusermodes-back)
    (define-key map (kbd ".") 'org-histogram-cycleusermodes-forw)
    (define-key map (kbd "return") 'org-histogram-mode)
    map)
  "Keymap for minor mode.")

(define-minor-mode org-histogram-mode
  "The mode for org-histogram."
  nil
  "ohm"
  org-histogram--modebind
  (if org-histogram-mode
      (progn (add-hook 'org-cycle-hook 'org-histogram--setoverlays)
             (read-only-mode t)
             (org-histogram--setoverlays))
    (remove-hook 'org-cycle-hook 'org-histogram--setoverlays)
    (org-histogram--removeoverlays)
    (read-only-mode -1)))

(provide 'org-histogram)
;;; org-histogram.el ends here
