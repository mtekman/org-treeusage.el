;;; org-density.el --- Examine the density of org headings -*- lexical-binding: t; -*-

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

;; A minor mode to show the line or word density of org-mode files.
;; The main motivation was to help in the archiving and arrangement
;; of very large org files that might have some redundant data still
;; in it.

;;; Code:
(require 'org-element)
(require 'dash)

(defvar org-density--prntalist nil)
(defvar org-density--currentmode 'bar)

(defgroup org-density nil
  "Customization group for org-density."
  :group 'org)

(defcustom org-density-percentlevels
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
  :group 'org-density)


(defvar org-density--backupformat "%1$-5s--%3$d"
  "Fallback in case an invalid format is chosen by the user.")

(defcustom org-density-formatchoices
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

(defun org-density--cycleusermodes (forw)
  "Cycle a user defined list of formats in direction FORW."
  (let ((oh-cm org-density--currentmode)
        (oh-fm (mapcar 'car org-density-formatchoices))
        (direc (if forw 1 -1)))
    (let* ((curr-index (position oh-cm oh-fm))
           (next-index (mod (+ curr-index direc) (length oh-fm)))
           (next-umode (nth next-index oh-fm)))
      (setq org-density--currentmode next-umode)
      (org-density--setoverlays)
      (message "Mode: %s" next-umode))))

(defun org-density-cycleusermodes-forw ()
  "Cycle user modes forwards."
  (interactive)
  (org-density--cycleusermodes t))

(defun org-density-cycleusermodes-back ()
  "Cycle user modes backwards."
  (interactive)
  (org-density--cycleusermodes nil))

(defun org-density--makekey (level header)
  "Generate key for hash using LEVEL and HEADER."
  (cons level header))

(defun org-density--makevalues (line chars
                                     &optional pline pchars bounds)
  "Create a plist from from LINE and CHARS.
Use their percentages PLINE and PCHARS, and their BOUNDS."
  (list :nlines line :nchars chars
        :plines pline :pchars pchars
        :bounds bounds))

(defun org-density--getlineinf (info keyf)
  "Get line number for specific INFO keyword KEYF."
  (line-number-at-pos (plist-get info keyf)))

(defun org-density--calcperc (child par)
  "Calculate percentage of CHILD against PAR.
Percentages are not super accurate, but are a good gauge."
  (/ (float (* 100 child)) par))

(defun org-density--calcnlines (info)
  "Calculate the number of lines from INFO."
  (- (org-density--getlineinf info :end)
     (org-density--getlineinf info :begin)))

(defun org-density--calcnchars (info)
  "Calculate the number of chracters from INFO."
  (- (plist-get info :end)
     (plist-get info :begin)))

(defun org-density--nextmatch (point1)
  "Get next match from current POINT1."
  (progn (org-next-visible-heading 1)
         (not (eq point1 (point)))))

(defun org-density--makeroot (hasher)
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
          (dkey (org-density--makekey 0 nil)))
      (move-beginning-of-line 0)
      (puthash dkey
               (org-density--makevalues dline dchar)
               hasher)
      dkey)))

(defsubst org-density--searchforward (header endmark)
  "Search forward for HEADER up to ENDMARK.
At first use shell argument, or failing that, without."
  (let ((res (search-forward-regexp
              (shell-quote-argument header)
              endmark t)))
    (or res (search-forward-regexp header endmark))))

(defun org-density--gettitlebounds (info)
  "Get title and bounds from INFO."
  (let ((head (plist-get info :title))
        (bend (plist-get info :contents-begin))
        (bbeg (line-beginning-position)))
    (when head
      (save-excursion
        (goto-char bbeg) ;; important
        (let* ((end (org-density--searchforward head bend))
               (beg (+ 2 (search-backward "* " bbeg))))
          `(,head . (,beg . ,end)))))))


(defun org-density--updateparents (lvl-now previousk)
  "Get or update parent into PRNT-ALIST based on PREVIOUSK.
From current level LVL-NOW."
  (let ((prev-lvl (car previousk))
        (prev-hdr (cdr previousk))
        (curr-parent (car org-density--prntalist)))
    (cond ((not prev-lvl)
           curr-parent)
          ((> lvl-now prev-lvl)
           ;; Gone N level's deep, push the last
           ;; heading as the new parent at level
           (car (push
                 (org-density--makekey prev-lvl prev-hdr)
                 org-density--prntalist)))
          ;;
          ((< lvl-now prev-lvl)
           ;; Returned to a level up. Pop all levels up to.
           (while (>= (caar org-density--prntalist) lvl-now)
             (pop org-density--prntalist))
           (car org-density--prntalist))
          (t curr-parent))))

(defun org-density--processvisible ()
  "The idea is to get stats only for the visible portions of the buffer.
To investigate further, expand a heading."
  (save-excursion
    (setq org-density--prntalist nil)
    (let ((hasher (make-hash-table :test 'equal))
          (prnt-curr nil)
          (prev-key nil))
      ;; Jump to beginning and parse headers
      (push (org-density--makeroot hasher) org-density--prntalist)
      ;;
      (while (org-density--nextmatch (point))
        (let ((info (cadr (org-element-at-point))))
          (let ((level (plist-get info :level))
                (bound (org-density--gettitlebounds info)))
            (let ((head (car bound))
                  (hrng (cdr bound)))
              (when head
                ;; Check and update parent
                (setq prnt-curr (org-density--updateparents
                                 level prev-key))
                ;;
                (let ((dline (org-density--calcnlines info))
                      (dchar (org-density--calcnchars info))
                      (elkey (org-density--makekey level
                                                     head))
                      (prnt-inf (gethash prnt-curr hasher)))
                  (let ((percline (org-density--calcperc
                                   dline
                                   (plist-get prnt-inf :nlines)))
                        (percchar (org-density--calcperc
                                   dchar
                                   (plist-get prnt-inf :nchars))))
                    (puthash elkey
                             (org-density--makevalues dline
                                                      dchar
                                                      percline
                                                      percchar
                                                      hrng)
                             hasher)
                    (setq prev-key elkey))))))))
      hasher)))


(defun org-density--removeoverlays ()
  "Remove all overlays."
  (let ((ovs (overlays-in (point-min) (point-max))))
    (if (cl-loop for ov in ovs
                 thereis (overlay-get ov :org-density))
        (dolist (ov ovs)
          (when (overlay-get ov :org-density)
            (delete-overlay ov))))))


(defun org-density--getformatline ()
  "Get format line, if custom, then use custom format string."
  (or (alist-get org-density--currentmode org-density-formatchoices)
      (progn (message "using backup format.")
             org-density--backupformat)))


(defvar org-density--difftype 'lines
  "Type is strictly either 'lines or 'chars.")

(defun org-density-toggledifftype ()
  "Toggle the difference mode from characters to lines."
  (interactive)
  (let* ((cmode org-density--difftype)
         (nmode (if (eq cmode 'lines) 'chars 'lines)))
    (setq org-density--difftype nmode)
    (org-density--setoverlays)
    (message "Type: %s" nmode)))

(defun org-density--setoverlays (&optional event)
  "Set the overlays, and use optional EVENT data."
  (ignore event)
  (org-density--removeoverlays)
  (let ((lineform (org-density--getformatline))
        (ntype (intern (format ":n%s" org-density--difftype)))
        (ptype (intern (format ":p%s" org-density--difftype))))
    (maphash
     (lambda (head info)
       (let ((bounds (plist-get info :bounds))
             (ndiffs (plist-get info ntype))
             (percer (plist-get info ptype))
             (leveln (car head))
             (header (cdr head)))
         ;; Have to choose either characters or lines at this
         ;; point to get the correct bar.
         (if percer
             (let ((oface (intern (format "org-level-%s" leveln)))
                   (ovner (make-overlay (car bounds) (cdr bounds)))
                   (barpc (cdr (--first (<= (caar it) (truncate percer)
                                            (cdar it))
                                        org-density-percentlevels))))
               (overlay-put ovner :org-density t)
               (overlay-put ovner 'face oface)
               (overlay-put ovner 'display
                            (format lineform barpc percer
                                    ndiffs header))))))
     (org-density--processvisible))))


(defun org-density--printstats ()
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
   (with-current-buffer "lorum.org" (org-density--processvisible))))



(defvar org-density--modebind
  (let ((map (make-sparse-keymap)))
    ;; Do not inherit from parent, which would
    ;; be read-only-mode.
    (define-key map (kbd ",") 'org-density-cycleusermodes-back)
    (define-key map (kbd ".") 'org-density-cycleusermodes-forw)
    (define-key map (kbd "l") 'org-density-toggledifftype)
    (define-key map (kbd "return") 'org-density-mode)
    map)
  "Keymap for minor mode.")

(define-minor-mode org-density-mode
  "The mode for org-density."
  nil
  "ohm"
  org-density--modebind
  (if org-density-mode
      (progn (add-hook 'org-cycle-hook 'org-density--setoverlays)
             (read-only-mode t)
             (org-density--setoverlays))
    (remove-hook 'org-cycle-hook 'org-density--setoverlays)
    (org-density--removeoverlays)
    (read-only-mode -1)))

(provide 'org-density)
;;; org-density.el ends here
