;;; org-treeusage-parse.el --- Main parsing library for org -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://github.com/mtekman/org-treeusage.el
;; Keywords: outlines
;; Version: 0.3.1

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
(require 'org-element)

(defvar-local org-treeusage-parse--prntalist nil
  "List of (level . heading) parent nodes.
Popped from and pushed to, as the org file is parsed.")

(defvar-local org-treescope-parse--toupdate nil
  "List of (position . (level . heading)) elements.
If the hashmap is not regenerated, then only these overlays updated.")

(defvar-local org-treeusage-parse--hashmap nil)


(defun org-treeusage-parse--gethashmap (&optional reusemap)
  "Retrieve or generate hashmap.  If REUSEMAP is t, then parse the buffer with the existing hashmap but only update the entries.  If REUSEMAP is -1, then completely regenerate the hashmap.  A value of nil will return the map as it is."
  (let ((noexist (not org-treeusage-parse--hashmap)))
    (cond ((eq reusemap nil) org-treeusage-parse--hashmap)
          ((or (eq reusemap -1) noexist)
           (progn (message "Regenerating.")
                  (org-treeusage-parse--processvisible t)))
          (t (progn (message "Updating.")
                    (org-treeusage-parse--processvisible nil))))))


(defun org-treeusage-parse--gettitlebounds (info)
  "Get header title and the bounding positions from org element INFO."
  (let ((head (plist-get info :title))
        (bend (plist-get info :contents-begin))
        (bbeg (line-beginning-position)))
    (when head
      (save-excursion
        (goto-char bbeg) ;; important
        (let* ((end (or (search-forward-regexp
                         (shell-quote-argument head) bend t)
                        ;; search using regexp, failing that use normal
                        (search-forward head bend)))
               (beg (progn (search-backward-regexp "^\\*+ " bbeg)
                           (match-end 0))))
          (cons head (cons beg end)))))))


(defun org-treeusage-parse--makeroot (hashmap)
  "Generate the initial root parent node by getting the full bounds of the whole org file and inserting them into the HASHMAP."
  (let* ((pend (progn (goto-char (point-max))
                      (org-backward-sentence)
                      (point)))
         (pbeg (progn (goto-char 0)
                      (org-next-visible-heading 1)
                      (point))))
    (let ((dchar (- pend pbeg))
          (dline (count-lines pbeg pend))
          (dword (count-words pbeg pend))
          (dkey (cons 0 nil))) ;; make key: level title
      (move-beginning-of-line 0)
      (puthash dkey
               (list :nlines dline :nchars dchar :nwords dword)
               hashmap)
      dkey)))


(defun org-treeusage-parse--updateparents (lvl-now previousk)
  "Get the parent of the current node at LVL-NOW, and update the parent if the current node deviates from the previous node PREVIOUSK."
  (let ((prev-lvl (car previousk))
        (prev-hdr (cdr previousk))
        (curr-parent (car org-treeusage-parse--prntalist)))
    (cond ((not prev-lvl)
           curr-parent)
          ((> lvl-now prev-lvl)
           ;; Gone N level's deep, push the last
           ;; heading as the new parent at level
           (car (push (cons prev-lvl prev-hdr)  ;; make key: level title
                      org-treeusage-parse--prntalist)))
          ;;
          ((< lvl-now prev-lvl)
           ;; Returned to a level up. Pop all levels up to.
           (while (>= (caar org-treeusage-parse--prntalist) lvl-now)
             (pop org-treeusage-parse--prntalist))
           (car org-treeusage-parse--prntalist))
          (t curr-parent))))

(defun org-treeusage-parse--processvisible (&optional regenerate)
  "Parse the visible org headings in the current buffer, and collect\
information at each heading/node about number of lines, characters, and\
their percentages with respect to the parent node.
The variable `org-treeusage-parse--hashmap' is updated to the hashmap\
 generated here.  If REGENERATE, clear the hashtable and do not re-use it."
  (save-excursion
    (setq-local org-treeusage-parse--prntalist nil)
    (let ((hasher (if regenerate (make-hash-table :test 'equal)
                    (org-treeusage-parse--gethashmap regenerate)))
          (calcperc (lambda (c p) (/ (float (* 100 c)) p)))
          (prnt-curr nil) (prev-key nil)
          (overlays-from-last
           (unless regenerate org-treescope-parse--toupdate))
          (overlays-to-update nil))
      (push (org-treeusage-parse--makeroot hasher)  ;; Jump to beginning
            org-treeusage-parse--prntalist)         ;; and parse headers
      (while (let ((prevpnt (point)))
               (progn (org-next-visible-heading 1)  ;; org-next-vis always
                      (not (eq prevpnt (point)))))  ;; returns nil
        ;; If this position already has an overlay, and we're not
        ;; regenerating, skip it and set the prev-key.
        (let ((key-at-pos (alist-get (point) overlays-from-last)))
          (if key-at-pos
              (setq prev-key key-at-pos)
            ;; No key, parse the region
            (let* ((info (cadr (org-element-at-point)))
                   (level (plist-get info :level))
                   (bound (org-treeusage-parse--gettitlebounds info))
                   (head (car bound)) (hrng (cdr bound))
                   (elkey (cons level head))
                   (elhash (gethash elkey hasher)))
              (when (and head (not elhash))
                ;; Set parent, regardless of whether if it already exists.
                (setq prnt-curr (org-treeusage-parse--updateparents
                                 level prev-key))
                (let* ((parent (gethash prnt-curr hasher))
                       (posbeg (plist-get info :begin))
                       (posend (plist-get info :end))
                       (dchar (- posend posbeg))
                       (dline (count-lines posbeg posend))
                       (dword (count-words posbeg posend)))
                  (let ((pchar (funcall calcperc dchar
                                        (plist-get parent :nchars)))
                        (pline (funcall calcperc dline
                                        (plist-get parent :nlines)))
                        (pword (funcall calcperc dword
                                        (plist-get parent :nwords))))
                    ;; Store positions and keys updated
                    (push (cons (car hrng) elkey) overlays-to-update)
                    (puthash elkey ;; Make values: plist
                             (list :nlines dline :nchars dchar :nwords dword
                                   :plines pline :pchars pchar :pwords pword
                                   :bounds hrng)
                             hasher))))
              (setq prev-key elkey)))))
      (setq-local org-treescope-parse--toupdate overlays-to-update)
      (setq-local org-treeusage-parse--hashmap hasher))))

(provide 'org-treeusage-parse)
;;; org-treeusage-parse.el ends here
