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

(defvar-local org-treeusage-parse--hashmap nil)

(defun org-treeusage-parse--gethashmap (&optional regenerate)
  "Retrieve or generate hashmap.  If REGENERATE, then re-parse the buffer."
  (when (or regenerate
            (not org-treeusage-parse--hashmap))
    (message "Regenerating")
    (org-treeusage-parse--processvisible))
  org-treeusage-parse--hashmap)


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
          `(,head . (,beg . ,end)))))))


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
          ;; make key: level title
          (dkey (cons 0 nil)))
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

(defun org-treeusage-parse--processvisible ()
  "Parse the visible org headings in the current buffer, and collect information at each heading/node about number of lines, characters, and their percentages wrt the parent node.  The variable `org-treeusage-parse--hashmap' is updated to the hashmap generated here."
  (save-excursion
    (setq-local org-treeusage-parse--prntalist nil)
    (let ((hasher (make-hash-table :test 'equal))
          (prnt-curr nil)
          (prev-key nil))
      ;; Jump to beginning and parse headers
      (push (org-treeusage-parse--makeroot hasher)
            org-treeusage-parse--prntalist)
      ;;
      (while (let ((prevpnt (point)))
               ;; org-next-vis always returns nil, so
               ;; check point advancement manually.
               (progn (org-next-visible-heading 1)
                      (not (eq prevpnt (point)))))
        ;;
        (let* ((info (cadr (org-element-at-point)))
               (level (plist-get info :level))
               (bound (org-treeusage-parse--gettitlebounds info))
               (head (car bound))
               (hrng (cdr bound)))
          (when head
            ;; Check and update parent
            (setq prnt-curr (org-treeusage-parse--updateparents
                             level prev-key))
            ;; Why have I nested lets like this? Lack of trust.
            ;;
            ;; calc number of chars, lines, words
            (let ((posbeg (plist-get info :begin))
                  (posend (plist-get info :end)))
              (let ((dchar (- posend posbeg))
                    (dline (count-lines posbeg posend))
                    (dword (count-words posbeg posend))
                    ;; make key: level title
                    (elkey (cons level head)))
                ;;
                (puthash elkey ;; Make values: plist
                         (list :nlines dline :nchars dchar
                               :nwords dword :bounds hrng
                               :parentkey prnt-curr)
                         hasher)
                (setq prev-key elkey))))))
      (setq-local org-treeusage-parse--hashmap hasher))))

(provide 'org-treeusage-parse)
;;; org-treeusage-parse.el ends here
