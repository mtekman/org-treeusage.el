;;; org-density-parse.el --- Main parsing library for org -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://github.com/mtekman/org-density.el
;; Keywords: outlines
;; Package-Requires: ((emacs "26.1") (org "9.1.6"))
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

;; See org-density.el

;;; Code:
(require 'org-element)

(defvar org-density-parse--prntalist nil
  "List of (level . heading) parent nodes.
Popped from and pushed to as the org file is parsed.")

(defvar org-density-parse--hashmap nil)

(defun org-density-parse--gethashmap (&optional regenerate)
  "Retrieve or generate hashmap.  If REGENERATE, then re-parse."
  (when (or regenerate
            (not org-density-parse--hashmap))
    (message "Regenerating")
    (org-density-parse--processvisible))
  org-density-parse--hashmap)


(defun org-density-parse--gettitlebounds (info)
  "Get title and bounds from INFO."
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


(defun org-density-parse--makeroot (hashmap)
  "Make the root (level . heading) node, by getting the full bounds of the whole org file, and insert into the HASHMAP."
  (let* ((pend (progn (goto-char (point-max))
                      (org-backward-sentence)
                      (point)))
         (pbeg (progn (goto-char 0)
                      (org-next-visible-heading 1)
                      (point)))
         (dchar (- pend pbeg))
         (dline (- (line-number-at-pos pend)
                   (line-number-at-pos pbeg)))
         ;; make key: level title
         (dkey (cons 0 nil)))
    (move-beginning-of-line 0)
    (puthash dkey
             ;; Make values: plist
             (list :nlines dline :nchars dchar)
             hashmap)
    dkey))


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
           (car (push (cons prev-lvl prev-hdr)  ;; make key: level title
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
To investigate further, expand a heading.  Updates `org-density--hashmap'."
  (save-excursion
    (setq org-density-parse--prntalist nil)
    (let ((hasher (make-hash-table :test 'equal))
          (prnt-curr nil)
          (prev-key nil))
      ;; Jump to beginning and parse headers
      (push (org-density-parse--makeroot hasher) org-density-parse--prntalist)
      ;;
      (while (let ((prevpnt (point)))
               ;; org-next-vis always returns nil, so
               ;; check point advancement manually.
               (progn (org-next-visible-heading 1)
                      (not (eq prevpnt (point)))))
        ;;
        (let* ((info (cadr (org-element-at-point)))
               (level (plist-get info :level))
               (bound (org-density-parse--gettitlebounds info))
               (head (car bound))
               (hrng (cdr bound)))
          (when head
            ;; Check and update parent
            (setq prnt-curr (org-density-parse--updateparents
                             level prev-key))
            (let ((dchar ;; -- calc number of chars
                   (- (plist-get info :end)
                      (plist-get info :begin)))
                  (dline ;; -- calc number of lines
                   (- (line-number-at-pos (plist-get info :end))
                      (line-number-at-pos (plist-get info :begin))))
                  ;;
                  (elkey (cons level head)) ;; make key: level title
                  (prnt-inf (gethash prnt-curr hasher))
                  (calcperc (lambda (c p)(/ (float (* 100 c)) p))))
              ;;
              ;; -- Calculate line and character percentages
              (let ((lperc (funcall calcperc dline
                                    (plist-get prnt-inf :nlines)))
                    (cperc (funcall calcperc dchar
                                    (plist-get prnt-inf :nchars))))
                (puthash elkey
                         ;; Make values: plist
                         (list :nlines dline :nchars dchar
                               :plines lperc :pchars cperc
                               :bounds hrng)
                         hasher)
                (setq prev-key elkey))))))
      (setq org-density-parse--hashmap hasher))))


(provide 'org-density-parse)
;;; org-density-parse.el ends here
