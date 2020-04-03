

(defvar prnt-curr nil)
(defvar prnt-alist nil)
(defvar prev-key nil)


(defun make-key (level header)
  "Generate key for hash using LEVEL and HEADER."
  (cons level header))

(defun make-values (line perc bounds)
  "Create a value from LINE and PERC BOUNDS set"
  (list :nlines line :percent perc :bounds bounds))

(defun get-lineinf (info keyf)
  "Get line number for specific INFO keyword KEYF."
  (line-number-at-pos (plist-get info keyf)))

(defun calc-percentage (child par)
  "Calculate percentage of CHILD against PAR.
Percentages are not super accurate, but are a good gauge."
  (/ (float (* 100 child)) par))

(defun calc-nlines (info)
  "Calculate the number of lines from INFO."
  (- (get-lineinf info :end)
     (get-lineinf info :begin)))

(defun next-match (point1)
  "Get next match from current POINT1."
  (progn (org-next-visible-heading 1)
         (not (eq point1 (point)))))

(defun setroot ()
  "Perform once. Get full bounds."
  (let ((dline (- (progn (end-of-buffer)
                         (org-backward-sentence)
                         (line-number-at-pos (point)))
                  (progn (goto-char 0)
                         (org-next-visible-heading 1)
                         (line-number-at-pos (point)))))
        (dkey (make-key 0 nil)))
    (puthash dkey (make-values dline nil nil) hasher)
    (push dkey prnt-alist)))

(defun update-parent (lvl-now)
  "Get or update parent from current level LVL-NOW."
  (let ((prev-lvl (car prev-key))
        (prev-hdr (cdr prev-key))
        (curr-parent (car prnt-alist)))
    (cond ((not prev-lvl)
           curr-parent)
          ((> lvl-now prev-lvl)
           ;; Gone N level's deep, push the last
           ;; heading as the new parent at level
           (car (push (make-key prev-lvl prev-hdr) prnt-alist)))
          ;;
          ((< lvl-now prev-lvl)
           ;; Returned to a level up. Pop all levels up to.
           (while (>= (caar prnt-alist) lvl-now)
             (pop prnt-alist))
           (car prnt-alist))
          (t curr-parent))))

(defun get-title-bounds (info)
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
               (beg (match-beginning 0)))
          `(,head . (,beg . ,end)))))))


(defun process-visible ()
  "The idea is to get stats only for the visible portions of the buffer.
To investigate further, expand a heading."
  (let ((hasher (make-hash-table :test 'equal)))
    (setq prnt-curr nil
          prnt-alist nil
          prev-key nil)
    (save-excursion
      (setroot)
      ;; Jump to beginning and parse headers
      (goto-char 0)
      (while (next-match (point))
        (let ((info (cadr (org-element-at-point))))
          (let ((level (plist-get info :level))
                (_bound (get-title-bounds info)))
            (let ((head (car _bound))
                  (hrng (cdr _bound)))
              (when head
                ;; Check and update parent
                (setq prnt-curr (update-parent level))
                ;;
                (let ((dline (calc-nlines info))
                      (elkey (make-key level head))
                      (_prnt-inf (gethash prnt-curr hasher)))
                  (let ((percent (calc-percentage
                                  dline
                                  (plist-get _prnt-inf :nlines))))
                    (puthash elkey
                             (make-values dline percent hrng)
                             hasher)
                    (setq prev-key elkey)))))))))
    hasher))


(defun set-overlays (hashtable)
  "Set the overlays from HASHTABLE."
  (maphash
   (lambda (head info)
     (let ((bounds (plist-get info :bounds))
           (nlines (plist-get info :nlines))
           (percent (plist-get info :percent)))
       (if percent
           (let ((ov (make-overlay (car bounds) (cdr bounds))))
             (overlay-put ov :org-histogram t)
             (overlay-put ov 'display (format "%2.1f%% {%d}"
                                              percent nlines))))))
   hashtable))

(defun remove-overlays ()
  ;; Scraped from alphapapa's behind
  (let ((ovs (overlays-in (point-min) (point-max))))
    (if (cl-loop for ov in ovs
                 thereis (overlay-get ov :org-histogram))
        (dolist (ov ovs)
          (when (overlay-get ov :org-histogram)
            (delete-overlay ov))))))



(defun print-stats ()
  "Released."
  (maphash
   (lambda (head info)
     (let ((indent (make-string (* 4 (car head)) ? ))
           (header (or (cdr head) "{root}"))
           (nlines (or (plist-get info :nlines) 0))
           (percnt (or (plist-get info :percent) 100)))
       (insert
        (format "\n;;%s %3.0f -- %s {%d}"
                indent percnt header nlines))))
   (with-current-buffer "lorum.org" (process-visible))))


;; 100 -- {root} {9770}
;;      27 -- Galaxy {2613}
;;          41 -- Tools {1072}
;;              82 -- Single-Cell Related {876}
;;               0 -- Linkage Related {4}
;;              18 -- Unrelated {191}
;;                   4 -- [2/2] GTF2Bed12 {7}
;;                   3 -- [1/2] ggplot2 {6}
;;                  38 -- [1/1] UMItools {72}
;;                  14 -- [1/2] Interactive Notebooks {26}
;;                  41 -- [6/6] GraphEmbed {79}
;;          15 -- Trainings {392}
;;           4 -- Datatypes {92}
;;           1 -- Workflows {21}
;;           1 -- Workflow Testing {28}
;;          29 -- Workshops {745}
;;           9 -- [6/6] User Requests {247}
;;           1 -- Reviews {15}
;;      20 -- Projects {1928}
;;       1 -- Talks and Conferences {137}
;;      12 -- Teaching {1186}
;;       1 -- Recreation {97}
;;       7 -- Productivity {661}
;;      12 -- Papers {1179}
;;       9 -- Home Projects {848}
;;      12 -- Life {1140}
