

(defvar prnt-curr nil)
(defvar prnt-alist nil)
(defvar prev-key nil)


(defun make-key (level header) (cons level header))
(defun make-values (line perc) (list :nlines line :percent perc))
(defun get-lineinf (info keyf) (line-number-at-pos (plist-get info keyf)))
(defun calc-percentage (child par) (/ (float (* 100 child)) par))
(defun calc-nlines (info) (- (get-lineinf info :end)
                             (get-lineinf info :begin)))
(defun next-match (point1) (progn (org-next-visible-heading 1)
                                  (not (eq point1 (point)))))

(defun setroot ()
  "Perform once.
Jump to end and beginning to get full bounds."
  (let ((dline (- (progn (end-of-buffer)
                         (org-backward-sentence)
                         (line-number-at-pos (point)))
                  (progn (goto-char 0)
                         (org-next-visible-heading 1)
                         (line-number-at-pos (point)))))
        (dkey (make-key 0 nil)))
    ;;
    (puthash dkey (make-values dline nil) hasher)
    (push dkey prnt-alist)))

(defun get-parent (lvl-now)
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
      ;;
      (while (next-match (point))
        (let* ((info (cadr (org-element-at-point)))
               (head (plist-get info :title))
               (level (plist-get info :level)))
          (when head
            ;; Check and update parent
            (setq prnt-curr (get-parent level))
            ;;
            (let ((dline (calc-nlines info))
                  (elkey (make-key level head))
                  (_prnt-inf (gethash prnt-curr hasher)))
              (let ((percent (calc-percentage
                              dline
                              (plist-get _prnt-inf :nlines))))
                (puthash elkey (make-values dline percent) hasher)
                (setq prev-key elkey)))))))
    hasher))


(setq tmp (with-current-buffer "lorum.org" (process-visible)))

(defun print-stats ()
  "Released."
  (maphash
   (lambda (levhead linperc)
     (let ((indent (make-string (* 4 (car levhead)) ? ))
           (header (or (cdr levhead) "-root-"))
           (nlines (or (plist-get linperc :nlines) 0))
           (percnt (or (plist-get linperc :percent) 0)))
       (insert
        (format "\n;;%s[%4d,%3.0f] -- %s "
                indent nlines percnt header))))
   (with-current-buffer "lorum.org" (process-visible))))



;;[9770,  0] -- -root- 
;;    [2613, 27] -- Galaxy 
;;    [1928, 20] -- Projects 
;;    [ 137,  1] -- Talks and Conferences 
;;    [1186, 12] -- Teaching 
;;        [ 740, 62] -- 2019 WS 
;;        [ 429, 36] -- Students / HiWis 
;;        [  16,  1] -- Other 
;;    [  97,  1] -- Recreation 
;;    [ 661,  7] -- Productivity 
;;    [1179, 12] -- Papers 
;;        [ 535, 45] -- Reviews 
;;            [ 367, 69] -- BIOINF-2018-2481 
;;            [ 126, 24] -- GigaScience Galaxy scRNA paper 
;;            [   3,  1] -- JESTCH_2019_1343 
;;            [  18,  3] -- [12/12] 2019 Feb - Omer 
;;            [  20,  4] -- [8/8] 2019 June - Viktor and Omer 
;;        [ 562, 48] -- Read 
;;        [  81,  7] -- Write 
;;    [ 848,  9] -- Home Projects 
;;    [1140, 12] -- Life 
