

;; (setq tree
;;       (with-current-buffer "lorum.org"
;;         (org-element-parse-buffer)))


;; (setq tmp
;;       (org-element-map tree 'headline
;;         (lambda (headline) headline)
;;         nil t))


(defun calculate-numlines (info)
  ""
  (- (line-number-at-pos (plist-get info :end))
     (line-number-at-pos (plist-get info :begin))))


(defun process-visible ()
  "The idea is to get stats only for the visible portions of the buffer.
To investigate further, expand a heading."
  (let ((hasher (make-hash-table :test 'equal)))
    (save-excursion
      ;; Jump to end and get root bounds
      (let ((dline (- (progn (end-of-buffer)
                             (org-backward-sentence)
                             (line-number-at-pos (point)))
                      (progn (goto-char 0)
                             (org-next-visible-heading 1)
                             (line-number-at-pos (point)))))
            (dkey (cons 0 nil)))
        ;;
        (puthash dkey (list :nlines dline :percent nil) hasher)
        ;;
        ;; Jump to beginning and parse headers
        (goto-char 0)
        (let ((header t)
              (current-parent dkey)
              (parent-alist nil)
              (prev-level 0)
              (prev-header nil))
          (while header
            (org-next-visible-heading 1)
            (let* ((info (cadr (org-element-at-point)))
                   (head (plist-get info :title))
                   (level (plist-get info :level)))
              (setq header head)
              (when head
                ;; -- Check parent
                (setq current-parent
                      (cond ((> level prev-level)
                             ;; Gone N level's deep, push the last
                             ;; heading as the new parent at level
                             (car (push (cons prev-level prev-header)
                                   parent-alist)))
                            ;;
                            ((< level prev-level)
                             ;; Returned to a level up.
                             ;; -- pop all levels in between
                             (while (>= (caar parent-alist) level)
                               (pop parent-alist))
                             (pop parent-alist))
                            (t current-parent)))
                ;;
                ;; -- Progress under assumption of correct root
                ;;
                (let* ((dlin (calculate-numlines info))
                       (key-element (cons level head))
                       (parent-info (gethash current-parent hasher))
                       (parent-lnum (plist-get parent-info :nlines))
                       (percent (/ (float (* 100 dlin)) parent-lnum)))
                  ;;
                  (puthash key-element (list :nlines dlin :percent percent)
                           hasher)
                  (setq prev-level level
                        prev-header head))))))))
        hasher))


(setq tmp
      (let ((res nil))
        (maphash
         (lambda (k info)
           (push (cons (cdr k) (plist-get info :percent)) res))
         (with-current-buffer "projects.org"
           (process-visible)))
        res))


(dolist (var (cdr (reverse tmp)))
  (insert (format "\n |%s | %2.1f |" (car var) (cdr var))))
 |Galaxy | 26.7 |
 |Projects | 19.7 |
 |Talks and Conferences | 1.4 |
 |GCC | 38.1 |
 |GCB | 24.5 |
 |Bled | 7.9 |
 |Book train tickets | 27.3 |
 |Submit Dienstreiseantrag to Monika | 18.2 |
 |Pay the registration fee | 36.4 |
 |Give to Monika confirmation of payment | 18.2 |
 |Recieve Compensation | 27.3 |
 |asdad | 66.7 |
 |Single Cell Journal Club | 6.5 |
 |MeInBio | 25.2 |
 |Teaching | 12.1 |
 |Recreation | 1.0 |
 |Productivity | 6.8 |
 |Papers | 12.0 |
 |Home Projects | 8.7 |
 |Life | 12.0 |

|Galaxy | 26.7 |
|Projects | 19.7 |
|Talks and Conferences | 1.4 |
   |GCC | 37.7 |
   |GCB | 23.9 |
   |Bled | 7.2 |
     |Book train tickets | 20.0 |
     |Submit Dienstreiseantrag to Monika | 10.0 |
     |Pay the registration fee | 30.0 |
     |Give to Monika confirmation of payment | 10.0 |
     |Recieve Compensation | 20.0 |
       |asdad | 50.0 |
   |Single Cell Journal Club | 5.8 |
   |MeInBio | 24.6 |
|Teaching | 12.1 |
|Recreation | 1.0 |
|Productivity | 6.8 |
|Papers | 12.0 |
|Home Projects | 8.7 |
|Life | 12.0 |

