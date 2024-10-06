(defgroup tramp-menu nil
  "Tramp completion menu."
  :prefix "tramp-menu-"
  :group 'tramp)

(defcustom tramp-menu-shell
  #'shell
  "What shell should be opened after a successfull connection."
  :type 'function
  :group 'tramp-menu)

(defun tramp-menu ()
  (interactive)
  (let* ((hosts (tramp-menu-hosts))
         (host (tramp-menu--completing-read hosts))
         (default-directory (format "/ssh:%s:" host)))
    (funcall tramp-menu-shell)))

(defun tramp-menu-known-hosts ()
  (with-current-buffer
      (find-file-noselect "~/.ssh/known_hosts")
    (split-string
     (save-restriction
       (widen)
       (buffer-substring-no-properties
        (point-min)
        (point-max)))
     "\n" t)))

(defun tramp-menu-hosts ()
  (flatten-tree
   (mapcar
    (lambda (x)
      (unless (string-prefix-p "#" x)
        (split-string (first (string-split x)) ",")))
    (tramp-menu-known-hosts))))

(defun tramp-menu--completing-read (hosts)
  (let* ((items hosts)
         (max-length (apply #'max (mapcar (lambda (x) (length x)) items)))
         (completion-extra-properties
          `(:annotation-function
            ,(lambda (name)
               (let ((annotation ""))
                 (concat
                  (make-string (- (+ max-length 2) (length name)) ?\s)
                  annotation)))))
         (value (completing-read "Tramp: " items)))
    value))
