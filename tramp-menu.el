;;; tramp-menu.el --- Connect to SSH quickly and effortlessly -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Jakub Kadlčík

;; Author: Jakub Kadlčík <frostyx@email.cz>
;; URL: https://github.com/FrostyX/tramp-menu
;; Version: 1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tramp

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package shows completion with all known SSH hosts and connects to the
;; selected one through TRAMP. The advantage over the built-in Emacs
;; functionality is that you don't have to `C-x C-f' which Evil users are not
;; used to, and you don't have to remember the remote file syntax:
;;     `/ssh:user@host:/path/to/file'


;;; Code:

;;;; Requirements

(defgroup tramp-menu nil
  "Tramp completion menu."
  :prefix "tramp-menu-"
  :group 'tramp)

;;;; Customization

(defcustom tramp-menu-shell
  #'shell
  "What shell should be opened after a successfull connection."
  :type 'function
  :group 'tramp-menu)

;;;; Commands

(defun tramp-menu ()
  (interactive)
  (let* ((hosts (tramp-menu-hosts))
         (host (tramp-menu--completing-read hosts))
         (default-directory (format "/ssh:%s:" host)))
    (funcall tramp-menu-shell)))

;;;; Functions

;;;;; Public

;;;;; Private

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

;;;; Footer

(provide 'tramp-menu)

;;; tramp-menu.el ends here
