;; counsel-recentp --- An counsel helper to list you recently used git repositories

;; Copyright (C) 2018 José Gutiérrez de la Concha

;; Author: José Gutiérrez de la Concha <jose@zeroc.com>
;; URL: https://github.com/pepone/counsel-recentp
;; Version: 0.1
;; Package-Version: 0.1
;; Package-Requires: ((emacs "24.3") (counsel "0.9.0") (magit "2.12.1"))
;; Keywords: matching

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this package.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The `counsel-recentp' function allows you to list your recently
;; used git projects and jump to the corresponding magit-status
;; buffer.
;;
;; Please see README.md from the same repository for documentation.

;;; Code:
(defun counsel-recentp-git-repository-p (d)
  "True if D contain a .git repository."
  (file-exists-p (concat d "/.git/config")))

(defun counsel-recentp-find-git-repository (d)
  "Find out if D belongs to a git repository."
  (if (equal (file-name-directory (directory-file-name d)) d)
      nil
    (if (counsel-recentp-git-repository-p d)
        d
      (counsel-recentp-find-git-repository (file-name-directory (directory-file-name d))))))

(defvar recentf-list)

;;;###autoload
(defun counsel-recentp ()
  "Find git repositories on `recentf-list'."
  (interactive)
  (require 'recentf)
  (require 'ivy)
  (require 'magit)
  (recentf-mode)
  (ivy-read "Recentp: " (delq nil (delete-dups (mapcar #'counsel-recentp-find-git-repository recentf-list)))
            :action (lambda (f)
                      (with-ivy-window
                          (let ((magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1))
                            (magit-status-internal f))))
            :caller 'counsel-recentp))

(provide 'counsel-recentp)

;;; counsel-recentp.el ends here
