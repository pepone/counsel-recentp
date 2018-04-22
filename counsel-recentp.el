;; counsel-recentp --- An counsel helper to list you recently used git repositories

;; Copyright (C) 2018 José Gutiérrez de la Concha

;; Author: José Gutiérrez de la Concha <jose@zeroc.com>
;; Maintainer: José Gutiérrez de la Concha <jose@zeroc.com>
;; Created: 21 Apr 2018
;; Modified: 21 Apr 2018
;; Version: 0.1
;; Package-Requires: ((emacs "24.3") (counsel "0.9") (magit "2.12.1"))
;; Keywords: magit counsel projects
;; URL: https://github.com/pepone/counsel-recentp

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

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
