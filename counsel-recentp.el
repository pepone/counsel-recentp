;;; counsel-recentp.el --- List recently used git repositories with counsel -*- lexical-binding: t; -*-

;; Copyright (C) 2018 José Gutiérrez de la Concha

;; Author: José Gutiérrez de la Concha <jose@zeroc.com>

;; Version: 0.1
;; Package-Version: 0.1
;; Package-Requires: ((emacs "24.3") (counsel "0.9.0") (magit "2.12.1"))
;; Keywords: matching
;; Homepage: https://github.com/pepone/counsel-recentp

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

(require 'recentf)
(require 'ivy)
(require 'magit)

(defun counsel-recentp-find-git-repository (directory)
  "Return the git repository containing DIRECTORY, nil if none."
  (ignore-errors
      (locate-dominating-file directory #'magit-git-repo-p)))

;;;###autoload
(defun counsel-recentp ()
  "Find git repositories on `recentf-list'."
  (interactive)
  (recentf-mode)
  (ivy-read "Recentp: " (delq nil (delete-dups (mapcar #'counsel-recentp-find-git-repository recentf-list)))
            :action (lambda (f)
                      (with-ivy-window
                          (let ((magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1))
                            (magit-status-internal f))))
            :caller 'counsel-recentp))

(provide 'counsel-recentp)

;;; counsel-recentp.el ends here
