;;; header3-defaults.el --- stores default values
;;
;; Filename: header3-defaults.el
;; Description: Launches header3 package and sets the default values
;; Author: Justine T Kizhakkinedath
;; Maintainer: Justine Thomas <justinethomas009@gmail.com>
;; Copyright (c) 2018, Justine T Kizhakkinedath
;; All rights reserved
;; Created: Fri 29 Jun 2018 00:04:07 IST
;; Version: 3.4.1
;; Package-Requires: ()
;; Last-Updated: Wed  4 Jul 2018 00:07:44 IST
;;           By: Justine T Kizhakkinedath
;;     Update #: 4
;; URL: https://github.com/justinethomas/header3
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;; -----------------------------------------------------------------------------
;;
;;; Commentary: This file contains all the default values for everything.
;;
;; -----------------------------------------------------------------------------
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; -----------------------------------------------------------------------------
;;
;;; Code:

;; Launch auto-update before saving the file

;;;###autoload
(eval-after-load 'header3 (progn
    (add-hook 'before-save-hook 'auto-update-file-header)
    (add-hook 'c++-mode-hook        (lambda () (auto-make-header "file-header")))
    ;; (add-hook 'c-mode-common-hook   (lambda () (auto-make-header "file-header")))
    (add-hook 'emacs-lisp-mode-hook (lambda () (auto-make-header "package-header")))
    (add-hook 'haskell-mode-hook    (lambda () (auto-make-header "file-header")))
    (add-hook 'python-mode-hook     (lambda () (auto-make-header "file-header")))
    (add-hook 'rust-mode-hook       (lambda () (auto-make-header "file-header")))
    (add-hook 'sh-mode-hook         (lambda () (auto-make-header "mini-header")))
    (add-hook 'markdown-mode-hook   'header-check-if-readme)
    ))

;;; header3-defaults.el ends here
