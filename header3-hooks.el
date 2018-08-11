;;; header3-hooks.el --- Keeps all the hooks in one place.
;;
;; Filename: header3-hooks.el
;; Description: Keeps all the hooks in one place.
;; Author: Justine Kizhakkinedath
;; Maintainer: Justine Kizhakkinedath <justine@kizhak.com>
;; Copyright (c) 2018, Justine Kizhakkinedath
;; All rights reserved
;; Created: Fri 29 Jun 2018 00:04:07 IST
;; Version: 3.4.5
;; Last-Updated: Sat 11 Aug 2018 22:00:09 IST
;;           By: Justine Kizhakkinedath
;;     Update #: 14
;; URL: https://gitlab.com/justinekizhak/header3
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;; -----------------------------------------------------------------------------
;;
;;; Commentary: This file keeps all the hooks in one place. Variables of the
;;  format "header-type-*" sets the type of header you want. The possible values
;;  are:
;;  1) mini-header
;;  2) file-header
;;  3) package-header
;;  Example:- (add-hook 'python-mode-hook (lambda () (auto-make-header header-type-python)))
;;  Here the hook "auto-make-header" is added to Python mode and "auto-make-header"
;;  takes the value from the variable header-type-python, which is set from the
;;  customize buffer.
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

;;;###autoload
(eval-after-load 'header3 (progn
    (add-hook 'before-save-hook 'auto-update-file-header)
    (add-hook 'c++-mode-hook        (lambda () (auto-make-header header-type-c++)))
    (add-hook 'emacs-lisp-mode-hook (lambda () (auto-make-header header-type-emacs-lisp)))
    (add-hook 'haskell-mode-hook    (lambda () (auto-make-header header-type-haskell)))
    (add-hook 'python-mode-hook     (lambda () (auto-make-header header-type-python)))
    (add-hook 'rust-mode-hook       (lambda () (auto-make-header header-type-rust)))
    (add-hook 'ruby-mode-hook       (lambda () (auto-make-header header-type-ruby)))
    (add-hook 'sh-mode-hook         (lambda () (auto-make-header header-type-shell-script)))
    (add-hook 'markdown-mode-hook   'header-check-if-readme)
    ))

;;; header3-defaults.el ends here
