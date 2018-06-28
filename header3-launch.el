;; -----------------------------------------------------------------------------
;; Created: Fri 29 Jun 2018 00:04:07 IST
;; Last-Updated: Fri 29 Jun 2018 00:06:34 IST
;;
;; header3-launch.el is part of header3
;; URL: https://github.com/justinethomas/header3
;; Description: Launch script for header3
;;
;; Copyright (c) 2018, Justine T Kizhakkinedath
;; All rights reserved
;;
;; Licensed under the terms of GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007
;; See LICENSE file in the project root for full information.
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


(add-to-list 'load-path "~/.emacs.d/private/header3/")
(load "header3")

(autoload 'auto-update-file-header "header3")
(add-hook 'before-save-hook 'auto-update-file-header)

(autoload 'auto-make-header "header3")
(add-hook 'emacs-lisp-mode-hook 'auto-make-header)
(add-hook 'c-mode-common-hook   'auto-make-header)
(add-hook 'python-mode-hook 'auto-make-header)
(add-hook 'sh-mode-hook 'auto-make-mini-header)
