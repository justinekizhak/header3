;;; header3-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
;; -----------------------------------------------------------------------------
;; Created: Fri 29 Jun 2018 00:04:07 IST
;; Last-Updated: Sat 30 Jun 2018 12:55:36 IST
;;
;; header3-autoloads.el is part of header3
;; URL: https://github.com/justinethomas/header3
;; Description: Launches header3 package and sets the default values
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


(defconst header-root-folder (file-name-directory load-file-name))

(defsubst header-fetch-resource-path (file)
  "INTERNAL FUNCTION. Get the path to the resource files"
  (expand-file-name file header-root-folder))

testing
(add-to-list 'load-path (header-fetch-resource-path ""))
(load "header3")

;; Launch auto-update before saving the file
(autoload 'auto-update-file-header "header3")
(add-hook 'before-save-hook 'auto-update-file-header)

(autoload 'auto-make-header "header3")

;; auto-make-header can take
;; - "mini-header"
;; - "file-header"
;; - "package-header" as parameters
(add-hook 'c++-mode-hook        (lambda () (auto-make-header "file-header")))
;; (add-hook 'c-mode-common-hook   (lambda () (auto-make-header "file-header")))
(add-hook 'clojure-mode-hook    (lambda () (auto-make-header "file-header")))
(add-hook 'coffee-mode-hook     (lambda () (auto-make-header "file-header")))
(add-hook 'emacs-lisp-mode-hook (lambda () (auto-make-header "file-header")))
(add-hook 'erlang-mode-hook     (lambda () (auto-make-header "file-header")))
(add-hook 'haskell-mode-hook    (lambda () (auto-make-header "file-header")))
(add-hook 'java-mode-hook       (lambda () (auto-make-header "file-header")))
(add-hook 'kotlin-mode-hook     (lambda () (auto-make-header "file-header")))
(add-hook 'python-mode-hook     (lambda () (auto-make-header "file-header")))
(add-hook 'ruby-mode-hook       (lambda () (auto-make-header "file-header")))
(add-hook 'rust-mode-hook       (lambda () (auto-make-header "file-header")))
(add-hook 'scala-mode-hook      (lambda () (auto-make-header "file-header")))
(add-hook 'scheme-mode-hook     (lambda () (auto-make-header "file-header")))
(add-hook 'swift-mode-hook      (lambda () (auto-make-header "file-header")))

(add-hook 'php-mode-hook        (lambda () (auto-make-header "mini-header")))
(add-hook 'sh-mode-hook         (lambda () (auto-make-header "mini-header")))

;; (add-hook 'find-file-hook       'check-for-shell-scripts)
(add-hook 'markdown-mode-hook   'check-if-readme)

(defun check-if-readme()
  (if (cl-search "readme"(downcase (buffer-name)) )
      (auto-make-header "readme")))

(defun check-for-shell-scripts ()
  (if (string-equal (file-name-extension (buffer-name) ""))
      (auto-make-header "mini-header")))
