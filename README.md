# header3.el for Emacs
header3.el is a package for Emacs which will help you to automatically add headers to your source code files when you create a new one in Emacs.

There are two types of headers that are available to use, "file-header" and "package-header".

**file-header** are usefull if you need smaller headers.

```
;; -----------------------------------------------------------------------------
;; Created: Tue  5 Jun 2018 13:36:51 IST
;; Last-Updated: Tue  5 Jun 2018 13:37:23 IST
;;
;; example-package.el is part of header3
;; URL: https://github.com/justinethomas009/header3
;; Description: Example of "file-header"
;;
;; Copyright (c) 2018, Justine T Kizhakkinedath
;; All rights reserved
;;
;; Licensed under the terms of GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007
;; See LICENSE file in the project root for full information.
;; -----------------------------------------------------------------------------
```
**package-header** are usefull if you need bigger headers.
```
;;; example-package.el ---
;;
;; Filename: example-package.el
;; Description: Example of "package-header"
;; Author: Justine T Kizhakkinedath
;; Maintainer:
;; Copyright (c) 2018, Justine T Kizhakkinedath
;; All rights reserved
;; Created: Tue  5 Jun 2018 13:43:54 IST
;; Version:
;; Package-Requires: ()
;; Last-Updated: Tue  5 Jun 2018 13:44:12 IST
;;           By: Justine T Kizhakkinedath
;;     Update #: 1
;; URL: https://github.com/justinethomas009/header3
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; example-package.el ends here
```
*Both examples are auto generated using the package.*

## All Features of [header2.el](https://www.emacswiki.org/emacs/download/header2.el) plus
* Standard header format.
* Auto timestamp for file creation and file updation.
* Automatic entry of the user name of last update and keeping track of update number.
* Easy Customisation Interface available. Send bug reports within the interface.
* Keep Commentary and Change Log inside the file.
### Automatically adding github project link into the header
### Automatic addition of license info into the header by extracting info from project "LICENSE" file

## To install
* `git clone https://github.com/justinethomas009/header3 ~/.emacs.d/site-lisp`
* Add these lines to your `.emacs` or your init file. 
  ```
  (add-to-list 'load-path "~/.emacs.d/site-lisp/")
  (load "header3")
  ```
*  Add these lines if you want to auto update your header on file save(which you probably do)
   ```
   (autoload 'auto-update-file-header "header3")
   (add-hook 'before-save-hook 'auto-update-file-header)
   ```
* To have Emacs add a file header whenever you create a new file in some mode, put these too.
  ```
  (autoload 'auto-make-header "header3")
  (add-hook 'emacs-lisp-mode-hook 'auto-make-header)
  (add-hook 'c-mode-common-hook   'auto-make-header)
  (add-hook 'python-mode-hook 'auto-make-header)
  ...
  ```
  
## For Automatic insertion of license info and git link in the header, these packages are required. 
* [Git-link](https://github.com/sshaw/git-link) This will extract the git link of the current buffer.

* [Projectile](https://github.com/bbatsov/projectile) This is the one which will check your project directory. 
Projectile provides us with project name and project path which is used to check if the project root contains any file called "LICENSE". 
If it does then it checks the first 5 lines for any mention of "License" and "Version" (case-insensitive). 
 
Instructions to install these packages are available at their links.
