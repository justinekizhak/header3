<a name="top"></a>
[![Built with Spacemacs](https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg)](http://spacemacs.org)
<a href="http://www.twitter.com/justinethomask"><img src="http://i.imgur.com/tXSoThF.png" alt="Twitter" align="right"></a>
<a href="https://www.facebook.com/JustineKizhakkinedath"><img src="http://i.imgur.com/P3YfQoD.png" alt="Facebook" align="right"></a>
<br>
- - -

# header3.el for Emacs
header3.el is a package for Emacs which will help you to automatically add headers to your source code files when you create a new one in Emacs.

There are two types of headers available, **file-header** and **package-header**.

* **file-header** is useful if you need smaller headers.

```
;; -----------------------------------------------------------------------------
;; Created: Fri  8 Jun 2018 03:20:29 IST
;; Last-Updated: Thu 28 Jun 2018 23:11:33 IST
;;
;; example-package.el is part of header3
;; URL: https://github.com/justinethomas/header3
;; Description: Example of "file-header"
;;
;; Copyright (c) 2018, Justine T Kizhakkinedath
;; All rights reserved
;;
;; Licensed under the terms of GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007
;; See LICENSE file in the project root for full information.
;; -----------------------------------------------------------------------------
;;
;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; -----------------------------------------------------------------------------
```
* **package-header** is useful if you need bigger headers.

```
;;; example-package.el ---
;;
;; Filename: example-package.el
;; Description: Example of "package-header"
;; Author: Justine T Kizhakkinedath
;; Maintainer:
;; Copyright (c) 2018, Justine T Kizhakkinedath
;; All rights reserved
;; Created: Fri  8 Jun 2018 03:21:24 IST
;; Version:
;; Package-Requires: ()
;; Last-Updated: Fri  8 Jun 2018 03:21:26 IST
;;           By: Justine T Kizhakkinedath
;;     Update #: 10
;; URL: https://github.com/justinethomas/header3
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;; -----------------------------------------------------------------------------
;;
;;; Commentary:
;;
;;
;;
;; -----------------------------------------------------------------------------
;;
;;; Change Log:
;;
;;
;; -----------------------------------------------------------------------------
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
;; -----------------------------------------------------------------------------
;;
;;; Code:



;; -----------------------------------------------------------------------------
;;; example-package.el ends here
```
*Both examples are generated using header3.el.*

## All Features of [header2.el](https://www.emacswiki.org/emacs/download/header2.el) plus
* Standard header format.
* Auto timestamp for file creation and file updating.
* Automatic entry of the user name of last update and keeping track of update number.
* Easy Customization Interface available. Send bug reports within the interface.
* Keep Commentary and Change Log inside the file.
### Automatically adding GitHub project link into the header
### Automatic addition of license into the header by extracting info from project License file

## To install
* `git clone https://github.com/justinethomas/header3 ~/.emacs.d/lisp/header3`
* Add these lines to your `.emacs` or your init file.
  ```
  (add-to-list 'load-path "~/.emacs.d/lisp/header3")
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

## Requirements
* [Git-link](https://github.com/sshaw/git-link) This will extract the git link of the current buffer.

* [Projectile](https://github.com/bbatsov/projectile) This is the one which will check your project directory.
Projectile provides us with project name and project path which is used to check if the project root contains any License file.
If it does then it checks the first 5 lines for any mention of "License" and "Version" (case-insensitive).

Instructions to install these packages are available at their links.

## Working
* Files that are recognized as License files are
  - LICENSE
  - License
  - LICENSE.md
  - License.md
  - LICENSE.txt
  - License.txt
* License templates included with the package
  - MIT
  - Apache License Version 2
  - Mozzila Public License
  - GNU AFFERO GENERAL PUBLIC LICENSE
  - GNU LESSER GENERAL PUBLIC LICENSE
  - GNU GENERAL PUBLIC LICENSE VERSION 2
  - GNU GENERAL PUBLIC LICENSE VERSION 3
