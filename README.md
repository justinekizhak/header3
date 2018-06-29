<a name="top"></a>
[![Built with Spacemacs](https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg)](http://spacemacs.org)
<a href="https://www.instagram.com/justinekizhakkinedath"><img src="https://i.imgur.com/G9YJUZI.png" alt="Instagram" align="right"></a>
<a href="http://www.twitter.com/justinethomask"><img src="http://i.imgur.com/tXSoThF.png" alt="Twitter" align="right"></a>
<a href="https://www.facebook.com/JustineKizhakkinedath"><img src="http://i.imgur.com/P3YfQoD.png" alt="Facebook" align="right"></a>
<br>
- - -
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![GitHub release](https://img.shields.io/github/release/justinethomas/header3.svg)](https://github.com/justinethomas/header3/releases)
[![Waffle.io - Columns and their card count](https://badge.waffle.io/justinethomas/header3.svg?columns=all)](https://waffle.io/justinethomas/header3)

# Header3 for Emacs
Header3 is a package for Emacs which will help you to automatically add headers
to your source code files when you create a new one in Emacs.  
Header3 is built upon [header2.el](https://www.emacswiki.org/emacs/download/header2.el)
and adds more features to it, but I have made efforts to be compatible with header2.

## Table of contents

* [Types of headers](#types-of-headers)
   * [mini-header](#mini-header)
   * [file-header](#file-header)
   * [package-header](#package-header)
* [Features](#features)
* [Requirements](#requirements)
* [Installation](#installation)
* [How to use?](#how-to-use)
* [Working](#working)

## Types of headers

### mini-header
is useful if you need the smallest header.

```
# -----------------------------------------------------------------------------
# Copyright (c) 2018, Justine T Kizhakkinedath
# All rights reserved
#
# Licensed under the terms of GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007
# See LICENSE file in the project root for full information.
# -----------------------------------------------------------------------------
```
### file-header
is useful if you need a little bigger headers.

```
;; -----------------------------------------------------------------------------
;; Created: Fri  8 Jun 2018 03:20:29 IST
;; Last-Updated: Fri 29 Jun 2018 01:33:00 IST
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
### package-header
is the biggest and the original one from `header2`.   
But this one can change the contents of the License inserted automatically when
it is inside a project and also insert the URL of project.

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
;;     Update #: 1
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
*All examples are generated using Header3.*

## Features
* All Features of [header2.el](https://www.emacswiki.org/emacs/download/header2.el) plus
  * Standard header format.
  * Auto timestamp for file creation and file updating.
  * Automatic entry of the user name of last update and keeping track of update number.
  * Easy Customisation Interface available. Send bug reports within the interface.
  * Keep Commentary and Change Log inside the file.
* Automatically add git project link into the header
* Automatically add License to the files.

## Requirements
* [Git-link](https://github.com/sshaw/git-link) This package is used to extract
the git link of the current buffer.

* [Projectile](https://github.com/bbatsov/projectile) This package is the one
which will check your project directory and get its root path.  
Projectile provides us with project name and project path which is used to check
if the project root contains any License file.  
If it does then it checks the first 5 lines for any mention of "License" and
"Version" (case-insensitive).

Instructions to install these packages are available at their links.

## Installation
* `git clone https://github.com/justinethomas/header3 ~/.emacs.d/lisp/header3`
* Add these lines to your `.emacs` or your init file.
  ```
  (add-to-list 'load-path "~/.emacs.d/lisp/header3")
  (load "header3-launcher")
  ```

## How to use?
* All new files created inside Emacs will automatically trigger a function that
will add the header.
* If you need to add header to an existing file execute either
  - `make-mini-header`
  - `make-file-header`
  - `make-package-header`

## Working
* Files that are recognised as License files are
  - LICENSE
  - License
  - LICENSE.md
  - License.md
  - LICENSE.txt
  - License.txt
* License templates included with the package
  - [MIT License](https://opensource.org/licenses/MIT)
  - [Apache License](http://www.apache.org/licenses/LICENSE-2.0)
  - [Mozzila Public License](https://opensource.org/licenses/MPL-2.0)
  - [GNU AFFERO General Public License](https://www.gnu.org/licenses/agpl-3.0.en.html)
  - [GNU LESSER General Public License](https://www.gnu.org/licenses/lgpl.html)
  - [GNU General Public License Version 2](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html)
  - [GNU General Public License Version 3](https://www.gnu.org/licenses/gpl-3.0.en.html)
* Default settings:
  - `file-header` is the default header for
    - C/C++
    - Clojure
    - Coffee script
    - Emacs lisp
    - Erlang
    - Haskell
    - Java
    - Kotlin
    - Python
    - Ruby
    - Rust
    - Scala
    - Scheme
    - Swift
  - `mini-header` for
    - PHP
    - Shell scripts
    - SQL
* Default settings are stored in `header3-launcher.el` file.
