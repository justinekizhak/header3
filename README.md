# header3.el for Emacs
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
* To have Emacs add a file header whenever you create a new file in some mode, put these too
  ```
  (autoload 'auto-make-header "header3")
  (add-hook 'emacs-lisp-mode-hook 'auto-make-header)
  (add-hook 'c-mode-common-hook   'auto-make-header)
  (add-hook 'python-mode-hook 'auto-make-header)
  ```