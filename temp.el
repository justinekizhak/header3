;; -----------------------------------------------------------------------------
;; Created: Thu  7 Jun 2018 23:45:48 IST
;; Last-Updated: Fri  8 Jun 2018 01:48:33 IST
;;
;; temp.el is part of header3
;; URL: https://github.com/justinethomas009/header3
;; Description:
;;
;; Copyright (c) 2018, Justine T Kizhakkinedath
;; All rights reserved
;;
;; Licensed under the terms of






(defun inserting-auto-license ()
  (with-temp-buffer
    (insert-file-contents (concat (projectile-project-root) "LICENSE"))
    (setq license-list (split-string (buffer-string) "\n")))
  (dotimes (i 5)
    (if (or (cl-search "license" (downcase (car license-list)))
            (cl-search "version" (downcase (car license-list))))
      (insert " " (string-trim (pop license-list))))
  )
  (insert "\n")
  )

(inserting-auto-license)
 GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007



;; ;; (setq qwiueyiuw "All rights reserved
;; ;; ;;
;; ;; ;; Licensed under the terms of GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007
;; ;; ;; See LICENSE file in the project root for full information.")

;; (defun check-print-hj (some-rand-str)
;;   "This function gets a single line of license text and checks if it includes
;; the licence name or its version case-insensitively"
;;   (if (cl-search "license" (downcase (some-rand-str)))
;;       (insert " " qwiueyiuw)))

;; (check-print-hj "Licensed under the terms of GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007")

;; ;; (cl-search  "under"(downcase  "Licensed UNDER the terms of GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007"))
