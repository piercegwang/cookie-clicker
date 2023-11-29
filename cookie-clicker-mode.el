;;; cookie-clicker-mode.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Pierce Wang
;;
;; Author: Pierce Wang <pierce.g.wang@gmail.com>
;; Maintainer: Pierce Wang <pierce.g.wang@gmail.com>
;; Created: November 29, 2023
;; Modified: November 29, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/piercewang/cookie-clicker-mode
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defvar cookie-clicker-update-timer nil
  "Timer for automatic updates in Cookie Clicker mode.")

(defvar-local cookie-clicker-cookie-count 0)
(defvar-local cookie-clicker-cookies-per-second 0)
(defvar-local cookie-clicker-automatic-clicker-timer nil)
(defvar-local cookie-clicker-automatic-clicker-enabled nil)

(defvar cookie-clicker-upgrades
  '((cursor 10 1)
    (grandma 50 5)
    (farm 100 10))
  "List of upgrades with their names, costs, and CPS increase.")

(defun cookie-clicker-buy-upgrade (upgrade)
  "Buy an upgrade to increase cookies per second (CPS).
UPGRADE is a symbol representing the kind of upgrade to buy."
  (let ((upgrade-info (assoc upgrade cookie-clicker-upgrades)))
    (if upgrade-info
        (let ((cost (cadr upgrade-info))
              (cps-increase (caddr upgrade-info)))
          (if (>= cookie-clicker-cookie-count cost)
              (progn
                (setq cookie-clicker-cookie-count (- cookie-clicker-cookie-count cost)
                      cookie-clicker-cookies-per-second (+ cookie-clicker-cookies-per-second cps-increase))
                (cookie-clicker-update-cookie-display)
                (message "You bought a %s upgrade! CPS: %d" upgrade cookie-clicker-cookies-per-second))
            (message "Not enough cookies to buy the %s upgrade. You need at least %d cookies." upgrade cost)))
      (message "Unknown upgrade: %s" upgrade))))

(defun cookie-clicker-select-upgrade ()
  "Prompt the user to select an upgrade and buy it."
  (interactive)
  (let* ((upgrade-names (mapcar 'car cookie-clicker-upgrades))
         (selected-upgrade (completing-read "Select an upgrade to buy: " upgrade-names nil t)))
    (when selected-upgrade
      (let ((upgrade-info (assoc-string selected-upgrade upgrade-names)))
        (if upgrade-info
            (cookie-clicker-buy-upgrade (intern selected-upgrade))
          (message "Invalid upgrade selected."))))))

(defun cookie-clicker-setup ()
  "Set up the initial state of the Cookie Clicker game."
  (let ((buffer-read-only nil))
    (erase-buffer)
    (insert "Welcome to Cookie Clicker!\n\n")
    (insert "C-c C-c to click cookies\n")
    (insert "Available upgrades:\n")
    (dolist (upgrade cookie-clicker-upgrades)
      (insert (format "%s: Cost %d cookies, CPS + %d\n"
                      (car upgrade) ; Name
                      (cadr upgrade) ; Cost
                      (caddr upgrade)))) ; CPS Increase
    (insert "\nCookies: 0\n")
    (insert "CPS: 0\n")
    (cookie-clicker-update-cookie-display)))

(defun cookie-clicker-click-cookie (&optional n)
  "Click a cookie and increment the cookie count by N (default is 1)."
  (interactive "p")
  (setq cookie-clicker-cookie-count (+ cookie-clicker-cookie-count (or n 1)))
  (cookie-clicker-update-cookie-display))

(defun cookie-clicker-auto-click-cookie ()
  "Function for automatic clicking of cookie."
  (setq cookie-clicker-cookie-count (+ cookie-clicker-cookie-count cookie-clicker-cookies-per-second))
  (cookie-clicker-update-cookie-display))

(defun cookie-clicker-toggle-automatic-clicker ()
  "Toggle the automatic clicker."
  (interactive)
  (if cookie-clicker-automatic-clicker-enabled
      (progn
        (setq cookie-clicker-automatic-clicker-enabled nil)
        (when cookie-clicker-automatic-clicker-timer
          (cancel-timer cookie-clicker-automatic-clicker-timer))
        (message "Automatic clicker disabled."))
    (setq cookie-clicker-automatic-clicker-enabled t)
    (setq cookie-clicker-automatic-clicker-timer (run-at-time 1 1 'cookie-clicker-auto-click-cookie))
    (message "Automatic clicker enabled. CPS: %d" cookie-clicker-cookies-per-second)))

(defun cookie-clicker-update-cookie-display ()
  "Update the display with the current cookie count and CPS."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "Cookies: \\([0-9]+\\)" nil t)
        (replace-match (format "Cookies: %d" cookie-clicker-cookie-count)))
      (when (re-search-forward "CPS: \\([0-9]+\\)" nil t)
        (replace-match (format "CPS: %d" cookie-clicker-cookies-per-second))))))

(defun cookie-clicker-start ()
  "Start a new Cookie Clicker game in a new buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "*Cookie Clicker*")))
    (switch-to-buffer buffer)
    (cookie-clicker-mode)
    (cookie-clicker-setup)
    (cookie-clicker-start-timer)))

(defun cookie-clicker-start-timer ()
  "Start the auto-update timer for Cookie Clicker mode."
  (setq cookie-clicker-update-timer
        (run-at-time nil 1 'cookie-clicker-update)))

(defun cookie-clicker-stop-timer ()
  "Stop the auto-update timer for Cookie Clicker mode."
  (when cookie-clicker-update-timer
    (cancel-timer cookie-clicker-update-timer)
    (setq cookie-clicker-update-timer nil))
  (when cookie-clicker-automatic-clicker-timer
    (cancel-timer cookie-clicker-automatic-clicker-timer)
    (setq cookie-clicker-automatic-clicker-enabled nil)))

(defun cookie-clicker-update ()
  "Update the Cookie Clicker game state and display."
  ;; (cookie-clicker-click-cookie cookie-clicker-cookies-per-second)  ; Simulate a click to update the state
  (cookie-clicker-update-cookie-display))

(defun cookie-clicker-mode-disable ()
  "Clean up when Cookie Clicker mode is disabled."
  (cookie-clicker-stop-timer))

(define-derived-mode cookie-clicker-mode special-mode "Cookie Clicker"
  "Major mode for a basic Cookie Clicker game in Emacs."
  (setq cookie-clicker-cookie-count 0
        cookie-clicker-cookies-per-second 0
        cookie-clicker-automatic-clicker-timer nil
        cookie-clicker-automatic-clicker-enabled nil)
  (add-hook 'kill-buffer-hook 'cookie-clicker-stop-timer nil t)
  (cookie-clicker-start-timer))

(add-hook 'cookie-clicker-mode-hook 'cookie-clicker-mode-disable)

(define-key cookie-clicker-mode-map (kbd "C-c C-c") 'cookie-clicker-click-cookie)
(define-key cookie-clicker-mode-map (kbd "C-c u") 'cookie-clicker-select-upgrade)
(define-key cookie-clicker-mode-map (kbd "C-c a") 'cookie-clicker-toggle-automatic-clicker)

(provide 'cookie-clicker-mode)

;;; cookie-clicker-mode.el ends here
