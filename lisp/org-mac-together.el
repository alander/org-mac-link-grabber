;;; org-mac-together.el --- grab and insert links from Together.app, and follow them as well
;;
;; Copyright (c) 2010 Anthony Lander
;; 
;; Author: Anthony Lander <anthony.lander@gmail.com>
;;
;;; Commentary:
;;
;; This code allows you to grab links to the selected items the
;; Together.app main window, paste them into an org-mode document, and
;; follow them using the normal org-mode link opening mechanism.
;; Together.app is a Mac OS X tool to organize information sold by
;; Reinvented Software <http://reinventedsoftware.com/>.
;;
;; This code is heavily based on org-mac-message.el written by John
;; Weigley and Christopher Suckling.
;;
;;
;; Installation:
;;
;; add ('require org-mac-together) to your .emacs, and then,
;; optionally, define a key binding like this:
;;
;; (add-hook 'org-mode-hook (lambda ()
;; 	(define-key org-mode-map (kbd "C-c t") 'org-together-item-insert-selected)))
;;
;; Usage:
;;
;; Either use the keybinding you defined above or type M-x
;; org-together-item-insert-selected RET to grab the current selected
;; items from the main window of Together.app, and insert links to
;; them into your org-mode document at point.
;;
;; To follow a link, use the normal org-mode link following mechanism.
;;
;;; Code:
(require 'org-mac-message)

(org-add-link-type "x-together-item" 'org-together-item-open)

(defun org-together-item-open (uid)
  "Open the given uid, which is a reference to an item in Together"
  (shell-command (concat "open \"x-together-item:" uid "\"")))


(defun as-get-selected-togther-items ()
  (do-applescript
	  (concat
	   "tell application \"Together\"\n"
	"set theLinkList to {}\n"
	"set theSelection to selected items\n"
	"repeat with theItem in theSelection\n"
		"set theLink to (get item link of theItem) & \"::split::\" & (get name of theItem) & \"\n\"\n"
		"copy theLink to end of theLinkList\n"
	"end repeat\n"
	"return theLinkList as string\n"
"end tell")))

(defun org-together-item-get-selected ()
  (interactive)
  (message "Applescript: Getting Togther items...")
  (let* ((as-link-list (as-get-selected-togther-items))
		 (link-list
		  (mapcar
		   (lambda (x) (if (string-match "\\`\"\\(.*\\)\"\\'" x) (setq x (match-string 1 x))) x)
		   (split-string as-link-list "[\r\n]+")))
		 split-link URL description orglink orglink-insert rtn orglink-list)
	(while link-list
      (setq split-link (split-string (pop link-list) "::split::"))
      (setq URL (car split-link))
      (setq description (cadr split-link))
      (when (not (string= URL ""))
		(setq orglink (org-make-link-string URL description))
		(push orglink orglink-list)))
    (setq rtn (mapconcat 'identity orglink-list "\n"))
    (kill-new rtn)
    rtn))

(defun org-together-item-insert-selected ()
  (interactive)
  (insert (org-together-item-get-selected)))

(provide 'org-mac-together)

;;; org-mac-together.el ends here
