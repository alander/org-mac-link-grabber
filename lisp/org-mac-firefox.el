;;; org-mac-firefox.el --- Grab Firefox.app urls and insert them as links into org-mode documents
;;
;; Copyright (c) 2010 Anthony Lander
;; 
;; Author: Anthony Lander <anthony.lander@gmail.com>
;;
;;; Commentary:
;;
;; This code allows you to grab the current active url from the main
;; Firefox.app window, and insert it as a link into an org-mode
;; document. Unfortunately, firefox does not expose an applescript
;; dictionary, so this is necessarily introduces some limitations.
;;
;; The applescript to grab the url from Firefox.app uses the System
;; Events application to give focus to the firefox application, select
;; the contents of the url bar, and copy it. It then uses the title of
;; the window as the text of the link. There is no way to grab links
;; from other open tabs, and further, if there is more than one window
;; open, it is not clear which one will be used (though emperically it
;; seems that it is always the last active window).
;;
;; This code is heavily based on org-mac-message.el written by John
;; Weigley and Christopher Suckling.
;;
;;
;; Installation:
;;
;; add ('require org-mac-firefox) to your .emacs, and then,
;; optionally, define a key binding like this:
;;
;; (add-hook 'org-mode-hook (lambda ()
;; 	(define-key org-mode-map (kbd "C-c f") 'org-mac-firefox-insert-frontmost-url)))
;;
;; Usage:
;;
;; Either use the keybinding you defined above or type M-x
;; org-mac-firefox-insert-frontmost-url RET to grab the frontmost
;; Firefox.app link and insert it into your org-mode document at
;; point.
;;
;;; Code:

(defun as-mac-firefox-get-frontmost-url ()
  (let ((result (do-applescript
					(concat
					 "set oldClipboard to the clipboard\n"
					 "set frontmostApplication to path to frontmost application\n"
					 "tell application \"Firefox\"\n"
					 "	activate\n"
					 "	delay 0.15\n"
					 "	tell application \"System Events\"\n"
					 "		keystroke \"l\" using command down\n"
					 "		keystroke \"c\" using command down\n"
					 "	end tell\n"
					 "	delay 0.15\n"
					 "	set theUrl to the clipboard\n"
					 "	set the clipboard to oldClipboard\n"
					 "	set theResult to (get theUrl) & \"::split::\" & (get name of window 1)\n"
					 "end tell\n"
					 "activate application (frontmostApplication as text)\n"
					 "return theResult\n"))))
	(car (split-string result "[\r\n]+" t))))

(defun org-mac-firefox-get-frontmost-url ()
  (interactive)
  (message "Applescript: Getting Firefox url...")
  (let* ((url-and-title (as-mac-firefox-get-frontmost-url))
		 (split-link (split-string url-and-title "::split::"))
		 (URL (car split-link))
		 (description (cadr split-link))
		 (org-link))
	(when (not (string= URL ""))
	  (setq org-link (org-make-link-string URL description)))
  (kill-new org-link)
  org-link))

(defun org-mac-firefox-insert-frontmost-url ()
  (interactive)
  (insert (org-mac-firefox-get-frontmost-url)))

(provide 'org-mac-firefox)

;;; org-mac-firefox.el ends here