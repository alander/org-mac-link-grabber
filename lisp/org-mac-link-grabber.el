
;;; org-mac-link-grabber.el --- Grab links and url from various mac
;;; application and insert them as links into org-mode documents
;;
;; Copyright (c) 2010 Anthony Lander
;; 
;; Author: Anthony Lander <anthony.lander@gmail.com>
;;
;;; Commentary:
;;
;; This code allows you to grab either the current selected items, or
;; the frontmost url in various mac appliations, and insert them as
;; hyperlinks into the current org-mode document at point.
;;
;; This code is heavily based on org-mac-message.el written by John
;; Weigley and Christopher Suckling.
;;
;; Detailed comments for each application interface are inlined with
;; the code. Here is a brief overview of how the code interacts with
;; each application:
;;
;; Finder.app - grab links to the selected files in the frontmost window
;; Mail.app - grab links to the selected messages in the message list
;; Firefox.app - Grab the url of the frontmost tab in the frontmost window
;; Together.app - Grab links to the selected items in the library list
;;
;; Installation:
;;
;; add ('require org-mac-link-grabber) to your .emacs.
;;
;; Usage:
;;
;; Type C-c g to activate the link grabber. This will present you with
;; a menu to choose an application from which to grab a link. You may
;; also type C-g to abort.
;;
;;
;;; Code:
(require 'org)
(require 'org-mac-message)


;; Define key bindings. C-g is the prefix key to grab links.

(progn
  (define-key org-mode-map (kbd "C-c g") 
	(define-prefix-command 'org-mac-grab-link-keymap nil 
	  "Grab links from [F]inder [m]ail.app [f]irefox [t]ogether"))

  (define-key org-mac-grab-link-keymap "F" 'org-mac-finder-insert-selected)
  (define-key org-mac-grab-link-keymap "m" 'org-mac-message-insert-selected)
  (define-key org-mac-grab-link-keymap "f" 'org-mac-firefox-insert-frontmost-url)
  (define-key org-mac-grab-link-keymap "t" 'org-mac-together-item-get-selected)
  (define-key org-mac-grab-link-keymap (kbd "C-g") 'keyboard-quit))


(defun org-mac-paste-applescript-links (as-link-list)
  "Paste in a list of links from an applescript handler. The
   links are of the form <link>::split::<name>"
  (let* ((link-list
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



(defun org-mac-together-item-get-selected ()
  (interactive)
  (message "Applescript: Getting Togther items...")
  (org-mac-paste-applescript-links (as-get-selected-together-items)))




;; Handle links from Firefox.app
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
					 "set links to {}\n"
					 "copy theResult to the end of links\n"
					 "return links as string\n"))))
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



;;
;;
;; Handle links from together.app
;;
;;

(org-add-link-type "x-together-item" 'org-mac-together-item-open)

(defun org-mac-together-item-open (uid)
  "Open the given uid, which is a reference to an item in Together"
  (shell-command (concat "open \"x-together-item:" uid "\"")))

(defun as-get-selected-togther-items ()
  (do-applescript
	  (concat
	   "tell application \"Together\"\n"
	   "	set theLinkList to {}\n"
	   "	set theSelection to selected items\n"
	   "	repeat with theItem in theSelection\n"
	   "		set theLink to (get item link of theItem) & \"::split::\" & (get name of theItem) & \"\n\"\n"
	   "		copy theLink to end of theLinkList\n"
	   "	end repeat\n"
	   "	return theLinkList as string\n"
	   "end tell")))

(defun org-mac-together-item-get-selected ()
  (interactive)
  (message "Applescript: Getting Togther items...")
  (org-mac-paste-applescript-links (as-get-selected-together-items)))

(defun org-mac-together-item-insert-selected ()
  (interactive)
  (insert (org-mac-together-item-get-selected)))



;;
;;
;; Handle links from Finder.app
;;
;;

(defun as-get-selected-finder-items ()
  (do-applescript
	  (concat
	   "tell application \"Finder\"\n"
	   "	set theSelection to the selection\n"
	   "	set links to {}\n"
	   "	repeat with theItem in theSelection\n"
	   "		set theLink to \"file://\" & (POSIX path of (theItem as string)) & \"::split::\" & (get the name of theItem) & \"\n\"\n"
	   "		copy theLink to the end of links\n"
	   "	end repeat\n"
	   "	return links as string\n"
	   "end tell\n")))

(defun org-mac-finder-item-get-selected ()
  (interactive)
  (message "Applescript: Getting Finder items...")
  (org-mac-paste-applescript-links (as-get-selected-finder-items)))

  ;; (let* ((as-link-list (as-get-selected-finder-items))
  ;; 		 (link-list
  ;; 		  (mapcar
  ;; 		   (lambda (x) (if (string-match "\\`\"\\(.*\\)\"\\'" x) (setq x (match-string 1 x))) x)
  ;; 		   (split-string as-link-list "[\r\n]+")))
  ;; 		 split-link URL description orglink orglink-insert rtn orglink-list)
  ;; 	(while link-list
  ;;     (setq split-link (split-string (pop link-list) "::split::"))
  ;;     (setq URL (car split-link))
  ;;     (setq description (cadr split-link))
  ;;     (when (not (string= URL ""))
  ;; 		(setq orglink (org-make-link-string URL description))
  ;; 		(push orglink orglink-list)))
  ;;   (setq rtn (mapconcat 'identity orglink-list "\n"))
  ;;   (kill-new rtn)
  ;;   rtn))

(defun org-mac-finder-insert-selected ()
  (interactive)
  (insert (org-mac-finder-item-get-selected)))


(provide 'org-mac-link-grabber)

;;; org-mac-link-grabber.el ends here