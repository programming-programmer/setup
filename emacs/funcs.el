;; -*- lexical-binding: t; -*-
;;; 
;;; Jake B Functions File
;;;

;; Copyright (C) Jake B
;; Author: Jake B <jakebox0@protonmail.com>
;; URL: https://github.com/jakebox/.emacs
;; This file is not part of GNU Emacs.
;; This file is free software.

;;;;;;;;;;;;;;;;;;;;;;
;; Window Functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun jib/split-window-vertically-and-switch ()
  (interactive)
  (split-window-vertically)
  (other-window 1))
(defun jib/split-window-horizontally-and-switch ()
  (interactive)
  (split-window-horizontally)
  (other-window 1))
;; from https://gist.github.com/3402786
(defun jib/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (if (and (= 1 (length (window-list)))
           (assoc ?_ register-alist))
      (jump-to-register ?_)
    (progn
      (window-configuration-to-register ?_)
      (delete-other-windows))))


;;;;;;;;;;;;;;;;;;;;;;;
;; Files and Buffers ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun jib/save-and-close-this-buffer (buffer)
  "Saves and closes given buffer."
  (if (get-buffer buffer)
	  (let ((b (get-buffer buffer)))
		(save-buffer b)
		(kill-buffer b))))

;; found at http://emacswiki.org/emacs/KillingBuffers
(defun jib/kill-other-buffers (&optional arg)
  "Kill all other buffers.
If the universal prefix argument is used then will the windows too."
  (interactive "P")
  (when (yes-or-no-p (format "Killing all buffers except \"%s\"? "
                             (buffer-name)))
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (when (equal '(4) arg) (delete-other-windows))
    (message "Buffers deleted!")))

(defun jib/split-and-close-sentence ()
  "Deletes current word, inserts a period, and capitalizes the next word -
splits the sentence."
  (interactive)
  (kill-word 1)
  (delete-backward-char 1)
  (insert ".")
  (capitalize-word 1))

(defun jib/listify (&optional count)
  "Turn the region's (or count = n lines) into an orgmode list by prepending a +."
  (interactive "p")
  (let ((lines (count-lines (region-beginning) (region-end)))) ;; By default grab a region
	(if (> count 1) (setq lines count)) ;; but if there was an argument, override the # of lines
	(save-excursion
	  (if (use-region-p) ;; If there's a region go to the start and deactivate the region
		  (goto-char (region-beginning)) (deactivate-mark))
	  (while (> lines 0) ;; Add "+ " at the beginning of each line
		(beginning-of-line)
		(insert " * ")
		(forward-line)
		(setq lines (1- lines))))))

(defun jib/insert-empty-lines-after-each-line ()
  "Add a blank line after each line in a region."
  (interactive)
  (let ((lines (count-lines (region-beginning) (region-end)))) ;; By default grab a region
	(save-excursion
	  (if (use-region-p) ;; If there's a region go to the start and deactivate the region
		  (goto-char (region-beginning)) (deactivate-mark))
	  (while (> lines 0)
		(end-of-line)
		(open-line 1)
		(next-line 2)
		(setq lines (1- lines))))))

;;;;;;;;;;;;;;;;;;
;; Calculations ;;
;;;;;;;;;;;;;;;;;;

(defun jib/time-difference ()
  "Ask for two times/date using `org-read-date' and compute the difference."
  (interactive)
  (message "%s" (ts-human-format-duration ;; Multiply by -1 so first input can be the earlier time
				 (* -1 (ts-difference (ts-parse-org (org-read-date))
									  (ts-parse-org (org-read-date)))))))

(defun jib/return-week-number ()
  (interactive)
  (message "It is week %s of the year." (format-time-string "%U")))

;;;;;;;;;;;;;
;; Orgmode ;;
;;;;;;;;;;;;;

(defun jib/org-copy-link-to-clipboard ()
  "Copy orgmode link to clipboard (simpleclip)."
  (interactive)
  (let ((context (org-element-context)))
	(if (eq (org-element-type context) 'link)
		(simpleclip-set-contents
		 (org-element-property :raw-link context))
	  (user-error "Not a link"))))

(defun jib/org-schedule-tomorrow ()
  "Org Schedule for tomorrow (+1d)."
  (interactive)
  (org-schedule t "+1d"))

(provide 'jib-funcs)
;;; jib-funcs.el ends here
