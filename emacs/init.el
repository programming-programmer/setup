(require 'package)

(package-initialize)
(org-babel-load-file "~/.config/emacs/config.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("/mnt/chromeos/GoogleDrive/MyDrive/SchoolDocuments/Notes/20230527140013-refile.org"))
 '(package-selected-packages
   '(magit rainbow-delimiters org-roam org-superstar counsel ivy-rich ivy which-key use-package nyan-mode guru-mode emojify doom-themes doom-modeline disable-mouse all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "Source Code Pro" :height 100))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.5))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.3))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.1))))
 '(variable-pitch ((t (:family "Source Code Pro" :height 100 :weight medium)))))
