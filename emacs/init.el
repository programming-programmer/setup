(require 'package)

(package-initialize)
(org-babel-load-file "~/.config/emacs/config.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("ae426fc51c58ade49774264c17e666ea7f681d8cae62570630539be3d06fd964" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "ce4234c32262924c1d2f43e6b61312634938777071f1129c7cde3ebd4a3028da" "683b3fe1689da78a4e64d3ddfce90f2c19eb2d8ab1bab1738a63d8263119c3f4" "2f8eadc12bf60b581674a41ddc319a40ed373dd4a7c577933acaff15d2bf7cc6" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "2dd4951e967990396142ec54d376cced3f135810b2b69920e77103e0bcedfba9" "6945dadc749ac5cbd47012cad836f92aea9ebec9f504d32fe89a956260773ca4" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" default))
 '(org-agenda-files
   '("/mnt/chromeos/GoogleDrive/MyDrive/SchoolDocuments/Notes/SUM_2023/ENGL_C101/20230609180007-engl.org" "/mnt/chromeos/GoogleDrive/MyDrive/SchoolDocuments/Notes/20230527140013-refile.org"))
 '(package-selected-packages
   '(calfw-org calfw super-save magit rainbow-delimiters org-roam org-superstar counsel ivy-rich ivy which-key use-package nyan-mode guru-mode emojify doom-themes doom-modeline disable-mouse all-the-icons))
 '(send-mail-function 'mailclient-send-it))
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
