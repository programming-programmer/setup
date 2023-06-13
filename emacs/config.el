(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(require 'use-package)
(setq use-package-always-ensure t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq gc-cons-threshold (* 50 1000 1000) 
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216 ; 16mb
                  gc-cons-percentage 0.1)))

(setq gcmh-idle-delay 'auto  ; default is 15s
      gcmh-auto-idle-delay-factor 10
      gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb

(setq-default
 ad-redefinition-action 'accept                   ; Silence warnings for redefinition
 sentence-end-double-space nil                    ; Double space after a period!? Inhumane!
 cursor-in-non-selected-windows t                 ; Hide the cursor in inactive windows
 fill-column 80                                   ; Set width for automatic line breaks
 help-window-select t                             ; Focus new help windows when opened
 indent-tabs-mode t                               ; Prefer tabs over inferior spaces
 inhibit-startup-screen t                         ; Disable start-up screen
 load-prefer-newer t                              ; Prefer the newest version of a file
 mark-ring-max 128                                ; Maximum length of mark ring
 select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
 tab-width 4                                      ; Set width for tabs
 vc-follow-symlinks t                             ; Always follow the symlinks
 view-read-only t                                 ; Always open read only files in view mode
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)

(setq initial-scratch-message
 " 



















                                                            ███████╗███╗   ███╗ █████╗  ██████╗███████╗
                                                            ██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
                                                            █████╗  ██╔████╔██║███████║██║     ███████╗
                                                            ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
                                                            ███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
                                                            ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝

 "                                           
  )

(setq-default mode-line-format nil)

(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n

(set-default-coding-systems 'utf-8)

(show-paren-mode 1)

(global-visual-line-mode nil)

(savehist-mode t)

(menu-bar-mode -1)

(tool-bar-mode -1)

(scroll-bar-mode -1)

(tooltip-mode -1)

(set-fringe-mode 10)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; one line at a time
(setq mouse-wheel-progressive-speed nil)            ; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                  ; scroll window under mouse
(setq scroll-step 1)                                ; keyboard scroll one line at a time

(dolist (mode '(org-mode-hook
                prog-mode-hook
                text-mode
                conf-mode))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(add-hook 'emacs-startup-hook 'toggle-frame-maximized)

(defun duplicate-line()
    (interactive)
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (open-line 1)
    (next-line 1)
    (yank))

(global-set-key (kbd "C-c d") 'duplicate-line)

(global-set-key "\C-x\C-m" 'execute-extended-command)

(guru-global-mode +1)

(use-package disable-mouse
  :config
  (disable-mouse-global-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package nyan-mode 
  :init (nyan-mode) 
  :config (setq nyan-wavy-trail t))

(use-package emojify
  :config
  (when (member "Noto Color Emoji" (font-family-list))
    (set-fontset-font
     t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend))
  (setq emojify-display-style 'unicode)
  (setq emojify-emoji-styles '(unicode))
  (bind-key* (kbd "C-c e") #'emojify-insert-emoji)) ; override binding in any mode

(set-face-attribute 'default t :height 100 :weight 'medium)
(set-face-attribute 'default t :font "Source Code Pro")

(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "Source Code Pro" :height 100 :weight medium))))
 '(fixed-pitch ((t ( :family "Source Code Pro" :height 100)))))

(use-package doom-themes
  :config
  (load-theme 'doom-gruvbox t))

(use-package ivy
  :bind (("C-s" . swiper)
       :map ivy-minibuffer-map
       ("TAB" . ivy-alt-done)
       ("C-j" . ivy-next-line)
       ("C-k" . ivy-previous-line)
       :map ivy-switch-buffer-map
       ("C-k" . ivy-previous-line)
       ("C-l" . ivy-done)
       ("C-d" . ivy-switch-buffer-kill)
       :map ivy-reverse-i-search-map
       ("C-k" . ivy-previous-line)
       ("C-d" . ivy-reverse-i-search-kill))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)

  (ivy-mode 1))

(use-package ivy-rich
 :init
 (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-x b" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))

(defun dw/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode))

(use-package org
   :defer t
   :hook (org-mode . dw/org-mode-setup)

:config
(setq

org-ellipsis " ▾"

org-todo-keywords
      '((sequence "TODO" "PROG" "DONE"))

org-todo-keyword-faces
      '(("PROG" . (:foreground "red" :weight bold)))

org-src-fontify-natively t

org-fontify-quote-and-verse-blocks t

org-src-tab-acts-natively t

org-edit-src-content-indentation 2

org-hide-block-startup nil

org-src-preserve-indentation nil

org-startup-folded 'content

org-cycle-separator-lines 2

org-startup-with-inline-images t

org-hide-emphasis-markers t

org-agenda-overriding-columns-format
"%PRIORITY %TODO %ITEM %TAGS"

)

(setq org-capture-templates '(

("t" "Todo" entry (file+headline "/mnt/chromeos/GoogleDrive/MyDrive/SchoolDocuments/Notes/20230527140013-refile.org" "Tasks")
 "* TODO %?\n ")

("m" "Scheduled Meeting" entry (file+headline "/mnt/chromeos/GoogleDrive/MyDrive/SchoolDocuments/Notes/20230527140013-refile.org" "Meetings")
"* %?\n ")

("r" "Reminders" entry (file+headline "/mnt/chromeos/GoogleDrive/MyDrive/SchoolDocuments/Notes/20230527140013-refile.org" "Reminders")
"* %?\n ")

("i" "Anything Ideas" entry (file+headline "/mnt/chromeos/GoogleDrive/MyDrive/SchoolDocuments/Notes/20230527140013-refile.org" "Ideas")
"* %?\n ")

))

(global-set-key "\C-ca" 'org-agenda)

(global-set-key "\C-cc" 'org-capture)

(require 'org-indent)

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.5))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.3))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.1))))
 )

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

;; Prettifying src blocks
(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "†")
                                       ("#+END_SRC" . "†")
                                       ("#+begin_src" . "†")
                                       ("#+end_src" . "†")
                                       (">=" . "≥")
                                       ("=>" . "⇨")))

(setq prettify-symbols-unprettify-at-point 'right-edge)
(add-hook 'org-mode-hook 'prettify-symbols-mode)

)

(use-package org-roam

:custom
(org-roam-directory "/mnt/chromeos/GoogleDrive/MyDrive/SchoolDocuments/Notes")

(org-roam-completion-everywhere t)

(org-roam-capture-templates '(

("d" "default" plain
 "%?"
 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
 :unnarrowed t)

("b" "book notes" plain (file "/mnt/chromeos/GoogleDrive/MyDrive/SchoolDocuments/Notes/RoamTemplates/book.org")
 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
 :unnarrowed t)

("c" "class" plain (file "/mnt/chromeos/GoogleDrive/MyDrive/SchoolDocuments/Notes/RoamTemplates/class.org")
 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
 :unnarrowed t)

("s" "school.org" plain (file "/mnt/chromeos/GoogleDrive/MyDrive/SchoolDocuments/Notes/RoamTemplates/school.org")
 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
 :unnarrowed t)

))

:bind (("C-c n l" . org-roam-buffer-toggle)
       ("C-c n f" . org-roam-node-find)
       ("C-c n i" . org-roam-node-insert)
       :map org-mode-map
       ("C-M-i" . completion-at-point))
:config
(org-roam-setup)

)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
