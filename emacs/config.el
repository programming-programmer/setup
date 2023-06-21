(setq gc-cons-threshold (* 50 1000 1000) 
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216 ; 16mb
                  gc-cons-percentage 0.1)))

(setq gcmh-idle-delay 'auto  ; default is 15s
      gcmh-auto-idle-delay-factor 10
      gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(require 'use-package)
(setq use-package-always-ensure t)

(load (expand-file-name "funcs.el" user-emacs-directory))

(server-start)

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
 view-read-only t)                                ; Always open read only files in view mode

(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
(set-default-coding-systems 'utf-8)               ; Default to utf-8 encoding
(show-paren-mode 1)                               ; Show parent paranthesis
(global-visual-line-mode nil)                     ; Turn off that silly visual line mode
(savehist-mode t)                                 ; Save history
(menu-bar-mode -1)                                ; Remove that menubar pls :)
(tool-bar-mode -1)                                ; Remove toolbar too
(scroll-bar-mode -1)                              ; Disable visible scrollbar
(tooltip-mode -1)                                 ; Disable tooltips
(set-fringe-mode 10)                              ; Give some breathing room

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

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; one line at a time
(setq mouse-wheel-progressive-speed nil)            ; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                  ; scroll window under mouse
(setq scroll-step 1)                                ; keyboard scroll one line at a time

(dolist (mode '(org-mode-hook
                prog-mode-hook
                text-mode
                conf-mode))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq create-lockfiles nil
      make-backup-files nil
      ;; But in case the user does enable it, some sensible defaults:
      version-control t     ; number each backup file
      backup-by-copying t   ; instead of renaming current file (clobbers links)
      delete-old-versions t ; clean up after itself
      kept-old-versions 5
      kept-new-versions 5
      backup-directory-alist (list (cons "." (concat user-emacs-directory "backup/"))))

(add-hook 'emacs-startup-hook 'toggle-frame-fullscreen)

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

(use-package guru-mode
  :config
  (guru-global-mode +1))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

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

(use-package evil
  :init
  (setq evil-want-keybinding t) ;; load Evil keybindings in other modes
  (setq evil-want-fine-undo t)
  (setq evil-want-Y-yank-to-eol t)
  :config
  ;; ----- Keybindings
  (define-key evil-window-map "\C-w" 'evil-delete-buffer) ;; Maps C-w C-w to evil-delete-buffer (The first C-w puts you into evil-window-map)
  (define-key evil-motion-state-map "/" 'swiper)

  ;; ----- Setting cursor colors
  (setq evil-emacs-state-cursor    '("#649bce" box))
  (setq evil-normal-state-cursor   '("#d9a871" box))
  (setq evil-operator-state-cursor '("#ebcb8b" hollow))
  (setq evil-visual-state-cursor   '("#677691" box))
  (setq evil-insert-state-cursor   '("#eb998b" (bar . 2)))
  (setq evil-replace-state-cursor  '("#eb998b" hbar))
  (setq evil-motion-state-cursor   '("#ad8beb" box))

  (evil-mode 1))

(use-package evil-surround
  :after evil
  :defer 2
  :config
  (global-evil-surround-mode 1))

(use-package hide-mode-line
  :commands (hide-mode-line-mode))

(use-package doom-modeline
  :config
  (doom-modeline-mode)
  (setq doom-modeline-enable-word-count nil
        doom-modeline-buffer-encoding nil
        doom-modeline-icon nil ; Enable/disable all icons
        doom-modeline-modal-icon nil ;; Icon for Evil mode
        doom-modeline-major-mode-icon nil
        doom-modeline-major-mode-color-icon nil
        doom-modeline-buffer-state-icon nil
        doom-modeline-bar-width 3))

(set-face-attribute 'default t :height 100 :weight 'medium)
(set-face-attribute 'default t :font "Source Code Pro")

(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "Source Code Pro" :height 100 :weight medium))))
 '(fixed-pitch ((t ( :family "Source Code Pro" :height 100)))))

(use-package doom-themes
  :config
  (load-theme 'doom-spacegrey t))

(use-package visual-fill-column
  :defer t
  :config
  (setq visual-fill-column-center-text t)
  (if (eq 'jib/computer 'desktop)
      (setq visual-fill-column-width 100)
    (setq visual-fill-column-width 80))
  (setq visual-fill-column-center-text t))

(use-package writeroom-mode
  :defer t
  :config
  (setq writeroom-maximize-window nil
        writeroom-mode-line t
        writeroom-global-effects nil ;; No need to have Writeroom do any of that silly stuff
        writeroom-extra-line-spacing 3) 
  (setq writeroom-width visual-fill-column-width)
  )

(use-package general)

(general-define-key
 :states '(normal motion visual)
 :keymaps 'override
 :prefix "SPC"

"f" '(counsel-find-file :which-key "find file")
"r" '(counsel-recentf :which-key "recent files")
"TAB" '(switch-to-prev-buffer :which-key "previous buffer")
"SPC" 'counsel-M-x
"C-q" '(save-buffers-kill-terminal :which-key "quit emacs")
"c" 'org-capture

;; "Applications"
"a" '(nil :which-key "applications")
"ao" '(org-agenda :which-key "org-agenda")
;; "am" '(mu4e :which-key "mu4e")
;; "aC" '(calc :which-key "calc")
"ac" '(org-capture :which-key "org-capture")

"ad" '(dired :which-key "dired")

"b" '(nil :which-key "buffer")
"bb" '(counsel-switch-buffer :which-key "switch buffers")
"bd" '(evil-delete-buffer :which-key "delete buffer")

;; Files
"f" '(nil :which-key "files")
"fb" '(counsel-bookmark :which-key "bookmarks")
"ff" '(counsel-find-file :which-key "find file")
"fr" '(counsel-recentf :which-key "recent files")
"fR" '(rename-file :which-key "rename file")
"fs" '(save-buffer :which-key "save buffer")
"fS" '(evil-write-all :which-key "save all buffers")

;; Help/emacs
"h" '(nil :which-key "help/emacs")

"hv" '(counsel-describe-variable :which-key "des. variable")
"hb" '(counsel-descbinds :which-key "des. bindings")
"hM" '(describe-mode :which-key "des. mode")
"hf" '(counsel-describe-function :which-key "des. func")
"hF" '(counsel-describe-face :which-key "des. face")
"hk" '(describe-key :which-key "des. key")

"hm" '(nil :which-key "switch mode")
"hme" '(emacs-lisp-mode :which-key "elisp mode")
"hmo" '(org-mode :which-key "org mode")
"hmt" '(text-mode :which-key "text mode")

"hp" '(nil :which-key "packages")
"hpr" 'package-refresh-contents
"hpi" 'package-install
"hpd" 'package-delete

;; Toggles
"t" '(nil :which-key "toggles")
"tt" '(toggle-truncate-lines :which-key "truncate lines")
"tv" '(visual-line-mode :which-key "visual line mode")
"tn" '(display-line-numbers-mode :which-key "display line numbers")
"tw" '(writeroom-mode :which-key "writeroom-mode")
"tR" '(read-only-mode :which-key "read only mode")
"tm" '(hide-mode-line-mode :which-key "hide modeline mode")

"w" '(nil :which-key "window")
"wm" '(jib/toggle-maximize-buffer :which-key "maximize buffer")
"wN" '(make-frame :which-key "make frame")
"wd" '(evil-window-delete :which-key "delete window")
"w-" '(jib/split-window-vertically-and-switch :which-key "split below")
"w/" '(jib/split-window-horizontally-and-switch :which-key "split right")
"wl" '(evil-window-right :which-key "evil-window-right")
"wh" '(evil-window-left :which-key "evil-window-left")
"wj" '(evil-window-down :which-key "evil-window-down")
"wk" '(evil-window-up :which-key "evil-window-up")
"wz" '(text-scale-adjust :which-key "text zoom")

)

(general-def
  :keymaps 'override

  ;; Emacs ---
  "C-x C-m" 'counsel-M-x

  ;; Utility ---
  "C-c c" 'org-capture
  "C-c a" 'org-agenda
  "C-s" 'swiper
  )

;; Insert keymaps
(general-def
  :states '(insert)
  "C-a" 'evil-beginning-of-visual-line
  "C-g" 'evil-normal-state
  "C-e" 'evil-end-of-visual-line
  "C-S-a" 'evil-beginning-of-line
  "C-S-e" 'evil-end-of-line
  "C-n" 'evil-next-visual-line
  "C-p" 'evil-previous-visual-line
      "C-y" 'yank
  )

(use-package ivy
  :diminish ivy-mode
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
  :general
  (general-define-key
   ;; Also put in ivy-switch-buffer-map b/c otherwise switch buffer map overrides and C-k kills buffers
   :keymaps '(ivy-minibuffer-map ivy-switch-buffer-map)
   "S-SPC" 'nil
   "C-SPC" 'ivy-restrict-to-matches ;; Default is S-SPC, changed this b/c sometimes I accidentally hit S-SPC
   ;; C-j and C-k to move up/down in Ivy
   "C-k" 'ivy-previous-line
   "C-j" 'ivy-next-line)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)

  (ivy-mode 1)
  )

(use-package ivy-rich
 :init
 (setq ivy-rich-path-style 'abbrev)
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

(use-package evil-org
  :diminish evil-org-mode
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda () (evil-org-set-key-theme))))

(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

(general-define-key
 :prefix ","
 :states 'motion
 :keymaps '(org-mode-map)
 "" nil
 "A" '(org-archive-subtree-default :which-key "org-archive")
 "a" '(org-agenda :which-key "org agenda")
 "6" '(org-sort :which-key "sort")
 "c" '(org-capture :which-key "org-capture")
 "s" '(org-schedule :which-key "schedule")
 "S" '(jib/org-schedule-tomorrow :which-key "schedule tmrw")
 "d" '(org-deadline :which-key "deadline")
 "g" '(counsel-org-goto :which-key "goto heading")
 "t" '(counsel-org-tag :which-key "set tags")
 "p" '(org-set-property :which-key "set property")
 "e" '(org-export-dispatch :which-key "export org")
 "." '(org-toggle-narrow-to-subtree :which-key "toggle narrow to subtree")

 "1" '(org-toggle-link-display :which-key "toggle link display")
 "2" '(org-toggle-inline-images :which-key "toggle images")

 ;; insert
 "i" '(nil :which-key "insert")

 "il" '(org-insert-link :which-key "org-insert-link")
 "l" '(org-insert-link :which-key "org-insert-link") ;; More convenient access
 "iL" '(counsel-org-link :which-key "counsel-org-link")

 "is" '(nil :which-key "insert stamp")
 "iss" '((lambda () (interactive) (call-interactively (org-time-stamp-inactive))) :which-key "org-time-stamp-inactive")
 "isS" '((lambda () (interactive) (call-interactively (org-time-stamp nil))) :which-key "org-time-stamp")

 ;; clocking
 "c" '(nil :which-key "clocking")
 "ci" '(org-clock-in :which-key "clock in")
 "co" '(org-clock-out :which-key "clock out")
 "cj" '(org-clock-goto :which-key "jump to clock")
 )

(general-define-key
 :prefix ","
 :states 'motion
 :keymaps '(org-agenda-mode-map)
 "" nil
 "a" '(org-agenda :which-key "org agenda")
 "c" '(org-capture :which-key "org-capture")
 "s" '(org-agenda-schedule :which-key "schedule")
 "," '(org-agenda-schedule :which-key "schedule") ;; quick access
 "d" '(org-agenda-deadline :which-key "deadline")
 "t" '(org-agenda-set-tags :which-key "set tags")
 ;; clocking
 "c" '(nil :which-key "clocking")
 "ci" '(org-agenda-clock-in :which-key "clock in")
 "co" '(org-agenda-clock-out :which-key "clock out")
 "cj" '(org-clock-goto :which-key "jump to clock")
 )

(evil-define-key 'motion org-agenda-mode-map
  (kbd "f") 'org-agenda-later
  (kbd "b") 'org-agenda-earlier)

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
