;; Add the package repositories
(require 'package)
;;(add-to-list 'package-archives
;;	     '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
	     '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t)

;; Load the packages before starting to modify them
(package-initialize)

;; Download the archives description when required
(when (not package-archive-contents)
  (package-refresh-contents))

;; Packages we want to install

(defvar my-packages
  '(;; Awesome for handling S-expressions
    paredit

    ;; Syntax highlighting and hotkeys for Clojure
    clojure-mode

    ;; Add some more font locking (unreliable?)
    clojure-mode-extra-font-locking

    ;; Clojure REPL integration
    cider

    ;; Integration with GNU R
    ess

    ;; Make it possible to use ido wherever it can be used
    ido-ubiquitous

    ;; Make M-x easier to use for command execution
    smex

    ;; Navigate projects
    projectile

    ;; Make your code taste like the rainbow - colorful parenthesis
    rainbow-delimiters

    ;; S-exp like editing for HTML tags
    tagedit

    ;; Integrate with git
    magit

    ;; The only text editor theme ever worth using
    monokai-theme
    ))

;; Install the packages when they aren't installed
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Show line numbers (to the left, to the left)
(global-linum-mode)

;; Why show scroll bars when you are in Emacs...
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Do not show menu bar
(menu-bar-mode 0)

;; Do not show toolbar
(tool-bar-mode 0)

;; Do not blink the cursor
(blink-cursor-mode 0)

;; The only theme that has ever made sense
(load-theme 'monokai t)

;; The only font that has ever made sense
(cond ((eq system-type 'windows-nt) (set-face-attribute 'default nil :family "Consolas" :height 110))
      ((eq system-type 'gnu/linux) (set-face-attribute 'default nil :family "Source Code Pro" :height 110)))

;; Never show startup screen
(setq inhibit-startup-screen t)

;; Full path in title bar
(setq-default frame-title-format "%b (%f)")

;; No bell function - the '80s aren't coming back
(setq ring-bell-function 'ignore)

(setq
 ;; Killing and yanking interact with OS clipboard
 x-select-enable-clipboard t

 ;; Use middle mouse button for copy and pasteas
 x-select-enable-primary t)

;; Easier hotkey for switching between windows
(defun frame-bck()
  (interactive)
  (other-window -1)
)
(define-key (current-global-map) (kbd "M-o") 'other-window)
(define-key (current-global-map) (kbd "M-O") 'frame-bck)

;; load the Org directory from Google Drive
;; note: can use google-drive-ocamlfuse under Linux to
;; mount the Google Drive in your home directory

;; WARNING: set the HOME variable for your user profile to %USERPROFILE%
;; for this to work under Windows 7
;; read more on: http://www.gnu.org/software/emacs/manual/html_node/emacs/Windows-HOME.html
(setq org-agenda-files '("~/Google Drive/Org"))