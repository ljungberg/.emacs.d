;; turn off welcome screen
(setq inhibit-startup-message t)

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://stable.melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
;; turn on parentheses highlighting
(show-paren-mode)

;;
(global-set-key "\M-g" 'goto-line)

;;
(global-set-key "\M-i" 'indent-region)

;;Set 24h time format
(setq display-time-24hr-format t)

;; Save backup files in a special directory instead of on current
;; directory.
(defvar backup-directory "~/.emacs.d/backup-files/")

(define-key global-map (kbd "C-'") 'revert-buffer)


(define-key global-map (kbd "C-z") 'undo) ; [C-z]
(define-key global-map (kbd "M-z") 'redo) ; [M-z]
(define-key global-map (kbd "<f1>") 'bs-show) ; [M-z]

;; Window move
(define-key global-map [M-left] 'windmove-left)          ; move to left windnow
(define-key global-map [M-right] 'windmove-right)        ; move to right window
(define-key global-map [M-up] 'windmove-up)              ; move to upper window
(define-key global-map [M-down] 'windmove-down)          ; move to downer window

;; ido mode
(require 'ido)
(ido-mode t)

;; Quickly jump in document with ace-jump-mode
(add-to-list 'load-path "~/.emacs.d/plugins/")  ; Add this directory to Emacs' load path
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)

;; Inconssolata font
;(set-default-font "Inconsolata-g")


(define-key global-map (kbd "C-ö") 'ace-jump-mode)
(define-key global-map (kbd "M-ö") 'ace-jump-mode-pop-mark)

(define-key global-map (kbd "C-.") 'imenu)
(define-key global-map (kbd "C-,") 'pop-global-mark)
;(define-key global-map (kbd "C-p") 'zap-up-to-char)
(define-key global-map (kbd "C-p") 'whack-whitespace)


(defun whack-whitespace (arg)
  "Delete all white space from point to the next word.  With prefix ARG 
 delete across newlines as well.  The only danger in this is that you
  don't have to actually be at the end of a word to make it work.  It
  skips over to the next whitespace and then whacks it all to the next
  word."
  (interactive "P") 
  (let ((regexp (if arg "[ \t\n]+" "[ \t]+")))
    (re-search-forward regexp nil t)
    (replace-match "" nil nil)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/Desktop/daily.org")))
 '(package-selected-packages (quote (magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
