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

;;-----------------------------------------------------
;; Open new frame instead of dividing the current frame

(setq special-display-buffer-alist
  '("*grep*" "*compilation*"))

;; Save backup files in a special directory instead of on current
;; directory.
(defvar backup-directory "~/.emacs.d/backup-files/")
;;(setq make-backup-files nil)
(setq auto-save-default nil)

(define-key global-map (kbd "C-'") 'revert-buffer)


(define-key global-map (kbd "C-z") 'undo)
(define-key global-map (kbd "M-z") 'redo)
(define-key global-map (kbd "<f1>") 'bs-show)


;; Redo / Undo TODO does not work maybe in melpa?
;;(require 'undo-tree)
;;(global-undo-tree-mode 1)
;;(defalias 'redo 'undo-tree-redo)

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

;; display column numbers
(column-number-mode 1)

;; Zenburn
(load-theme 'zenburn t)

(define-key global-map (kbd "C-ö") 'ace-jump-mode)
(define-key global-map (kbd "M-ö") 'ace-jump-mode-pop-mark)

;; For some reason ctrl-ö does not work in /// citirx it will
;; generate ctrl-; when ctrl-ö is pressed...
(define-key global-map (kbd "C-;") 'ace-jump-mode)
(define-key global-map (kbd "M-;") 'ace-jump-mode-pop-mark)

(define-key global-map (kbd "C-.") 'imenu)
(define-key global-map (kbd "C-,") 'pop-global-mark)
;(define-key global-map (kbd "C-p") 'zap-up-to-char)
(define-key global-map (kbd "C-p") 'whack-whitespace)

(defun print-to-pdf ()
  (interactive)
  (ps-spool-buffer-with-faces)
  (switch-to-buffer "*PostScript*")
  (write-file "/tmp/tmp.ps")
  (kill-buffer "tmp.ps")
  (setq cmd (concat "ps2pdf14 /tmp/tmp.ps ~/print/" (buffer-name) ".pdf"))
  (shell-command cmd)
  (shell-command "rm /tmp/tmp.ps")
  (message (concat "Saved to:  ~/print/" (buffer-name) ".pdf")))

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
;; tabsAreEvil use spaces instead
 '(indent-tabs-mode nil)
'(package-selected-packages
   (quote
    (markdown-mode dumb-jump zenburn-theme magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Minimalism
;;(tool-bar-mode -1)

;; Remove trailing whitespaces on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;-----------------------------------------------------
;; Erlang/OTP
;;
(setq load-path (cons "/vobs/gsn_rel_otp/built/c_linux_gpb_lws/lib/tools-0/emacs" load-path))
(setq exec-path (append '("/vobs/gsn_rel_otp/built/c_linux_gpb_lws/bin") exec-path))
(require 'erlang-start)

;; Enable upcase-region command (default disabled in emacs)
(put 'upcase-region 'disabled nil)

;; Inconsolata font
;(set-default-font "Inconsolata-g")

; This somehow change the font size ! :( it should not I guess
;; update: worked when we add size font when setting font :)
;; I guess it must have a default size otherwise
;(add-to-list 'default-frame-alist '(font . "Inconsolata-g"))

;; extra 5 WIDTH since it is used in margin for line number
;(add-to-list 'default-frame-alist '(width  . 85))
;(add-to-list 'default-frame-alist '(font . "Inconsolata-g-12"))
