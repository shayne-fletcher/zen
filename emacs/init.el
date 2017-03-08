(set-face-attribute 'default nil :font "Source Code Pro")
(setq-default
   tab-width 4
   next-line-add-newlines nil
   require-final-newline nil
   teach-extended-commands-p t
   mode-line-buffer-identification '("%12b [%f]") ;;Show file name in the buffer's mode-line
   indent-tabs-mode nil ;; Always indent using spaces instead of tabs
   buffer-file-coding-system 'undecided-unix
)
(setq transient-mark-mode t) ;;Enable visual feedback on selections.
(setq default-buffer-file-coding-system 'undecided-unix)

;; Customizations for all of c-mode, c++-mode, objc-mode, java-mode
(defun my-c-mode-common-hook ()
 (c-set-offset 'substatement-open 0) ;; Don't indent '{'
 (setq c++-tab-always-indent t) ;;Reindent the current line and nothing else. This is the default.
 (setq c-basic-offset 2) ;; Default is 2
 (setq c-indent-level 2) ;; Default is 2
 ;; (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
 (setq tab-width 2)
 (setq indent-tabs-mode nil)  ; use spaces only if nil
)
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; Visual Basic

(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\)$" .
                                visual-basic-mode)) auto-mode-alist))

;; ;; CMake

(setq load-path (cons (expand-file-name "~/.emacs.d/site-packages") load-path))
 (require 'cmake-mode)
 (setq auto-mode-alist
       (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                 ("\\.cmake\\'" . cmake-mode))
               auto-mode-alist))

;; Batch

;; (require 'batch-mode)

;; ;; Compilation

;; (setq compile-command "run_cmake.cmd&&nmake")
;; (global-set-key "\C-x\C-y" 'compile)

;;Tuareg mode

(add-to-list 'load-path "~/.emacs.d/site-packages/tuareg-2.0.6")
    (autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
    (autoload 'camldebug "camldebug" "Run the Caml debugger" t)
    (autoload 'tuareg-imenu-set-imenu "tuareg-imenu" 
      "Configuration of imenu for tuareg" t) 
    (add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
    (setq auto-mode-alist 
        (append '(("\\.ml[ily]?$" . tuareg-mode)
	          ("\\.topml$" . tuareg-mode))
                  auto-mode-alist))

;;Coq
(setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
  (autoload 'coq-mode "gallina" "Major mode for editing Coq vernacular." t)

;;Nsis

(autoload 'nsis-mode "nsis-mode" "NSIS mode" t)
(setq auto-mode-alist (append '(("\\.\\([Nn][Ss][Ii]\\)$" .
                                 nsis-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.\\([Nn][Ss][Hh]\\)$" .
                                 nsis-mode)) auto-mode-alist))

(autoload 'nsi-mode "nsi-mode" "nsi editing mode." t)
(add-to-list 'auto-mode-alist '("\\.nsi$" . nsi-mode))

;; paren-mode

;; (progn
;;   (customize-set-variable 'paren-mode 'sexp)
;;   (customize-set-variable 'toolbar-visible-p nil)
;; )

(show-paren-mode 1)
(setq show-paren-delay 0)

;; aspell

(add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
(setq ispell-program-name "aspell")
;;(setq ispell-personal-dictionary "C:/path/to/your/.ispell")
 (require 'ispell)
(global-set-key (kbd "<f8>") 'ispell-word)
(global-set-key (kbd "C-<f8>") 'flyspell-mode)
(put 'upcase-region 'disabled nil)

;;Finding characters that don't fit the buffer encoding

(defun find-first-non-ascii-char ()
  "Find the first non-ascii character from point onwards."
  (interactive)
  (let (point)
    (save-excursion
      (setq point
            (catch 'non-ascii
              (while (not (eobp))
                (or (eq (char-charset (following-char))
                        'ascii)
                    (throw 'non-ascii (point)))
                (forward-char 1)))))
    (if point
        (goto-char point)
        (message "No non-ascii characters."))))

(defun find-next-unsafe-char (&optional coding-system)
  "Find the next character in the buffer that cannot be encoded by
coding-system. If coding-system is unspecified, default to the coding
system that would be used to save this buffer. With prefix argument,
prompt the user for a coding system."
  (interactive "Zcoding-system: ")
  (if (stringp coding-system) (setq coding-system (intern coding-system)))
  (if coding-system nil
    (setq coding-system
          (or save-buffer-coding-system buffer-file-coding-system)))
  (let ((found nil) (char nil) (csets nil) (safe nil))
    (setq safe (coding-system-get coding-system 'safe-chars))
    ;; some systems merely specify the charsets as ones they can encode:
    (setq csets (coding-system-get coding-system 'safe-charsets))
    (save-excursion
      ;;(message "zoom to <")
      (let ((end  (point-max))
            (here (point    ))
            (char  nil))
        (while (and (< here end) (not found))
          (setq char (char-after here))
          (if (or (eq safe t)
                  (< char ?\177)
                  (and safe  (aref safe char))
                  (and csets (memq (char-charset char) csets)))
              nil ;; safe char, noop
            (setq found (cons here char)))
          (setq here (1+ here))) ))
    (and found (goto-char (1+ (car found))))
    found))
