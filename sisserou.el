;; Sisserou -- A linguistic toy based on System F Omega   ;;
;;                                                        ;;
;; Copyright 2014--2024 K.D.P.Ross <KDPRoss@gmail.com>    ;;
;;                                                        ;;
;; This codebase is licensed for the following purposes   ;;
;; only:                                                  ;;
;;                                                        ;;
;; - study of the code                                    ;;
;;                                                        ;;
;; - use of the unaltered code to compile the interpreter ;;
;;   for noncommercial educational and entertainment      ;;
;;   purposes only                                        ;;
;;                                                        ;;
;; - gratis redistribution of the code in entirety and in ;;
;;   unaltered form for any aforementioned purpose        ;;
;;                                                        ;;
;; The code may not be used for any other purposes,       ;;
;; including but not limited to:                          ;;
;;                                                        ;;
;; - any commercial purpose                               ;;
;;                                                        ;;
;; - use by any governmentally-affiliated organisation    ;;
;;                                                        ;;
;; - connection to any external system for any useful     ;;
;;   purpose whatsoever                                   ;;





(defvar sisserou-mode-hook nil)

(defvar sisserou-mode-map
  (let ((sisserou-mode-map (make-keymap)))
    sisserou-mode-map)
  "Keymap for Sisserou major mode")

(defun sisserou-make-face-italic (face)
  (condition-case nil (make-face-italic face) (error nil)))
(defun sisserou-make-face-bold (face)
  (condition-case nil (make-face-bold face) (error nil)))
(defun sisserou-make-face-unitalic (face)
  (condition-case nil (make-face-unitalic face) (error nil)))
(defun sisserou-make-face-unbold (face)
  (condition-case nil (make-face-unbold face) (error nil)))
(make-face 'sisserou-font-lock-comment-face)
(set-face-foreground 'sisserou-font-lock-comment-face "DeepSkyBlue")
(sisserou-make-face-italic 'sisserou-font-lock-comment-face)
(make-face 'sisserou-font-lock-reference-face)
(set-face-foreground 'sisserou-font-lock-comment-face "DeepSkyBlue")
(make-face 'sisserou-font-lock-local-face)
(set-face-foreground 'sisserou-font-lock-local-face "Pink4")
(make-face 'sisserou-font-lock-string-face)
(set-face-foreground  'sisserou-font-lock-string-face "DarkGreen")
(make-face 'sisserou-font-lock-function-name-face)
(set-face-foreground 'sisserou-font-lock-function-name-face "Blue2")
(make-face 'sisserou-font-lock-keyword-face)
(set-face-foreground 'sisserou-font-lock-keyword-face "purple")
(sisserou-make-face-bold 'sisserou-font-lock-keyword-face)
(make-face 'sisserou-font-lock-variable-name-face)
(set-face-foreground 'sisserou-font-lock-variable-name-face "DarkOliveGreen4")
(make-face 'sisserou-font-lock-type-face)
(set-face-foreground 'sisserou-font-lock-type-face "darkorange2")
(sisserou-make-face-bold 'sisserou-font-lock-type-face)
(make-face 'sisserou-font-lock-reference-face)
(set-face-foreground 'sisserou-font-lock-reference-face "CadetBlue")
(make-face 'sisserou-font-lock-sisserou-face)
(set-face-foreground 'sisserou-font-lock-sisserou-face "Yellow")
(sisserou-make-face-bold 'sisserou-font-lock-sisserou-face)
(make-face 'sisserou-punctuation-face)
(set-face-foreground 'sisserou-punctuation-face "DarkOrange1")
(sisserou-make-face-bold 'sisserou-punctuation-face)
(make-face 'sisserou-diacritics-face)
(set-face-foreground 'sisserou-diacritics-face "white")
(sisserou-make-face-italic 'sisserou-diacritics-face)
(make-face 'sisserou-fail-face)
(set-face-foreground 'sisserou-fail-face "Red")
(sisserou-make-face-italic 'sisserou-fail-face)

(defconst sisserou-font-lock-keywords-1
  (list
   '("^[ ]*--.*$" . 'sisserou-font-lock-comment-face)
   '("[[][[][^]]*[]][]]\\|\\<[A-Z][A-Za-z0-9']*\\>" . 'sisserou-font-lock-variable-name-face)
   '("[[][[][^]]*[]][]]\\|\\<[A-Z][A-Za-z0-9']*[.]" . 'sisserou-font-lock-sisserou-face)
   '("#load\\|\\(\\<new[ ]+\\(cons\\|type\\)\\>\\)\\|[*]" . 'sisserou-punctuation-face)
   '("\\(: \\|-\\|[<] \\| [>]\\|[0-9]+#\\)" . 'sisserou-font-lock-sisserou-face)
   '("\\<\\(in\\|let\\|letrec\\|fix\\|case\\|of\\)\\>\\|=\\|,\\|[[] \\| []]\\| |[]]\\|| " . 'sisserou-font-lock-keyword-face)
   '("[[][[][^]]*[]][]]\\|\\<[a-z][A-Za-z0-9']*\\>" . 'sisserou-diacritics-face)
   '("[][<>]\\|;\\| [.]" . 'sisserou-fail-face)
)
  "Highlighting expressions for Sisserou mode")

(defvar sisserou-mode-syntax-table
  (let ((sisserou-mode-syntax-table (make-syntax-table)))
		(modify-syntax-entry ?_ "w" sisserou-mode-syntax-table)
		(modify-syntax-entry ?- "w" sisserou-mode-syntax-table)
		(modify-syntax-entry ?. "w" sisserou-mode-syntax-table)
    sisserou-mode-syntax-table)
  "Syntax table for sisserou-mode")

(defun sisserou-mode ()
  "Major mode for editing Sisserou code"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table sisserou-mode-syntax-table)
  (use-local-map sisserou-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(sisserou-font-lock-keywords-1))
  (setq major-mode 'sisserou-mode)
  (setq mode-name "sisserou")
  (setq default-tab-width 2)
  (setq comment-start "--")
  (run-hooks 'sisserou-mode-hook))

(provide 'sisserou-mode)
