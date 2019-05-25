;;; flycheck-hunspell --- simple spell checking with hunspell and flycheck

;;; Commentary

;; This package adds support for spell checking to flycheck using
;; the hunspell (https://hunspell.github.io) command line interface.

;; In particular it (ab)uses its "-u1" flag which provides a ispell-like
;; (though not 100% compatible) communication format that can be parsed.

;; The package currently defines checkers for TeX with fixed languages (de).
;; This is done because the author of this package pefers it for their workflow.

;; Enable your preferred checkers by adding them to `flycheck-checkers` like so:

;; (add-to-list 'flycheck-checkers 'tex-hunspell-lang)

;; You may also want to automatically enable flycheck for TeX or any other mode.

;; You may also want to advice `ispell-pdict-save` for instant feedback when inserting
;; new entries into your local dictionary:

;; (advice-add 'ispell-pdict-save :after 'flyspell-recheck-idle)
;; (defun flyspell-recheck-idle (_)
;;   (flycheck-buffer))

;;; Code

(require 'flycheck)

(flycheck-define-checker tex-hunspell-de
  "A spell checker for TeX files using hunspell"
  :command ("hunspell"
	    "-d" "de_AT"
	    "-u1" "-t")
  :standard-input t
  :modes (tex-mode latex-mode context-mode)
  :error-parser flycheck-parse-hunspell)

(defun flycheck-parse-hunspell (output checker buffer)
  (let ((return nil)
	(count 1))
    (dolist (line (split-string output "\n"))
      (cond
       ;; empty lines mean newlines in source
       ((string-match-p "^$" line)
	(setq count (1+ count)))
       ;; # indicates that no replacement could be found
       ((string-match "^#" line)
	(progn
	  (string-match
	   (rx line-start "# "		; start
	       (group (+ char)) " "	; error
	       (group (+ digit)))	; column
	   line)
	  (push
	   (flycheck-error-new-at
	    count
	    (1+ (string-to-number (match-string 2 line)))
	    'error
	    (concat "Unknown: " (match-string 1 line))
	    :checker 'hunspell-generic
	    :filename (buffer-file-name buffer)
	    :buffer buffer)
	   return)))
       ;; & indicates that replacements could be found
       ((string-match-p "^&" line)
	(progn
	  (string-match
	   (rx line-start "& "		; start
	       (group (+ char)) " "	; error
	       (+ digit) " "		; suggestion count
	       (group (+ digit)) ": "	; column
	       (group (+? anything)) line-end)
	   line)
	  (push
	   (flycheck-error-new-at
	    count
	    (1+ (string-to-number (match-string 2 line)))
	    'error
	    (concat "Suggest: " (match-string 1 line) " -> " (match-string 3 line))
	    :checker 'hunspell-generic
	    :filename (buffer-file-name buffer)
	    :buffer buffer)
	   return)))))
    return))

(provide 'flycheck-hunspell)

;;; Outdated code

;; (setq prose-hunspell-dictionary "en_US")

;; (flycheck-define-checker prose-hunspell
;;   "A spell checker usning the hunspell tool"
;;   :command ("hunspell" "-u3"
;; 	    (option-flag "-d" prose-hunspell-dictionary))
;;   :standard-input t
;;   :modes (markdown-mode text-mode TeX-mode LaTeX-mode nroff-mode)
;;   :error-patterns
;;   ((error line-start "(null):" line ": " (message) line-end)))

;; (flycheck-define-checker tex-hunspell-de
;;   "A spell checker for TeX files using hunspell"
;;   :command ("hunspell"
;; 	    "-d" "de_AT"
;; 	    "-t" "-u2"))

