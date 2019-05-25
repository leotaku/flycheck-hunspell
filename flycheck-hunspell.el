;;; flycheck-hunspell --- simple spell checking with hunspell and flycheck -*- lexical-binding: t; -*-

;; Author: Leo Gaskin <leo.gaskin@brg-feldkirchen.at>
;; Created: 24 May 2019
;; Homepage: https://github.com/leotaku/flycheck-hunspell
;; Keywords: flycheck, spell, hunspell
;; Package-Version: 0.1.0
;; Package-Requires: ((flycheck "31") (emacs "25.1"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary

;; * flyspell-hunspell :README:

;; This package adds support for spell checking to flycheck using
;; the [[https://hunspell.github.io][hunspell]] command line interface.

;; In particular it (ab)uses its "-u1" flag which provides a ispell-like
;; (though not 100% compatible) communication format that can be parsed.

;; The package currently defines checkers for TeX with fixed languages (de).
;; This is done because the author of this package pefers it for their workflow.

;; ** Installation

;; I recommend using [[https://github.com/raxod502/straight.el][straight.el]] for
;; installing non-(m)elpa sources.

;; #+begin_src elisp
;; (use-package flycheck-hunspell
;;   :straight (flycheck-hunspell :type git :host github
;; 			          :repo "leotaku/flycheck-hunspell")
;;   :after flycheck)
;; #+end_src

;; ** Configuration

;; Enable your preferred checkers by adding them to =flycheck-checkers= like so:
 
;; #+begin_src elisp
;; (add-to-list 'flycheck-checkers 'tex-hunspell-lang)
;; #+end_src

;; You may also want to automatically enable flycheck for TeX or any other mode.

;; #+begin_src elisp
;; (add-hook 'your-mode-hook 'flycheck-mode)
;; #+end_src

;; You may also want to advice `ispell-pdict-save` for instant feedback when inserting
;; new entries into your local dictionary:
 
;; #+begin_src elisp
;; (advice-add 'ispell-pdict-save :after 'flyspell-recheck)
;; (defun flyspell-recheck (_)
;;   (when (bound-and-true-p flycheck)
;;    (flycheck-buffer))
;; #+end_src

;; * bottom delimiter for el2org :code:

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
	(push (flycheck-hunspell-handle-hash line buffer) return))
       ;; & indicates that replacements could be found
       ((string-match-p "^&" line)
	(push (flycheck-hunspell-handle-and line buffer) return))))
    return))

(defun flycheck-hunspell-handle-hash (line buffer)
  (string-match
   (rx line-start "# "			; start
       (group (+ char)) " "		; error
       (group (+ digit)))		; column
   line)
  (flycheck-error-new-at
   count
   (1+ (string-to-number (match-string 2 line)))
   'error
   (concat "Unknown: " (match-string 1 line))
   :checker 'hunspell-generic
   :filename (buffer-file-name buffer)
   :buffer buffer))

(defun flycheck-hunspell-handle-and (line buffer)
  (string-match
   (rx line-start "& "			; start
       (group (+ char)) " "		; error
       (+ digit) " "			; suggestion count
       (group (+ digit)) ": "		; column
       (group (+? anything)) line-end)
   line)
  (flycheck-error-new-at
   count
   (1+ (string-to-number (match-string 2 line)))
   'error
   (concat "Suggest: " (match-string 1 line) " -> " (match-string 3 line))
   :checker 'hunspell-generic
   :filename (buffer-file-name buffer)
   :buffer buffer))

(provide 'flycheck-hunspell)

