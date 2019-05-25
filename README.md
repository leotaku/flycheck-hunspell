Note: this file is auto converted from flycheck-hunspell.el by [el2org](https://github.com/tumashu/el2org), please do not edit it by hand!!!

- [flyspell-hunspell](#orgb922f41)
  - [Installation](#orga0a6c53)
  - [Configuration](#orgc31761a)


<a id="orgb922f41"></a>

# flyspell-hunspell

[flyspell-hunspell in action](./screenshot.png)

This package adds support for spell checking to flycheck using the [hunspell](https://hunspell.github.io) command line interface.

In particular it (ab)uses its "-u1" flag which provides a ispell-like (though not 100% compatible) communication format that can be parsed.

The package currently defines checkers for TeX with fixed languages (de). This is done because the author of this package pefers it for their workflow.


<a id="orga0a6c53"></a>

## Installation

I recommend using [straight.el](https://github.com/raxod502/straight.el) for installing non-(m)elpa sources.

```elisp
(use-package flycheck-hunspell
  :straight (flycheck-hunspell :type git :host github
                      :repo "leotaku/flycheck-hunspell")
  :after flycheck)
```


<a id="orgc31761a"></a>

## Configuration

Enable your preferred checkers by adding them to `flycheck-checkers` like so:

```elisp
(add-to-list 'flycheck-checkers 'tex-hunspell-lang)
```

You may also want to automatically enable flycheck for TeX or any other mode.

```elisp
(add-hook 'your-mode-hook 'flycheck-mode)
```

You may also want to advice \`ispell-pdict-save\` for instant feedback when inserting new entries into your local dictionary:

```elisp
(advice-add 'ispell-pdict-save :after 'flyspell-recheck)
(defun flyspell-recheck (_)
  (when (bound-and-true-p flycheck)
   (flycheck-buffer))
```
