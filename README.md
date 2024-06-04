# Koishiemacs
##### *pronounced "koishi-macs"*
---
![](https://ptpimg.me/4x6m6h.png)
---

These are just my emacs configuration files. It's only really meant for me to sync my config across places with relative ease.

My mindset when messing around was that I wanted something that didn't interrupt my windows habits, while keeping all of Emacs' goodies. Tested on Emacs 30.0.

Some settings are bound to change, since I still haven't used the editor long enough.

## Installation
```sh
git clone https://github.com/DashDashDashDashDash/Koishiemacs/ ~/.emacs.d
```
You'll get prompted once, maybe twice, to install fonts for `emojify` and `nerd-icons`. On Windows, you must install the font `nerd-icons` drops manually.

If you're using smaller fonts, or if you're on a smaller screen, you might want to change the dashboard banner image's max height. To do so, just edit the following line... 
```elisp
(setq dashboard-image-banner-max-height 450) ; ...to 300, maybe
```

---

## Main ~~features~~ differences

- Cua mode
- Doom Emacs' `doom-material` [theme](https://github.com/doomemacs/themes) and `doom-modeline`
- Native Emacs tabs
- LSP via `lsp-mode`, with support for `emacs-lsp-booster`
- Autostart at 160x50
- Separate custom.el

## Features borrowed from [Witchmacs](https://github.com/snackon/Witchmacs)

- company's :config
- switch-window's :config
- rebinds for `C-x 2` and `C-x 3`
- nearly all minor QoL changes

## Details on the packages used

- `ivy`: autocompletion to emacs commands
- `counsel`: use ivy whenever possible
- `swiper`: better interface to `C-s`
- `ivy-posframe`: configured to make `M-x` appear in the center of the window
- `doom-themes`: set to doom-material theme
  - also flashes the modeline on errors
- `projectile`(`-ripgrep`): project management, prefixed to `C-c p`, ripgrep by `C-c p s r`
- `magit`: git if it was awesome
- `git-gutter-fringe`: show details on lines added/modified/deleted on the left fringe of the buffer
- `avy`: `C-:` to move cursor anywhere on the screen using hotkeys
- `switch-window`: `C-x o` brings a hotkey for each window if you have over 2 windows open in the same frame
- `undo-tree`: treat undo history as a tree. check it out with `C-x u`
- `nerd-icons`: for compatibility with doom-modeline below
- `nerd-icons-dired`: use icons on dired (`C-x d`)
- `doom-modeline`: doom emacs' modeline
- `treemacs`: a tree view of a directory in emacs
- `company`: autocompletion inside buffers
- `yasnippet`(`-snippets`): code snippets, bound to company with `C-c y`
- `lsp-*`: language server protocol -- turns emacs into a recognizable ide
- `dap-mode`: debugging, goes hand-in-hand with the lsp package above
- `which-key`: hints at command combos in the minibuffer when you're in the middle of one
- `flycheck`: syntax/error checking
  - also customized to use doom emacs' style and position of its indicators
- `beacon`: flash line at point when switching buffers
- `move-text`: bringing a familiar feature to `M-<up>` and `M-<down>`
- `page-break-lines`: display `^L` as horizontal lines, for:
- `dashboard`: the frontpage in the image above
- `solaire-mode`: distinguish file buffers from "fake" buffers
  - customized to exclude dashboard-mode to avoid conflicts
- `emojify`: emoji support
- `rainbow-mode`: hex codes get displayed in their colors in CSS files