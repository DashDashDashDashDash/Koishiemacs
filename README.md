# Koishiemacs
##### *pronounced "koishi-macs"*
---
![](https://ptpimg.me/zu73x2.png)
---

These are just my emacs configuration files. It's only really meant for me to sync my config across places with relative ease.

My mindset when messing around was that I wanted something that didn't interrupt my windows habits, while keeping all of Emacs' goodies. Tested on Emacs 30.1.

Some settings are bound to change, since I still haven't used the editor long enough.

## Installation
```sh
git clone https://github.com/DashDashDashDashDash/Koishiemacs/ ~/.emacs.d
```
You'll get prompted once to install fonts for `nerd-icons`. On Windows, you must install the font `nerd-icons` drops manually.

If you want to use tree-sitter, you'll need to install the grammars onto Emacs. One way of doing this is embedded into the init.el, and you can use it by searching for `treesit`, uncommenting the relevant lines and `C-x C-e`ing the two expressions.

If you're using smaller fonts, or if you're on a smaller screen, you might want to change the dashboard banner image's max height. To do so, just edit the following line... 
```elisp
(setq dashboard-image-banner-max-height 450) ; ...to 300, maybe
```

---

## Main ~~features~~ differences

- Cua mode
- Doom Emacs' `doom-material` [theme](https://github.com/doomemacs/themes) and `doom-modeline`
- Native Emacs tabs (treat them as workspaces!)
- Native Emacs buffer tabs
- LSP via `lsp-mode`, with support for `emacs-lsp-booster`
  - Just add the `emacs-lsp-booster` executable to your PATH
- Tree-sitter support, with `major-mode-remap-alist` already set
- Autostart at a wide 160x50
- Separate custom.el


## Features borrowed from [Witchmacs](https://github.com/snackon/Witchmacs)

- rebinds for `C-x 2` and `C-x 3`
- some minor QoL changes

## Features borrowed from [Prelude](https://github.com/bbatsov/prelude)

- open links in WSL's host OS
- `windmove-mode` bound to `C-c <arrow keys>` for easier frame navigation
- lots of QoL changes

## Features borrowed from [zcjava's .emacs.d](https://github.com/zcjava/emacs.d_configuration)

- smartparens ignoring ' and ` on elisp files

## Features borrowed from [magnars' .emacs.d](https://github.com/magnars/.emacs.d/)

- magit expands sections by default
- the `eval-and-replace` function, bound to `C-x M-e`

## Details on the packages used

- `smartparens`: tries to be smart about pairs (like `()`, `[]`, `""`, etc)
- `origami`: code folding bound to `C-c o`
- `vertico`: provides completion to emacs commands on the minibuffer
- `savehist`: "Persist history over Emacs restarts." from vertico
- `orderless`: a different, more feature-rich completion style for use with vertico & co.
- `corfu`: code completion on buffers
- `doom-themes`: set to doom-material theme
  - also flashes the modeline on errors
- `projectile`(`-ripgrep`): project management, prefixed to `C-c p`, ripgrep by `C-c p s r`
- `magit`: git if it was awesome. `C-x g` to get started.
- `git-gutter-fringe`: show details on lines added/modified/deleted on the left fringe of the buffer
- `avy`: `C-;` to move cursor anywhere on the screen using hotkeys
- `switch-window`: `C-x o` brings a hotkey for each window if you have over 2 windows open in the same frame
- `undo-tree`: treat undo history as a tree. check it out with `C-x u`
- `nerd-icons`: for compatibility with doom-modeline below
- `nerd-icons-dired`: use icons on dired (`C-x d`)
- `doom-modeline`: doom emacs' modeline
- `treemacs`: a tree view of a directory in emacs
- `consult`: replaces lots of search-related and other features with richer menus
  - in particular, replaces `C-s`. `C-S-s` for a wider search on all open buffers.
- `marginalia`: adds useful annotations to anything completion-related
- `embark`: provides a "context menu" to whatever is at point with `C-.`
- `consult-embark`: because we're told so
- `lsp-*`: language server protocol -- turns emacs into a recognizable ide
- `dap-mode`: debugging, goes hand-in-hand with the lsp package above
- `flycheck`: syntax/error checking
  - also customized to use doom emacs' style and position of its indicators
- `beacon`: flash line at point when switching buffers
- `move-text`: bringing a familiar feature to `M-<up>` and `M-<down>`
- `page-break-lines`: display `^L` as horizontal lines, for:
- `dashboard`: the frontpage in the image above
- `solaire-mode`: distinguish file buffers from "fake" buffers
  - customized to exclude dashboard-mode to avoid conflicts
- `rainbow-mode`: hex codes get displayed in their colors in CSS files
- `emmet-mode`: emmet on html buffers via `C-j`
