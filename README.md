# Koishiemacs
##### *pronounced "koishi-macs"*
---
![](https://ptpimg.me/4x6m6h.png)
---

This is just my emacs configuration file. It's only really meant for me to sync my config across places with relative ease.

My mindset when messing around was that I wanted something that didn't interrupt my windows habits, while keeping all of Emacs' goodies. Tested on Emacs 30.0.

Some settings are bound to change, since I still haven't used the editor long enough.

## Use
```sh
git clone https://github.com/DashDashDashDashDash/Koishimacs/ ~/.emacs.d
```

## Change

If you're using smaller fonts, or if you're on a smaller screen, you might want to change the dashboard banner image's max height. To do so, just edit the following line... 
```elisp
(setq dashboard-image-banner-max-height 500) ; ...to 300, maybe
```

---

## Main ~~features~~ differences

- Cua mode
- Doom Emacs' `doom-material` [theme](https://github.com/doomemacs/themes) and `doom-modeline`
- Native Emacs tabs
- LSP via `lsp-mode`
- Autostart at 160x50
- Separate custom.el

## Features borrowed from [Witchmacs](https://github.com/snackon/Witchmacs)

- company's :config
- switch-window's :config
- rebinds for `C-x 2` and `C-x 3`
- nearly all minor QoL changes
