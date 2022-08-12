+++
date = "2019-07-27T15:51:23+09:00"
categories = ["emacs"]
tags = ["neotree"]
title = "neotreeを試してみる"

+++

以下の機能を盛り込んで実用的なneotreeの設定を試してみました。

1. アイコン表示（use all-the-icons）
2. 文字サイズの変更（縮小）
3. キーマップスタイルの変更（ワンキーで使う）
4. ファイルオープン時にneotreeバッファーを隠す

![NeoTree](https://live.staticflickr.com/65535/48385436792_f843793b6e_b.jpg) 


## 設定

```emacs-lisp
(use-package neotree
  :commands (neo-smart-open neo-create-file-auto-open)
  :init
  (setq-default neo-keymap-style 'concise)
  :config
  (setq neo-smart-open t)
  (setq neo-create-file-auto-open t)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (bind-key [f8] 'neotree-projectile-toggle)
  (bind-key "a" 'neotree-hidden-file-toggle neotree-mode-map)
  (bind-key "<left>" 'neotree-select-up-node neotree-mode-map))


;; Change neotree's font size
;; from https://github.com/jaypei/emacs-neotree/issues/218(setq-default neo-show-hidden-files t)
(defun neotree-text-scale ()
  "Text scale for neotree."
  (interactive)
  (text-scale-adjust 0)
  (text-scale-decrease 1)
  (message nil))
(add-hook 'neo-after-create-hook
	  (lambda (_)
	    (call-interactively 'neotree-text-scale)))


;; Hide neotree window after open file
;; from https://github.com/jaypei/emacs-neotree/issues/77
(add-hook 'neo-enter-hook
          (lambda (type & rest)
           (if (equal type 'file)
               (neotree-hide))))

```
