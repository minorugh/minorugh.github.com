+++
date = "2021-11-13T20:20:44+09:00"
categories = ["emacs"]
tags = ["mozc","elisp"]
title = "ac-mozc : モードレス日本語入力を試してみた"

+++
随分前に一度試してみたが、mozc.elと併用する時に若干変換ルールに違いがあって使いにくく、そのうち使わなくなっていた。
その後、elispの勉強も進み Emacsのスキルも多少向上したので、再度工夫して試してみることにした。

mozc.el導入時には、ローマ字テーブルに azik を使っていたが、これが曲者だということに最近気づいた。結局、標準の mozc.elの romotableを使って、azikのうち手慣れたものだけ追加して試してみたところ、快適に使えるようになったので備忘録に残しておくことにした。 

アルファベットと日本語との間に半角スペースを空けるかどうかについては諸論あるけれど、見やすさを優先した。

pangu-spacing.elというパッケージを使うときは自動的に処理してくれるのでいのだけれど、数字にも反応するので日付表示のときはどうにも間の抜けた感じになる。 
なので数字には対応させないようにした。

## 使ってみた感想
スムーズに変換できているときにはとても効率がいいのだけれど、一度ミスタイプになったものを修正しようとするとおかしな変換になるときがあり、
修正するときは mozc-modeにして対応するということが多い。結局、どちらが快適という点はよくわからない。


変換スピードがもう少し改善されたら更に使いやすくなると思うので、誰か companyと連携した ac-mozc作ってくれないかしらと思う。

```emacs-lisp
 ;; ac-mozc with extended auto-complete
  (leaf auto-complete
	:ensure t
	:bind (:ac-completing-map
		   ("<tab>" . ac-next)
		   ("<backtab>" . ac-previous))
	:hook ((text-mode-hook markdown-mode-hook) . auto-complete-mode)
	:custom (ac-comphist-file . "~/.emacs.d/tmp/ac-comphist.dat")
	:config
	(leaf ac-mozc
	  :ensure t
	  :hook ((text-mode-hook markdown-mode-hook) . my-ac-mozc-setup)
	  :config
	  (add-to-list 'ac-modes '(text-mode . markdown-mode))
	  (bind-key* "<henkan>" 'ac-complete-mozc ac-mode-map)
	  (defun my-ac-mozc-setup ()
		(setq ac-sources
			  '(ac-source-mozc ac-source-ascii-words-in-same-mode-buffers))
		(set (make-local-variable 'ac-auto-show-menu) 0.2)))
	;; auto add space between Japanese and English characters.
	(leaf pangu-spacing
	  :ensure t
	  :hook (markdown-mode-hook . pangu-spacing-mode)
	  :custom (pangu-spacing-real-insert-separtor . t)))
```

