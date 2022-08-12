+++
date = "2021-09-16T14:31:40+09:00"
categories = ["emacs"]
tags = ["theme"]
title = "Emacs : 作業目的別にテーマを切り替えて使う"

+++
Melpaから package installできる Emacsテーマは、たくさんあってお好みで選べるようになりとても便利なのですが、一つに絞り込むのは難しいですね。

私の場合、作業目的に応じて３つのテーマを切り替えて使えるようにしています。

* [Peach Melpa](https://peach-melpa.org/themes) 


## 1. iceberg-themes
普段使いで目に優しいテーマ：iceberg-theme（自作テーマ）

私の Emacsは、執筆むけに使うことが多く、日本語の文章作成、編集が大半ですので、目に優しいこのテーマを愛用しています。

もともとは、Vim向けに開発された、目に優しいダークブルーの配色です。

* [iceberg.vim](https://cocopon.github.io/iceberg.vim/) 

Emacs向けに移植されたものは、幾つか存在しますが、お好みにカスタマイズしたかったので自作しました。GitHubに置いてます。

* [minorugh/iceberg.emacs](https://github.com/minorugh/iceberg.emacs) 

![Alt Text](https://live.staticflickr.com/65535/51481200845_5052d93c3a_b.jpg) 

## 2. doom-doracura.theme
コード編集するときのテーマ：doom-dracura.theme

ごくたまに Emacs-Lispや Perlスクリプトなどを編集することもあるので、そのときは視認性に優れていて多くの Emacserの間でも普及しているこのテーマを愛用しています。

![Alt Text](https://live.staticflickr.com/65535/51480502098_1b348ba32e_b.jpg) 

## 3. doom-solarized-light.theme
執筆モードでリラックスできるテーマ：doom-solarized-light.theme

Darkroom-mode で、短い文章を書いたり、エッセイの下書き、編集などをするときに珈琲を飲みながらゆったりした気分で文字書きできるので愛用しています。メニューバーもモードライも一切なしの Fullscreen画面は白紙の上になぐり書きしているかのようで快適です。

別途 installしてもよかったのですが doom-themesの中に solarized-light.themeもあったので、そちらを使っています。

![Alt Text](https://live.staticflickr.com/65535/51481235075_cc52b356be_b.jpg) 


## 4. テーマを切り替えて使うための設定

同様の趣旨で作られたものが Melpaでも見つかったのですが、私の場合うまく作動しなかったので独自に関数を作りました。

というより下記記事からのパクリです。

* [Cycle Custom Themes](https://stackoverflow.com/questions/23793288/cycle-custom-themes-w-emacs-24) 

```emacs-lisp
(leaf doom-themes
  :ensure t
  :init
  (leaf iceberg.emacs :el-get minorugh/iceberg.emacs)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/el-get/iceberg.emacs/"))

(leaf cycle-custom-theme
  :config
  (setq my:themes (list 'iceberg 'doom-solarized-light 'doom-dracula))
  (setq curr-theme my:themes)

  (defun cycle-custom-theme ()
	"Switch themes to cycle."
	(interactive)
	(disable-theme (car curr-theme))
	(setq curr-theme (cdr curr-theme))
	(if (null curr-theme) (setq curr-theme my:themes))
	(load-theme (car curr-theme) t)
	(message "%s" (car curr-theme)))

  (bind-key "<f11>" 'cycle-custom-theme)
  (setq curr-theme my:themes)
  (load-theme (car curr-theme) t))
```
