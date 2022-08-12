+++
date = "2017-02-25T22:27:27+09:00"
categories = ["emacs"]
tags = ["markdown"]
title = "livedown：markdownリアルタイムビューアーの導入"
+++

EmacsのMarkdown-modeビューアーは、multimarkdownが定番のようですが、プレビュー機能は専用エディタほどではありません。そこでお勧めしたいのがLivedownです。Livedownはオートリロード機能が備わっていますので、編集して保存するたびにプレビューが更新されます。できあがりが分かりやすく確認できるのでどんどん書き進められます。ぼくの場合、ちょこっとしたメモもMarkdownで書いているので、そのメモを印刷したい時にビューアーの表示をそのままプリントアウト出来るので便利です。

## 導入

導入は下記リンクに詳しく書かれているけど、README.mdの内容を転載しておこう。

* https://github.com/shime/emacs-livedown


### Installation

First make sure you have node with npm installed.

Then install livedown with

```bash
$ npm install -g livedown
```

Then install this plugin with

```
git clone https://github.com/shime/emacs-livedown.git ~/.emacs.d/emacs-livedown
cat <<EOF >> ~/.emacs.d/init.el
(add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-livedown"))
(require 'livedown)
EOF
```
### Configuration

This plugin uses some configurable variables, with the following defaults

```lisp
(custom-set-variables
 '(livedown-autostart nil) ; automatically open preview when opening markdown files
 '(livedown-open t)        ; automatically open the browser window
 '(livedown-port 1337)     ; port for livedown server
 '(livedown-browser nil))  ; browser to use
```

Make sure to place them before the require line in your init.el.

You can also call the functions manually with

```lisp
M-x livedown-preview
M-x livedown-kill
```

## 使用感、活用法など
* Mac unix Windowsなどマルチ環境で使えるのが good!
* Emacsの Markdown-modeで書いていれば、リアルタイムビューアーの必要性はさほど感じない。
* A4一枚ぐらいのメモ書きをササッと印刷したい時に livedownのプレビュー画面をそのままプリントできるのが超便利。
* Pandocとか使って自動化すれば簡単にPDFにもできそうだけどそれも面倒くさい…

