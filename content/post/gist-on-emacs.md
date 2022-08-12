+++
date = "2021-09-01T12:38:38+09:00"
categories = ["emacs"]
tags = ["gist"]
title = "Gist on Emacs"
+++

## みなさん、Emacsで Gist 使ってますか？

私も気軽に使っているのですが [GitHub Gist](https://note.com/ojk/n/nc18e9a15af2a) の Webページを開いて直接コード書き込んだりコピペするというのは何げに中途半端ですね。


ローカルに cloneして git管理するという Tipsも見かけましたが、そもそもバージョン管理がしたいのであればGistにこだわらず、素直にリポジトリで管理すれば済むことです。

## 何がしたいのか整理してみる

* Emacsで開いている Buffer（or region）をそのまま即 gistしたい。
* gistに成功したら、結果をブラウザで開いてほしい。
* 自分の GitHub Gist ページを一発で開きたい。

試行錯誤しましたが、`yagist.el` を package installすれば簡単だと気づきました。
`yagist.el` には、いろいろ機能があるのですが、私の場合は必要な関数だけ利用しています。

* [https://github.com/mhayashi1120/yagist.el/blob/master/README.md](https://github.com/mhayashi1120/yagist.el/blob/master/README.md) 

## ターミナルで gist コマンドが扱えること

このTipsは、ターミナルからコマンドラインで gist が扱えるように設定済であることが前提です。

* [コマンドラインでgistを操作できる "gist"](https://qiita.com/minoruGH/items/6a81af6e35a8ba896a02) 

ターミナルで `gist -help` を打つといろいろオプションパラメターがわかります。

```shell
Usage: gist [-o|-c|-e] [-p] [-s] [-R] [-d DESC] [-u URL]
                          [--skip-empty] [-P] [-f NAME|-t EXT]* FILE*
       gist --login
       gist [-l|-r]

        --login                      Authenticate gist on this computer.
    -f, --filename [NAME.EXTENSION]  Sets the filename and syntax type.
    -t, --type [EXTENSION]           Sets the file extension and syntax type.
    -p, --private                    Makes your gist private.
        --no-private
    -d, --description DESCRIPTION    Adds a description to your gist.
    -s, --shorten                    Shorten the gist URL using git.io.
    -u, --update [ URL | ID ]        Update an existing gist.
    -c, --copy                       Copy the resulting URL to the clipboard
    -e, --embed                      Copy the embed code for the gist to the clipboard
    -o, --open                       Open the resulting URL in a browser
        --no-open
        --skip-empty                 Skip gisting empty files
    -P, --paste                      Paste from the clipboard to gist
    -R, --raw                        Display raw URL of the new gist
    -l, --list [USER]                List all gists for user
    -r, --read ID [FILENAME]         Read a gist and print out the contents
        --delete [ URL | ID ]        Delete a gist
    -h, --help                       Show this message.
    -v, --version                    Print the version.
```
## Emacsの設定

### 1. yagist-region-or-buffer
名前の通り Emacsで開いている Buffer（or region）を POSTしてくれます。

### 2. open-gist-web-page
プラウザで自分のGistページを開くための設定です。<gist-user> に自分のユーザー名をいれます。

### 3. open-lepton
クライアントアプリ Leptonを開くための設定です。

### 4. init.el の設定
```emacs-lisp
(leaf yagist
  :ensure t
  :bind (("C-c y" . yagist-region-or-buffer)
         ("C-c g" . open-gist-web-page)
         ("C-c l" . open-lepton))
  :config
  ;; automatically use `browse-url' to view gists after they'reposted.
  (setq yagist-view-gist t)

  (defun open-gist-web-page ()
    "Open GitHub Gist page with chrome."
    (interactive)
    (browse-url "https://gist.github.com/<gist-user>"))

  (defun open-lepton ()
	"Open keepassxc withe auto passwd input."
	(interactive)
	(compile "lepton")
	(delete-other-windows)))
```


## クライアントアプリの紹介
ローカルでビジュアルにGistを管理したいという方には、マルチプラートホームで利用可能な [Lepton](https://github.com/hackjutsu/Lepton) というクライアントアプリがお薦めです。

WEBページよりは見やすいですし、編集、削除も可能です。New fileをコピペして投稿することもできます。

ただ新規POSTは、上述した `yagist-buffer-or-region` の方が遥かに便利です。

* [https://github.com/hackjutsu/Lepton#readme](https://github.com/hackjutsu/Lepton#readme) 
