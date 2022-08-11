+++
date = "2018-09-24T08:24:19+09:00"
categories = ["emacs"]
tags = ["buffer"]
title = "タブを使わない究極のバッファー移動"
+++

Emacsのバッファー移動は、tabbar /  ELscreen    などのタブ系を使っていましたが、diredなどの隠れていてほしいバッファーまで開いてしまうのが嫌でした。 
そんなときに、るびきちさんのこの記事を読んでiflipb.elを試してみましたのでレポートします。

<!--more-->

- [詳説！Alt+Tab感覚で賢くバッファをワンタッチで切り替える](http://emacs.rubikitch.com/iflipb/) 

標準機能の switch-to-buffer に比べてユニークな動作をすることは上記の記事で理解できたのですが、[GitHubのReadme](https://github.com/jrosdahl/iflipb) 
を読むと以下のような機能も設定できることがわかりました。

- 非表示のバッファーを設定できる：iflipb-ignore-buffers
- 戻り機能（最後になったら先頭へ戻る）を設定できる：iflipb-wrap-around

## iflipbの設定

```emacs-lisp
;; iflipb
(setq iflipb-ignore-buffers (list "^[*]" "^magit" "]$"))
(setq iflipb-wrap-around t)
(bind-key "C-<right>" 'iflipb-next-buffer)
(bind-key "C-<left>" 'iflipb-previous-buffer)
```
ignore-buffersの設定は、正規表現が使えます。上記の設定では以下のバッファーを設定しています。

- ````*scratch*````などのアスタリスクのついたもの： ````"^[*]"````
- magit-statusを実行するたびに作られるもの： ````"^magit"````
- diredバッファー： ````"]$"````

私の場合、diredのバッファー名には、hogehoge[dir] というふうに [dir] を付加してファイル名と勘違いしないように工夫していますので、
最後の文字にマッチさせる正規表現 "]$" で機能します。

## ミニバッファーがタブ代わりになる

実際に使ってみてわかったのですが、バッファー移動操作をするとミニバッファーに以下のように開いているバッファー名が表示されます。
（カレントバッファーは、[ ]で囲われて表示されます。）

![Alt Text](https://goo.gl/YUyurv) 

tabbar感覚で使えるのでなかなか面白いです。

## scratchバッファーだけは直ぐに開きたい

作業中に付箋感覚で使ったり、ちょこっとした式を評価したりとよく使うので````*scratch*````だけはどんなタイミングでも直ぐ開きたいですね。そのためにELscreenをタブ無しで使うことにしました。

ヒントになったのは下記の記事です。

- [超シンプル目立たない複数ウィンドウ管理(ぞ式ElScreen)](https://goo.gl/aALmTA) 

## elscreenの設定

``` emacs-lisp
;; elscreen
(setq elscreen-display-tab nil)  ;; Turn off tabs
(setq elscreen-tab-display-kill-screen nil)
(setq elscreen-tab-display-control nil)
(elscreen-start)
(elscreen-create) ;; start with 2 tabs

(bind-key "C-<tab>" 'elscreen-next)
```
ただし上記設定だけでは````*scratch*````を二面開く…という感じになって味気ないので、Emacs起動時のカレントバッファーには、以前、別記事で紹介した dashboard を表示させるようにしました。

- [Emacsのスタート画面をイケメンにする](https://snap.textgh.org/201710161509/) 

![Alt Text](https://goo.gl/4bahMU) 


こうすることでelscreenの裏面に````*scratch*````バッファーが開かれ、表面に````*dashboard*````が表示されます。そして、````C-<tab>````することで表裏の切り替えができるのです。

実際の作業は表面を使い、iflipbを使って複数のバッファーを行き来し、必要なときは````C-<tab>````でいつでも````*scratch*````が呼び出せるという仕組みです。

