+++
date = "2022-10-12T17:09:25+09:00"
categories = ["emacs"]
tags = ["qiita"]
title = "EmacsからQiitaの記事投稿用にAtomic Chromeを再導入"
+++

久しぶりにQiitaに投稿しようとしたら以前は出来たコピペができなくなっている。

Emacsのクリップボードの設定ミスを疑って調べたけれど問題ない。仕様が変更されたのかどうかはわからないが、ダイレクトで記事を書くなんてことは考えられない。みんなどうしてるんだろう？

はてさてと悩んで、前に使った `atomic-chrome` を再導入してみた。結果はGood！
 Chrome側の拡張機能は、atomic-chrome-extensions だったと記憶していたのだけれど見当たらず、
[Gost Text](https://chrome.google.com/webstore/detail/ghosttext/godiecgffnchndlihlpaajjcplehddca) に置き換わったみたい。このあたりは下記Tipsに詳しい。

* [atomic-chrome再訪](https://qiita.com/iwaokimura/items/4932e0f8dcfd55c4556c) 

## Emacsの設定
取り急ぎ下記の設定で試したところ快適にコピペ出来た。

```emacs-lisp
;; atomic-chrome
(leaf atomic-chrome
  :ensure t
  :hook (after-init-hook . atomic-chrome-start-server)
  :custom (atomic-chrome-buffer-open-style . 'full))
```
結構起動時間を消費するので、`after-init-hook` で遅延起動している。また、Splitバッファーで表示されるデフォルトのスタイルが気に入らなかったので `full window` に変更している。


