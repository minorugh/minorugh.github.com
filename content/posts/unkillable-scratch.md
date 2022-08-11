+++
date = "2018-09-24T17:11:06+09:00"
categories = ["emacs"]
tags = ["buffer"]
title = "Scratchバッファの保存と復元"
+++

Scratchバッファはkillさせない。Scratchバッファーの内容を保存して再起動時に復元する。
これを実現させるための Emacs-lisp は沢山紹介されていますが、MELPAからpackage-installするだけで簡単に導入できるので紹介します。
<!--more-->

## unkillable-scratch

````lisp
;; unkillable-scratch
(setq unkillable-buffers '("^\\*scratch*\\*$" "^\\*dashboard\\*$"))
(unkillable-scratch 1)
````
scratchバッファをkillさせないためのものですが、正規表現マッチを追加することで自由にカスタマイズできます。私の場合は、dashboardも消さないように追記しています。

## persist-scratch

````lisp
;; Persist and save/restore *scratch* buffer
(persistent-scratch-setup-default)
````
scratchバッファの内容を保存して再起動時に復元させるものです。保存データーはデフォルトで````~/.emacs.d/.scratch````に保存されます。

