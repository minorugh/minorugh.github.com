+++
date = "2019-09-03T14:56:12+09:00"
categories = ["emacs"]
tags =["mac"]
title = "mac⌘英かな.appをEmacs専用に使う"
+++

macのkeyboard customizerとしては、
[Karabiner-Element](https://pqrs.org/osx/karabiner/) が有名ですが、Emacs専用として使うなら
[⌘英かな.app](https://beadored.com/command-ei-kana-the-savior-of-macos-sierra-of-the-us-keyboard-users/) が超簡単でおすすめです。

- [Download ⌘英かな.app](https://ei-kana.appspot.com/) 

## コンセプト ##
通常は、他のキーと組み合わせて機能させる修飾キー、`Command_L` `command_R` `Option_L` `Fn` `かな` は単独では何も仕事をしません。そこで、⌘英かな.appを使ってEmacsのキーバインドを割り当てようというものです。

比較的頻繁に操作するキーバインドを以下のように設定すればワンキー操作で動きます。いづれもFunctionキーに割り当てるという発想もありますが、手近な修飾キーを利用することでより便利に操作できます。

#### キーリマップ ####

| key  | remap   | command   |
|---|---|---|
|Command_L   |C-g   | keyboard-quit  |
|Command_R   |s-m   | mozc-temp-convert  |
|Option_L   |s-n   | neo-tree-toggle  |
| Fn  |C-j   | emmet-expand-line  |
| かな  |s-j   | toggle-input-method  |

## 設定法 ##

<img alt="" src="https://live.staticflickr.com/65535/48669783403_cbbf8d3160_o.png" style="width:500px" />
