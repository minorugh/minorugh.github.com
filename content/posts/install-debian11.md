+++
date = "2022-09-22T22:09:24+09:00"
categories = ["tech"]
tags = ["debian"]
title = "Debian 11 bullseye 〜Wifiを使ったネットインストール"
+++
Debian10をインストールしてから最近11にアップデートしたのだけれど、いろいろ試行錯誤して触りまくっていたのでなんとなく安定しないので、思い切ってクリーンインストールすることにした。

Dotfilesのお陰でさほど苦労もなくdebian11をクリーンインストールして動作も安定し心なしかフットワークもかるくなった気がする。一年に一度くらいはメンテナンスを兼ねてクリーンインストールするのもいいかなと思った。基本的に下記のTipsの通りの作業であるが、何箇所か躓いているので備忘録を記す。

* [Debian 11 “bullseye” stable 〜Wifiを使ったネットインストール方法](https://www.linux-setting.tokyo/2021/08/debian-11-bullseye-stable-wifi.html?utm_source=pocket_mylist) 

## non-free-firmwareを含んだdebianインストールメディア
というのも見つけたので試したが、自分のThinkpadではだめだった。結局上記Tipsのとおりに、下記のfirmwareセットをダウンロードして上手く行った。

* [Debian 11のfirmwareセット（zip形式）](https://cdimage.debian.org/cdimage/unofficial/non-free/firmware/stable/11.0.0/) 

## デスクトップ環境のリストア
パネル関係を再構築するのに結構手間取った。

どうバックしておいたら簡単にリストアできるのかわからなくて対策していなかったのだけれど、${HOME}/.config/xfce4フォルダーをバックしておくといい感じであった。次回は、これで試せるようにmakefileを修正しておいた。

## Live版のインストールメディア
こちらも試して見た。インストールは簡単に出来てデスクトップ表示まではOKだったが、そこからインストールを選ぶとWiFiで躓く。
Live版にfirmwareを反映させる方法がわからないので諦めた。

