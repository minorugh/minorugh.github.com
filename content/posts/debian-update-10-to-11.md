+++
date = "2021-08-23T08:21:40+09:00"
categories = ["linux"]
tags = ["Debian"]
title = "Debian 10(buster) を 11(bullseye)へ更新"

+++
```
Debian 11.0 was released on August 14th, 2021. 
The release included many major changes, described in our press release and the Release Notes.
```

ということで、取り急ぎ更新してみました。

参考にしたのは、下記のTipsです。というよりそのままのコピーです(^^)

* [Debian 10(buster)から11(bullseye)への更新メモ](https://izaten.fc2.net/blog-entry-325.html?utm_source=pocket_mylist) 

## sources.listの書き換え

- アップデートに先立って現状のbusterを最新版に更新（update/upgrade）しておきます。
- 次に、/etc/apt/sources.list を下記のとうりに書き換えます。

```
deb http://ftp.jp.debian.org/debian/ bullseye main contrib non-free
deb-src http://ftp.jp.debian.org/debian/ bullseye main contrib non-free

deb http://ftp.jp.debian.org/debian/ bullseye-updates main contrib non-free
deb-src http://ftp.jp.debian.org/debian/ bullseye-updates main contrib non-free

deb http://security.debian.org/ bullseye-security main
deb-src http://security.debian.org/ bullseye-security main
```
要は、buster の部分が bullseye に変わるだけです。

## 更新

- まずは `# apt update && apt upgrade` を実行して最小限のアップグレードを行ったあと、問題がないことを確認します。
- その後 `# apt full-upgrade` または `# apt dist-upgrade` を実行して最後の更新をします。

途中、いろいろ問い合わせがあるので注意して選択します。私の場合、`sudoers` ファイルの扱いについて問い合わせ確認があったので、"既存のまま残す" を選択しました。

処理時間は、WiFi環境によって異なると思いますが、私の場合(5G環境)でおおよそ30分ほどでした。

## 確認
Debianのバージョンとカーネルのバージョン確認しておきます。

### Debianのバージョン

```shell
$ cat /etc/debian_version
11.0
```

### カーネルのバージョン
```shell
$ uname -a
Linux e590 5.10.0-8-amd64 #1 SMP Debian 5.10.46-4 (2021-08-03) x86_64 GNU/Linux
```
※ 上記の e590 というのは、私のマシン名です。

## スクリーンショット
Debian 11 xface4 のデスクトップ画面のスクショです。だいぶん垢抜けしてきました(^^)

![Alt Text](https://live.staticflickr.com/65535/51395292747_c52f2dc3e8_b.jpg)

