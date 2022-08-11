+++
date = "2017-07-17T17:19:35+09:00"
categories = ["tech"]
tags = ["perl","ftp","mac","linux"]
title= "UpFtpをMacとLinuxとで共用する方法"
+++
UpFtpと言うのは、結城浩さんの作られたMakeWebと併せて使うためのFTP自動化ツールのことです。

* [Makeweb](http://www.hyuki.com/makeweb/):テキストをXHTMLに変換するPerlスクリプト 
* [UpFtp](http://www.hyuki.com/upftp/):ホームページ更新用Perlスクリプト 

これらのツールを使えば、MakeWeb記法(Markdown記法のような書式)で書いた、index.txtをMakeWebでコンパイルしてindex.htmlを生成させ、UpFtpでサーバーに送って更新する…という手順を下記のように makefileに書き、```make -k``` するだけで自動処理されます。

### makefile
```makefile
target = index.html
a.out:all
all: $(target)

.SUFFIXES: .txt .html
.txt.html:
	perl ~/Dropbox/Web/GH/mw/makeweb.pl $< $@ # index.txt to index.html
	perl ~/Dropbox/Web/GH/upftp/upftp.pl 
```

## 異なる環境でデータを共有するのはDropboxが便利
Dropboxは、Windows/Linux/macOSとマルチ環境で使えるので、WEB構築の元データを複数端末で共有するのには一番適している。同期速度などの点でも他のクラウドと比較して一番早い。

ぼくの場合は、どの環境もユーザーフォルダの直下にDropboxをインストールしているので、以下のようPATH構成になる。

* Windows ```C:\Users/minoru/Dropbox```
* Lnux ```/home/minoru/Dropbox```
* Mac ```/Users/minoru/Dropbox```

Makefileの場合は、~/Dropboxと書くことで認識してくれるので容易に共用設定が書けるが、UpFtpの設定ファイルの場合は、以下のようにフルパスを書く必要があるので工夫が必要となる。

```perl
# Local root directory (in fullpath)
my $localrootdir = "";
# File list (in fullpath)
my $filelist = "";
```

## ユーザフォルダーを変数で取得
フルパスを直書きするとマルチ環境で共用できないので、変数を取得してそれを共通で使えるようにします。

### Mac/Linuxの場合
MacもLinuxも同じUNIXなので以下の方法でユーザーフォルダのPATHを取得できる。

```perl
$home = $ENV{"HOME"}
```

<br />
### Windowsの場合
試してないので自信ないのですが、以下の方法でユーザフォルダーのPATHを取得出来る。（らしい）

```
$home = %USERPROFILE%
```

## Mac/Linux共用の設定
私の場合は、以下のように書くことでうまく動いている。

```perl
# Get HOME directory from environment variables
my $home = $ENV{"HOME"};
# Local root directory (in fullpath)
my $localrootdir = "$home/Dropbox/Web/GH";
# File list (in fullpath)
my $filelist = "$home/Dropbox/Web/GH/upftp/filelist.txt";
```

## Windows/Mac(Linux)共用の設定
ここからは確認できていないので無責任になるけれども、OSを判別する特殊変数を使って分岐させる必要がある。

$^O

がその特殊変数です。

```perl
print "$^O\n";
```

と書いたプログラムをそれぞれのOS環境で実行すると下記のようになるはずなので、これを使って条件分岐して$homeを定義すればいいと思います。

* Windows XPの場合「MSWin32」
* Linux系OSの場合「linux」
* macOSの場合「darwin」

## Windows/Mac共用のお勧め環境
Windows環境で有名な秀丸エディタをどうしても使いたい…という場合は別ですが、どちらの環境でもEmacs/Vimといった同じ使い勝手のエディタを使うほうが便利です。その場合は、WindowsにVboxなどの仮想環境を導入してDebian/UbuntuといったLinux系のOSをインストールされる方が遥かに便利です。私の場合は、Windows10上のVboxにDebianをインストールして、Mac環境とほぼ同じ感覚でEmacsを使っています。


