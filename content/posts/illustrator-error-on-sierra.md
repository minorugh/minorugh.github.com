+++
date = "2017-09-23T22:33:33+09:00"
categories = ["tech"]
tags = ["mac"]
title= "SierraでIllustrator(CS3~6)のエラーを回避する方法"
+++
macOSをSierraにバージョンアップすると、せっかく構築した開発環境にいろいろ障害が出そうなので、ずっとMarvericsで頑張っていましたが、iTunesなどのバージョンアップについていけなくなったので、やむなくSierraに移行しました。

## Javaランタイムエラーを回避する方法
Sierraにアップグレードした状態で、Illustrator(CS3~6)を起動させると、次のようなエラーメッセージが出ます。

![Alt Text](https://c1.staticflickr.com/5/4428/37248330042_8c1d6434d4_z.jpg) 

原因は、[Appleが、El Capitan以降でJava 6のサポートを終了する。](https://applech2.com/archives/45349646.html) 

とのことで、Sierra(10.12)では、CSシリーズを起動することができなくなったというわけです。

### Javaチェックの実態
エラーがでるのは、起動時に以下のJava6の2つのディレクトリの有無をチェックしているからだとのことです。

* ```/System/Library/Java/JavaVirtualMachines/1.6.0.jdk```
* ```/System/Library/Java/Support/Deploy.bundle```

ところが実際には、Java6は使っていないそうです。
ということは、Java6の2つのディレクトリさえ存在していれば、実際にJava6がインストールされてなくても、Adobe CSシリーズは動作するということになります。

### SierraにJava6のデレクトリを作成する方法
実は、Sieeraの問題点はここからです。

EI Capitan以後に採用された、System Integrity Protection(SIP)による制約のために、管理者権限をもってしても、/System/Library 下に任意のデレクトリを新規作成することができません。

そこで、一時的にSIPを無効にし、当該ディレクトリを作成した後に、再びSIPを有効にする必要があります。

[macOS SierraでSIPを無効にする方法](https://snap.textgh.org/201709242028/) 

上記によりSIPを無効化して再起動し、改めてターミナルを起動します。ターミナルに以下のように入力して実行します。

```bash
sudo mkdir -p /System/Library/Java/JavaVirtualMachines/1.6.0.jdk /System/Library/Java/Support/Deploy.bundle
```

非常に長いコマンドなので、右端の、Deploy.bundleまで確実にコピーしてください。

管理者パスワードを要求されるので入力します。

これで、Illustrator/Photoshop が起動できると思います。

## Illustrator CS4のプラグイン読み込みエラーを回避する方法
私の場合、Illustrator CS4を起動したところ「プラグインの読み込みエラーです。PhotoshopImport.aip」とでます。

いろいろググって試しましたが解決せず、最終的に下記サイトにある、```Yosemite.AICS4.Fix.mpkg.zip```をDownloadし、インストールすることで解決しました。

[https://github.com/ralvarezt/aics4_yosemite_fix/releases](https://github.com/ralvarezt/aics4_yosemite_fix/releases)

Yosemite対応のようですが、Sierra でも大丈夫でした。


