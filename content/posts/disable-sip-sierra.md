+++
date = "2017-09-24T20:28:52+09:00"
categories = ["tech"]
tags = ["mac"]
title= "macOS SierraでSIPを無効にする方法"
+++
Sierraには、SIP（System Integrity Protecton）というセキュリティを強化機能が追加されていて、sudoを使ってもシステム用とされる/usr等のディレクトリに変更を加えられないようになっていて、開発環境の構築になにかと不都合です。

そこでSIPを無効にすることにしました。

## 1.リカバリーモードで起動する
command + r を押したままMacを起動させます。

## 2.ターミナルを起動
再起動後いつもとは違う画面となっていますが、ビックリしないでください。

画面上の選択のユーティリティーからターミナルを起動します。

![Alt Text](https://c1.staticflickr.com/5/4461/36608131523_42793469ff_z.jpg) 


## 3.SIPコマンドを打ち込む

立ち上げたターミナルに以下を打ち込み、enterを押す。

```bash
csrutil disable
```

![Alt Text](https://c1.staticflickr.com/5/4401/36608134733_f719cfd7ab_z.jpg) 

上図のように、

```bash
Successfully disabled System Integrity Protection. Please restart the machine for the changes to take effect.
```

と返ってくれば成功です。Appleメニューから「再起動」を選んで再起動すると，SIPが無効化された状態で起動します。

## 4.SIPを有効に戻す

SIPが無効の状態で行う必要な作業が終わったら、元に戻しておきましょう。

同じ手順でターミナルを立ち上げて、csrutil enable と打ち込むだけです。

ちなみに私は無効のままにしています。
