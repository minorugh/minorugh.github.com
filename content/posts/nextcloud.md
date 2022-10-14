+++
date = "2022-09-22T22:48:31+09:00"
categories = ["tech"]
tags = ["cloud"]
title = "XserverにNextcloudを導入"
+++
WEBサイトを運用するためにXserverレンタルサーバーを利用している。

月額1,000円ほどのスタンダード契約なんだけれど300GBのNVMeが割り当てられている。
メインの俳句サイトや個人ブログ、息子や娘のWordpressサイトなど複数ドメインで利用しているが、せいぜい30GBほどの消費で、将来的な増加を配慮したとしてもなお200GB超の余裕がある。

せっかくなので勉強を兼ねてNextcloudを導入してみた。
Dropboxも無料で16GBのスペースを確保できているので実用上は困っていないのだけれど、他に活用できるかもと思って導入してみた。
手順は、下記サイトのTipsを参考にした。

* [Nextcloudをエックスサーバーにインストールして自分専用のクラウドストレージを構築する](https://tech.cmd08.com/nextcloud-xserver) 

## 1. 導入手順
参考サイトに詳しく説明があるが、要はWordpressをインストールするのと同じだと考えれば良い。

### 1.1 インストールする場所を決める
サブドメインでも構わない。
自分は、サーバーを契約したときに自動的に割り当てられる独自ドメイン `minorugh.xsrv.jp`
が使わずに残っていたのでそれを使いました。

* Nextcloud本体を`home/minorugh/minorugh.xsrv.jp/public_html/nextcloud` へインストールします。
* データー保存用には、非公開ディレクトリ`home/minorugh/data/nextcloud`を作成しておきます。

```
📁 home/minorugh
|--📁 minorugh.xsrve.jp
|   |--📁 ...
|   └--📁 public_html
|       └--📁 nextcloud
└--📁 data
    └--📁 nextcloud
```	
### 1.2 データーベースの準備
Nextcloud用にあたらしくデータベースを作成し、ユーザーを追加します。

エックスサーバーのサーバーパネル(MySQL設定)にて

* 新規データベースの作成
* 必要があれば新規ユーザー作成
* アクセス権所有の設定

### 2. インストーラーのダウンロード
[Nextcloud Install](https://nextcloud.com/install/#instructions-server) へアクセス、
DOWNLOAD Server > Community projects > Web installer をクリック、
setup-nextcloud.phpをダウンロードします。今回はサーバーへの転送を省くために直接サーバーにダウンロードします。

エックスサーバーにSSH接続して以下のコマンドを実行します

```shell
$ cd ~/minorugh.xsrv.jp/public_html/nextcloud

$ wget https://download.nextcloud.com/server/installer/setup-nextcloud.php
......
...... `setup-nextcloud.php' へ保存完了
```
### 3. インストール
`minorug.xsrv.jp/nextcloud/setup-nextcloud.php` へアクセスしてインストールします。

インストールディレクトリの指定に「.」を入力してカレントディレクトリに展開します。以上でインストールが完了してセットアップに移るのですが、エックスサーバーではこの記事公開時点で 500 Internal Server Errorが出ます。

minorugh.xsrv.jp/nextcloud/.htaccessの該当箇所を下記のように修正します。

```.htaccess
<IfModule pagespeed_module>
#  ModPagespeed Off コメントアウト
</IfModule>
```
## 4. セットアップ
`.htaccess` の修正が終わったら再度`minorugh.xsrver.jp/nextcloud` へアクセスしてセットアップを終了させます。

### 4.1 ユーザー名
ユーザー名兼ユーザーIDになります。ここで入力した文字列(ユーザーID。最大64文字)がWebDAVアクセスへのURLとして使用されます。ユーザー名の変更は後からも可能ですが、IDは変更できません。

### 4.2 データフォルダー
準備で作成したディレクトリパスを入力します。

### 4.3 ユーザー作成(インストール)
その他の入力も済まし、インストールをクリックします。次ページの推薦アプリはキャンセルをクリックします。


## 5. クライアントアプリをインストール



