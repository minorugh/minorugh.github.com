+++
date = "2022-01-26T10:19:14+09:00"
categories = ["tech"]
tags = ["Sylpheed"]
title = "Sylpheedをログイン時に最小化起動させる"
+++

元ネタは下記
- [Sylpheedをログイン時に最小化起動する方法](https://bre.is/Vf9rwzFo) 


## Devil's Pieのインストール

```shell
 sudo apt install devilspie
```

## 設定ファイルの作成
~/.devilspie/に設定ファイル sylpheed.dsを以下の内容で作成する。　ファイル名は拡張子が.dsならアプリケーション名でなくてもいいようだ。

```sh
 (if
 (is (application_name) "sylpheed")
 (begin (minimize))
 )
```

## Devil's Pie起動＆終了スクリプトの作成
/usr/local/bin/あたりにシェルスクリプト minimize_sylpheed.shを以下の内容で作成する。　ファイル名は任意、実行権限を忘れずに付与しておくこと。

```sh
 #!/bin/bash
 devilspie &
 sylpheed &
 sleep 5s
 killall -9 devilspie
```
devilspieは常駐監視するタイプのツールなので、5秒で強制終了させている。　 Sylpheedが最小化しないときは時間を延ばしてみる。
## シェルスクリプトを自動開始アプリケーションに登録する

Debian場合は「設定マネージャー」→「セッションと起動」→「自動開始アプリケーション」
 Sylpheedの設定「新着メールを自動チェックする（○分ごと）」「起動時に新着メールをチェックする」を利用するとバックグラウンドでメールチェックしてくれて便利。
