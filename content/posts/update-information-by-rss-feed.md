+++
date = "2018-12-20T19:23:18+09:00"
categories = ["tech"]
tags = ["rss","php"]
title = "RSSフィードを取得してブログの更新情報を表示"
+++

phpスクリプトに初挑戦した。

## 参考にした情報
- [ブログの更新情報をPHPで読み込んで表示する方法](https://sole-color-blog.com/blog/44/) 

## 記事更新後7日間はNew表示する

基本的には参考サイトのSampleどおり。ブログ記事更新後１週間はNew表示させるようにModifyしてみました。

投稿日の表示には、date("Y.n.j", strtotime($entry->pubDate)) を使いました。n：1~12 / j：1~31 表示になることも初めて知りました。
ただ１週間を判定するための比較演算式ではうまく機能しませんでしたので、そこだけは、date("Y/m/d") に変更しました。m：01~12 / d：01~31

``` php
if (date("Y/m/d", strtotime($entry->pubDate)) >= date('Y/m/d', strtotime('- 7 day'))) {…} else {…}
```
## 完成したスクリプト

まだまだ素人臭いスクリプトで恥ずかしいけど出来上がったのは以下の通り。一応目的どおりには動きます。

##### rss-update-info.php 
``` php
<?php

$rssdata = simplexml_load_file("https://blog.wegh.net/index.xml");

// 読み込み件数を決定する
$num_of_data = 5;

//出力内容の初期化
$outdata = "";

//設定した読み込み件数分だけ取得を繰り返す
for ($i=0; $i<$num_of_data; $i++){
    $entry = $rssdata->channel->item[$i]; //記事1個取得
    $date = date("Y.n.j", strtotime($entry->pubDate));
    $title = $entry->title; //タイトル取得
    $link = $entry->link; //リンクURL取得

    //出力内容（リンクとタイトル）を生成する
    $outdata .= '<span style="line-height:1.5;font-size:110%;"><a
target="_blank" style="color:#24890d;text-decoration:none;" href="' . $link . '">' . $title . ' ';
    //出力内容に投稿日を加える（7日間はNew表示）
    if (date("Y/m/d", strtotime($entry->pubDate)) >= date('Y/m/d', strtotime('- 7 day')))
    {
        $outdata .= $date . '</a> <img src="./images/new.gif"></span><br>';
    } else {
        $outdata .= $date . '</a></span><br>';
    }
}

//実行結果を出力する
echo '<p>' . $outdata . '</p>';

?>
```
## Screenshots
ホームページに表示させたスクリーンショット（Gospel-Haiku Blog の部分）

![Alt Text](https://c1.staticflickr.com/5/4813/46393542371_325785568f_b.jpg) 

