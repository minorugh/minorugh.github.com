+++
date = "2018-01-23T20:51:12+09:00"
categories =["tech"]
tags = ["htaccess"]
title= "WEBソース表示等での文字化け対策を.htaccessに書く"
+++
## はじめに
WEB環境のファイルの文字コードは、基本的に全てUTF-8で統一している。

テキストファイル、Perl、cgi、Ruby、js、htmlなどシステムファイルのエンコードは基本UTF-8とし、HTMLファイルのメタタグでもUTF-8と宣言している。

結果、システム的には機嫌よく動いているだけれど、プラウザでソース表示やテキストファイルを直接表示させると文字化けしている。

どうやらサーバー環境が関係していると睨んでググってみて、.htaccessとも関係あるらしいことを知った。

## 文字化け対策としての.htaccess

下記のように対象ファイルの拡張子を羅列しておけばいいみたい。

わたしの場合、perl cgi rubyなどのソースをプラウザで表示させることはありえないので、.js .txt .html を設定しておいた。

##### .htaccess
```
AddType "text/html; charset=UTF-8" .html .js .txt
```

無事解決(^o^)v

ここのサイトが参考になった。

http://www.shtml.jp/htaccess/mojibake.html
