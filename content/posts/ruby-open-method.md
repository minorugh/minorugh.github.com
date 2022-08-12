+++
date = "2017-11-18T15:43:44+09:00"
categories = ["tech"]
tags = ["ruby"]
title= "RubyでWeb上からファイルをダウンロードする：openメソッドのお勉強"
+++
別サイトで運用しているLINEBLOGの記事をダウンロードし、そこから更新情報を抜き出して自分のホームページに表示したかったのでRubyのお勉強を兼ねて試してみた。
<!--more-->

わかりやすく解説してあるページを見つけたので >>> http://uxmilk.jp/22830

まずは、サンプル通りにコードを書いてquickrun 

## openメソッドのサンプル

```ruby
require 'open-uri'
url = 'https://lineblog.me/minorugh'
open(url) do |file|
  puts file.read
end
```

- ブロック構文を使わないというサンプルも試してみる

```ruby
require 'open-uri'
url = 'https://lineblog.me/minorugh'
file=open(url) 
  puts file.read
```

- なるほどね

## メタデータの取得を試す
取得したファイルからメタデータを取得する関数が使えるとのことで試す

- puts file.base_uri
- puts file.charset
- puts file.content_type
- puts file.last_modified
- puts file.meta
- puts file.status

```ruby
require 'open-uri'
url = 'https://lineblog.me/minorugh'
file = open(url)
puts file.last_modified
```

- ほかはみな確認できたのに、last_modifiedは、なぜかうまくいかなかった

## ファイルをダウンロードして保存する
- ファイルの読み込みはできているので、これを保存することにした

```ruby
require 'open-uri'
url = 'https://lineblog.me/minorugh'

open(url) do |file|
 open("lineblog.html", "w+b") do |out|
 out.write(file.read)  
 end
end
```
- せっかくお勉強したのでもう少し簡潔にしてみる
- LINEBLOG側のサーバーのトラブルに備えて、念のためbackupも取るようにした

```ruby
## getfile.rb

require 'open-uri'
url = 'https://lineblog.me/minorugh'

file = open(url)
open("lineblog.html", "w+b") do |out|
out.write(file.read)
end

# backup
require "date"
d = Date.today
str = d.strftime("backup/%Y%m%d_lineblog.html")

require "fileutils"
FileUtils.cp("lineblog.html", str)    # コピー
```

## 更新情報に反映させる
- ダウンロードしたlineblog.htmlを自分のWEBサーバーにアップロードする
- サーバにあげたファイルは、他のファイルと同様にlast_updateが取得できて表示させることが出来る
- 通常、更新情報からは、当該ファイルにジャンプするようにリンクされているが、lineblogの場合は、本物のLINEBLOGページにジャンプさせないといけない
- つまりWEBから取得したファイルは更新情報の判定フラッグとして使うだけ、表示用には使わない
- ファイルのダウンロードからサーバーへのアップロードするまでの作業は、makefileで自動化している

```makefile
## maikefile

a.out:getfile upftp

getfile:
	ruby getfile.rb
upftp:
	perl ~/Dropbox/Web/GH/upftp/upftp.pl
```

## 参考サイト

[RubyでWeb上からファイルをダウンロードする：open-uri](http://uxmilk.jp/22830)

[UpFtp-結城浩](http://www.hyuki.com/upftp/) 


