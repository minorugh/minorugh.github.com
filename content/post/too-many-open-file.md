+++
date = "2018-05-02T13:58:42+09:00"
categories = ["emacs"]
tags = ["mac"]
title = "Macの 'too many open file' エラーを解消する方法"
+++

MacでEmacsを起動しているときに ```'too many open file'``` と出て固まってしまうことがありました。

別環境のDebianで起動しているEmacsでは、そうした現象もないので、Mac固有の問題だと考えて情報を収集していたら、
以下のTipsを見つけました。

* [Macの「Too many open files」エラーを解消](https://qiita.com/sou_lab/items/1ca051a1f3b906a23dc8) 

以下、内容的には重複しますが備忘録として残しておきます。

## Macの上限値を調べる

ターミナルでコマンドを打って現在の上限数を確認。
私の場合は、以下の表示でした。

```shell
$ sudo launchctl limit
    cpu         unlimited      unlimited
    filesize    unlimited      unlimited
    data        unlimited      unlimited
    stack       8388608        67104768
    core        0              unlimited
    rss         unlimited      unlimited
    memlock     unlimited      unlimited
    maxproc     1418           2128
    maxfiles    256            unlimited
```
maxfilesが 256 になっているのが確認できました。
## 設定ファイルを作る
```/Library/LaunchDaemons/limit.maxfiles.plist``` に設定ファイルを作成する。

初期では、当該ファイルはないので新規に作成します。
```
$sudo vi /Library/LaunchDaemons/limit.maxfiles.plist
```
vi が開かれたら以下のようにペーストします。

##### limit.maxfiles.plist
```ruby
<?xml version="1.0" encoding="UTF-8"?>  
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">  
  <dict>
    <key>Label</key>
    <string>limit.maxfiles</string>
    <key>ProgramArguments</key>
    <array>
      <string>launchctl</string>
      <string>limit</string>
      <string>maxfiles</string>
      <string>524288</string>
      <string>524288</string>
    </array>
    <key>RunAtLoad</key>
    <true/>
    <key>ServiceIPC</key>
    <false/>
  </dict>
</plist>
```

## plistファイルを読み込んで適用させる。

```shell
$ sudo launchctl load -w /Library/LaunchDaemons/limit.maxfiles.plist
```

## 適用されたことを確認する

```
$ sudo launchctl limit
    cpu         unlimited      unlimited
    filesize    unlimited      unlimited
    data        unlimited      unlimited
    stack       8388608        67104768
    core        0              unlimited
    rss         unlimited      unlimited
    memlock     unlimited      unlimited
    maxproc     1418           2128
    maxfiles    524288         524288
```

maxfilesが 524288 に増えていることが確認できました。

