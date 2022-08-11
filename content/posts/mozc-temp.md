+++
date = "2017-05-25T19:48:12+09:00"
categories = ["emacs"]
tags = ["mozc"]
title ="mozc-tempを使ったモードレス日本語入力が快適"
+++

モードレスに魅せられて、ac-mozcを導入して使っていたが、新しいモードレスのコードで、mozc-tempなるものがあることを知った。早速使って見た。


* https://github.com/HKey/mozc-temp 

mozc-tempのREADMEを見るとコンセプトがよく分かる。

>
mozc-tempはmozc.elによる入力をモードレス化するラッパーです。
ac-mozcをもとに作成されました。
基本的な挙動はac-mozcと同じになるように作られていて、全角文字と半角文字の混在する文章の入力を楽にすることを目的としています。
ac-mozcとの違いは、これがmozc.elのインターフェイスに対するラッパーであるということです。 そのため、変換時の候補選択はmozc.elのものと同じ操作が可能です。

![Alt Text](https://github.com/HKey/mozc-temp/raw/master/images/screencast.gif) 

<hr>

## 導入
パッケージインストールで mozc-temp.elを入れて、init.elに以下を設定するだけ。

```
(global-set-key (kbd "s-j") #'toggle-input-method)
(global-set-key (kbd "s-m") #'mozc-temp-convert)
```

<br />
キーバインドは何でもいいが、わたしの場合は「⌘英かな」というアプリで macbookの右commadキーにこれを割り当てて使っている。左commandキーには、mozc-modeのon/offが出来るように設定しておくと便利です。

![Alt Text](https://c1.staticflickr.com/5/4195/34664022620_7b83530565.jpg) 

## 感想
ac-mozcを使ってモードレスの環境を試したことはあったが、拗音（っ)とかの変換がうまくできないので中途半端な印象でした。
その点mozc-tempはそうした問題もなくとても使いやすいです。

Emacsで快適にmozcを使うためには、Emacs使用時は、他の日本語入力メソッドが同時に機能しないようにコントロールすることがとても大切です。それぞれの環境によって工夫が必要ですが、わたしの場合（MaC）について、別Tipsで紹介していますので参考にしてください。

[Mac+Emacs で emacs-mozc をかなキーで ON/OFF させる裏技](https://snap.textgh.org/201710031643/) 




