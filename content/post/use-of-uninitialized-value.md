+++
date = "2019-01-13T18:36:50+09:00"
categories = ["tech"]
tags = ["perl"]
title = "Perlで未定義値かどうかを判定する方法"
+++

機嫌よく動いていた自作のスクリプトに 

```perl
use warnings; 
```

を入れたら作業そのものは止まることなく終了するのだけれど、

```perl
Use of uninitialized value ... 
```
と叱られるようになった。

```perl
foreach my $line (@log){
    chomp($line);
    my ($no,$name,$date,$mail,$comment,$pass)=split(/,/,$line);
    # $comment が空だった場合の処理
    if($comment eq ""){
	$comment=$no;$c++
    }
    if ($c > 7){  # 7日分を取得したら繰り返し終了
	last
    }
	....
```
上記を走らせたときに下記の部分で前述の警告が出る。

```perl
if($comment eq ""){
	$comment=$no;$c++
}
```
$comment が存在しなかったときに他の変数を代入する…

という条件式のつもりでしたが、存在しない（未定義の）変数だよ！ という警告なのである。落ち着いて考えたら当たり前ですね。ググってみたら未定義値かどうかを判定する 
[defined関数](https://www.javadrive.jp/perlfunc/other/index1.html) 
 というのがあるのを知りました。

```perl
if (defined $name) {
  ...
}
```

上記では定義されている場合に処理させるというものですが、私の場合は値が未定義だった場合に処理をさせたいので 
[unless](https://www.javadrive.jp/perl/if/index6.html) 
 を使えばいいという情報を得たので下記のように修正して警告が出なくなりました。めでたし(^o^)


```perl
# $comment が未定義だった場合の処理
unless (defined $comment){
	$comment=$no;$c++
}
```



