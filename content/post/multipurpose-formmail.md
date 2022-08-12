+++
date = "2018-11-05T17:09:06+09:00"
categories = ["tech"]
tags = ["perl"]
title = "Formをhtmlに直接表示しないでcgiで別窓表示する"
+++
読者からのFeedbackを受け付けるためにホームページ上のコンテンツごとにメールフォームを貼っていた。
SPAM対策はしてあったがそれでもそれをくぐり抜けて攻撃されることが多くなったので、各ページにフォームを貼り付けるのはやめてCGIで表示させるようにした。

<!--more-->
##### form.cgi
```perl
#!/usr/bin/perl

use Encode 'decode';
$subject = @ARGV[0];

print "Content-type: text/html; charset=utf-8\n\n";
print<<"EOF";
<form method="post" action="http://*******/send.cgi" onsubmit="return sendmail(this);">
<!--ここに$subjectに応じた画面表示用のfrom.htmlを書く -->
EOF
```
```form.cgi?subject``` で起動させると、引数（例えばコンテンツ名）の値が ```@ARGV[0];``` に保存される。
私の例では、```$subject``` がそれになる。

form.cgiスクリプトの中で引数の値に応じて分岐させることで、引数に応じて全く異なる画面表示にすることも可能である。
私の場合は、各コンテンツのページタイトルを引数$subjectとして指定するようにしている。

## Screenshot

```form.cgi?みのるの日記``` でアクセスした場合の表示画面をスクショで撮ったので貼っておく。

下の例では、「みのるの日記」 部分の表示が ```$subject``` によって変化するのである。

<img src="https://c1.staticflickr.com/5/4831/44814872015_7abb4d2338_b.jpg"> 





