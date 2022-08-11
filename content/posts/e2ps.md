+++
date = "2019-07-04T19:13:26+09:00"
categories = ["tech"]
tags = ["mac","linux"]
title = "Macにe2psをインストールする"
+++

[WtSeries : UNIXソフト](http://wtpage.info/wtseries/unix.html)からe2psをダウンロードして解凍します。
現在の最新バージョンは、e2ps-4.34

解凍フォルダー内のps-font.cをエディタで開き、14~53行の行末にバックスラッシュを追加します。

```c
char *gsFonts = "Times-Roman\
Times-Italic\
Times-Bold\
Times-BoldItalic\
Helvetica\
Helvetica-Oblique\
Helvetica-Bold\
Helvetica-BoldOblique\
Courier\
Courier-Oblique\
Courier-Bold\
Courier-BoldOblique\
Symbol\
AvantGarde-Book\
AvantGarde_BookOblique\
AvantGarde_Demi\
AvantGarde_DemiOblique\
Bookman-Light\
Bookman-LightItalic\
Bookman-Demi\
Bookman-DemiItalic\
Helvetica-Narrow\
Helvetica-Narrow-Oblique\
Helvetica-Narrow-Bold\
Helvetica-Narrow-BoldObliqeu\
NewCenturySchlbl-Roman\
NewCenturySchlbl-Italic\
NewCenturySchlbl-Bold\
NewCenturySchlbl-BoldItalic\
Palatino-Roman\
Palatino-Italic\
Palatino-Bold\
Palatino-BoldItalic\
Zapfchancery-MediumItalic\
ZapfDingbats\
Ryumin-Light.Hiragana\
Ryumin-Light.Katakana\
Gothic-Meduim.Hiragana\
Gothic-Medium.Katakana\
";
```

terminalで解凍フォルダー内に移動します。

```shell
cd e2ps-4.34
```

続いて以下のようにmakeします。

```shell
make
sudo make install
```

/usr/local/bin に e2ps と e2lpr がインストールされていたら成功です。

