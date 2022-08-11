+++
date = "2021-09-26T07:24:50+09:00"
categories = ["tech"]
tags = ["debian"]
title = "Debian でコンソール画面の解像度を変更する"
+++

高解像度の pcで Debianを使うと GRUBの起動画面がとても小さくて見にくいので解像度を 800x600に変更した。

```
# /etc/default/grub
```

```
# コメントアウトを外して好みの解像度に
GRUB_GFXMODE=800x600
# こちらは追記
GRUB_GFXPAYLOAD_LINUX=keep
```

```
# update-grub2
# reboot
```

## 参考

[Debian でコンソール画面の解像度を変更する](https://bre.is/bJdJJa6E) 
