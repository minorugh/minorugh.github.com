+++
date = "2022-01-26T10:29:22+09:00"
categories = ["tech"]
tags = ["slack", "linux"]
title = "SlackAppを最小化状態で自動起動する for Linux"
+++

元ネタ
[SlackAppを最小化状態で自動起動する](https://bre.is/tEgwhUeT) 


SlackAppを自動起動にしている場合は `~/.config/autostart/slack.desktop` と言うファイルがあります。 私の環境では、`slack.desktop` は `/usr/share/applications/slack.desktop` に存在していました。 なので、編集するには `sudo`` が必要です。

```conf
[Desktop Entry]
Name=Slack
StartupWMClass=Slack
Comment=Slack Desktop
GenericName=Slack Client for Linux
Exec=/usr/bin/slack %U
Icon=/usr/share/pixmaps/slack.png
Type=Application
StartupNotify=true
Categories=GNOME;GTK;Network;InstantMessaging;
MimeType=x-scheme-handler/slack;
```

Exec が実際に実行するコマンドを指定しています。`/usr/bin/slack` が本体なので、この直後に `--startup` を挿入すれば最小化状態で自動起動するようになります。

```conf
Exec=/usr/bin/slack --startup %U
```
おそらく、SlackAppをアップデートすると `/usr/share/applications/slack.desktop` は上書きされて `--startup` が消えるので、`~/.config/autostart/` にはシンボリックリンクではなく、コピーを置くなどのしたほうがいいです。
