+++
date = "2022-01-23T12:10:02+09:00"
categories = ["tech"]
tags = ["ssh"]
title = "GUIの起動時にパスフレーズを自動入力して ssh-add を実行させる"
+++
Emacsを作業テーブルとして常用的に SSHを使うので、ログイン時にパスフレーズを自動入力させて ssh-addを実行させるように環境構築したので備忘録を残しておく。

Debian Linux環境であるので他の環境の場合は、それぞれに応じた工夫がいるかと思う。

1. autologin.sh の設定（shell script）
2. autostart.desktop の設定（Linuxの自動起動）


## autologin.sh
expectを利用した標準的なスクリプトです。 

```shell
#!/usr/bin/expect
# Run ssh-add with passphrase auto input at GUI startup
# Look ${PWD}/.config/autostart/autologin.desktop

# Include Password
source ~/Dropbox/backup/zsh/env.sh

spawn ssh-add /home/minoru/.ssh/id_rsa
expect "Enter passphrase for /home/minoru/.ssh/id_rsa:"
send "${PW}\n";
interact
```
autologin.sh はどこに配置してもよいのですが、私の場合は、ホームデレクトリに置きました。

作成後、動作権限の付与を忘れないように。

```shell
$ chmod 600 autologin.sh
```

スクリプトは GitHubで公開しているので passphraseは直接書かないで別ファイルとしてインクルードしている。

include するファイルは下記のような内容である。

```shell
## Password file to include in ~/.autologin.sh
set PW "<your passphrase>"
```
`your passphrase` には、秘密鍵 id_rsa の作成時に設定したパスワードを書きます。パスワードは長いほど解読されにくくて安全なのですが、長すぎると暗号化や復号処理に過大な負荷がかかります。私の場合は安全を考えて４０桁にしましたが今の所無理なく機能しているようです。

## autostart.desktop
GUIログイン時に autologin.shが自動起動されるように設定します。

debian Linuxの GUI「セッションと起動」に直接設定してもいいのですが、下記のような autostart.desktopファイル作成したほうが簡単です。

```conf
[Desktop Entry]
 Version=1.0
 Name=AutoLogin
 Comment=Run ssh-add with passphrase auto input at GUI startup
 Exec=gnome-terminal -- zsh -c "expect $HOME/.autologin.sh"
 Icon=utilities-terminal
 Terminal=false
 Type=Application
 Categories=Application;
```
これを、`~/.config/autostart` フォルダーに保存します。上記の設定の場合、gnome-terminal は、コマンド実行後すぐに閉じます。

上記ファイルを保存してログインし直すと瞬間的に実行される様子が分かります。メニューから「設定」→「セッションと起動」と開き、「自動開始アプリケーション」のタブを開くと設定が反映されているのを確認できます。
