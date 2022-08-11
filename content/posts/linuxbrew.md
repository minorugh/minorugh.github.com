+++
date = "2021-11-25T10:16:28+09:00"
categories = ["tech"]
tags = ["linuxbrew"]
title = "Debian11 (bullseye)に Linuxbrewを install"
+++
GitHub CLIツール ghをインストールするのに brewの方が簡単だったので Debianに Linuxbrewをインストールすることにした。
Mac時代に慣れ親しんだ Homebrewが Linuxからも使えるのは感動でした。

## install

brewインストールの前に依存ツールをダウンロードしておきます。私の場合は、既にインストール済みだったので省略しました。

```shell
% sudo apt install build-essential curl file git
```
つづいて公式にあるように下記でインストールできます。

```shell
% sh -c "$(curl -fsSL https://raw.githubusercontent.com/Linuxbrew/install/master/install.sh)"
```

## warning at install

install時に `Warning: /home/linuxbrew/.linuxbrew/bin is not in your PATH.` パスがないと怒られますので、`.bashrc` or `.zshrc` に PATHを追加します。私の場合は、zshなので下記のようになります。

```shell
% echo 'export PATH="/home/linuxbrew/.linuxbrew/bin:$PATH"' >>  ~/.zshrc
% source ~/.zshrc
```
## Confirmation after install

brewが正常に installされたことを確認しておきます。

```shell
% which brew
/home/linuxbrew/.linuxbrew/bin/brew
```
PATHが通っていることが確認できました。

```shell
% brew help
Example usage:
  brew search TEXT|/REGEX/
  brew info [FORMULA|CASK...]
  brew install FORMULA|CASK...
  brew update
  brew upgrade [FORMULA|CASK...]
  brew uninstall FORMULA|CASK...
  brew list [FORMULA|CASK...]

Troubleshooting:
  brew config
  brew doctor
  brew install --verbose --debug FORMULA|CASK

Contributing:
  brew create URL [--no-fetch]
  brew edit [FORMULA|CASK...]

Further help:
  brew commands
  brew help [COMMAND]
  man brew
  https://docs.brew.sh
```
helpコマンドが表示されたら正常です。つづいて update確認しておきます。

```shell
% brew update
Already up-to-date.
```

最後に `brew doctor` を実行します。

私の場合は、あれこれと Warningが出たので、指示されたものを順次削除しました。

```shell
% brew doctor
Your system is ready to brew.
```
ここまでこぎつければ Linuxbrewのインストールは完了です。
