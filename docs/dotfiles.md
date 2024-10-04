---
layout: default
title: Dotfiles
---

# Dotfiles for Debian Linux

## 1. はじめに
```note
* ここは [@minoruGH](https://twitter.com/minorugh) の dotfiles の一部を解説しているページです。
* 私の ditfiles は Debian Linux 用です。[masasam/dotfiles](https://github.com/masasam/dotfiles) を参考に作成しました。
* dotfils本体は、[GitHub](https://github.com/minorugh/dotfiles) に公開しています。

```
### 1.1. dotfilesとは？
dotfilesとは、ホームディレクトリに置いてあるドット(.)から始まる設定ファイル(.bashrcとか)を管理しているリポジトリのことです。シェルやエディタの設定からアプリケーションの設定まで幅広いものが置かれています。

dotfilesを極めることで、メインマシンの更新時や複数端末などで自分の育て上げた快適な作業環境を瞬時にサクっと再生したり移植することができます。これからの時代の働き方には効率的なdotfilesが必須だと言えます。


### 1.2. わたしの環境
このドキュメントを参考に dotfilesを構築されるときに環境による差異が発生する可能性が高いので私の使っている環境を書いておきます。

* Debian 12.7  86_64 GNU/Linux
* ThinkPad P1 Gen1 i7/32GB/1TB
* ThinkPad X250 i5/16GB/500GB
* zsh 5.9
* vim 9.0
* GNU Emacs 29.4

## 2. dotfilesの作り方
dotfilesの構成と実際の作り方を説明します。

### 2.1. dotfilesの構成
Githubに構築したリポジトリをGit管理する上で間違いを避ける意味でローカルマシンにも同じデレクトリー構成 `~/src/github.com/<ユーザーID>/' を作成しておきます。

dotfilesの詳しい作り方は後述しますが、出来上がった私の今の dotfilesは以下のような構成になっています。
```codesession
~/src/github.com/minorugh/dotfiles
│
├── .config/
├── .emacs.d/
├── .font/
├── .git/
├── .gnupg/
├── .local/
├── .vim/
├── bin/
├── devils/
├── etc/
├── tex/
├── .Xmodmap
├── .Xresources
├── .autologin.sh
├── .bashrc
├── .gitignore
├── .tmux.conf
├── .vimrce
├── .zprofile
├── .zshrc
├── Makefile
└── README.md

```



Make による自動化はカスタマイズが非常に簡単なのでお勧めします。

## 3. dotfilesからmakeで自動的に環境構築する
makefileで自動実行させる前に以下のような手順が必要です。
* 
* 3.1. 予めOSインストール用のUSBを作成しておきます。
* 3.2. インストールUSBからOS（debian/macOSなど）をクリーンインストールします。
* 3.3. makeで自動実行させる前にいくつかの準備を手動で行います。
* 3.4. ターミナルからmakefileを走らせる。

```
## =====================================================================
## Manual setting before executing make
## =====================================================================
## 1. Boot from USB to netinstall Debian latest
# Create installation USB from netinst iso image, use Rufs.exe on Windows
# Download firmware from https://bre.is/f2LBmD3t
# Unzip firmware.zip, then paste to firmware directory of install USB

## 2. Register username to sudoers
# Log in as root
# | gpasswd -a ${USER} sudo
# | logout
# | sudo visudo ## edit sudoers file to [%sudo  ALL=(ALL:ALL) NOPASSWD:ALL]
# | log out

## 3. Set home sub directorys to English notation
# Log in with ${USER}
# | sudo apt install -y xdg-user-dirs-gtk
# | LANG=C xdg-user-dirs-gtk-update --force
# | sudo apt update
# | sudo apt install -y zsh git make nautilus
# | chsh -s /bin/zsh

## 4. Install dropbox & setting
# | sudo apt install -y nautilus-dropbox
# | Launch dropbox from Menu then install and setting

## 5. Restore dotfiles
# | mkdir -p ~/src/github.com/minorugh
# | cd ~/src/github.com/minorugh
# | git clone https://github.com/minorugh/dotfiles.git
# | cd dotfiles
# | make all

## =====================================================================
## Run make from here
## =====================================================================

After this, refer to makefile
```

## Restore procedure with makefile
How to restore with makefile,Please refer to 
[masasam/dotfiles](https://github.com/masasam/dotfiles). 

## My Emacs configuration 
Detailed explanation is written at the following URL.

* [https://minorugh.github.io](https://minorugh.github.io/init.html) 

----

## update infomeition
* 2021.11.01 Remote repository also on xserver (simultaneous Push)
* 2021.10.11 Content organization
* 2021.08.26 Update to Debian 11(bullseye)
* 2021.08.26 Update to emacs 27.2
* 2021.02.20 Update to emacs 27.1
* 2021.01.29 Fixed mozc
* 2021.01.28 Fixed so that it can be shared between two Thinkpads
* 2020.11.10 Rebuilding
* 2020.10.27 first commit
