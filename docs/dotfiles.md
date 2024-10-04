---
layout: default
title: Dotfiles
---

# Dotfiles for Debian Linux

## 1. はじめに
```note
* ここは [@minoruGH](https://twitter.com/minorugh) の Dotfiles の一部を解説しているページです。
* [makefile](https://github.com/minorugh/dotfiles/blob/main/Makefile) と Dotfils本体は、[GitHub](https://github.com/minorugh/dotfiles) に公開しています。
* この ditfiles は Debian Linux 用です。[masasam/dotfiles](https://github.com/masasam/dotfiles) を参考に作成しました

```

## 2. 動作環境
以下の環境で使用しています。が、動作を保証するものではありません。

* ThinkPad P1 Gen1 i7/32GB/1TB
* Debian 12.7  86_64 GNU/Linux
* 自分でビルドした GNU Emacs 29.4

## 3. Makefileで環境を構築してみよう
Make による自動化はカスタマイズが非常に簡単なのでお勧めします。

## 4. Makefileで環境構築する手順

### インストールUSBからdebianをクリーンインストール
* 
### makeを実行する前に手動での準備が必要

### ターミナルから下記の手順でmakefileを走らせます。

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
