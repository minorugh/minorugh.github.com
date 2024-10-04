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
### 1.1. わたしの環境
このドキュメントを参考に dotfilesを構築されるときに環境による差異が発生する可能性が高いので私の使っている環境を書いておきます。

* Debian 12.7  86_64 GNU/Linux
* ThinkPad P1 Gen1 i7/32GB/1TB
* ThinkPad X250 i5/16GB/500GB
* zsh 5.9
* vim 9.0
* GNU Emacs 29.4

## 2. dotfilesとは？
dotfilesとは、ホームディレクトリに置いてあるドット（.）から始まる設定ファイル（.bashrcとか）を管理しているリポジトリのことです。シェルやエディタの設定からアプリケーションの設定まで幅広いものが置かれています。

dotfilesを極めることでメインマシンを更新したりサブマシーンに同じ環境を移植したりするときに日々育て上げた快適な作業環境を瞬時にサクっと再生・移植することが出来ます。これからの時代の開発環境には効率的な dotfilesが必須だと言えます。

### 2.1. dotfilesの構成
dotfilesの詳しい作り方は後述しますが、私の今の dotfilesは以下のような構成になっています。

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

### 2.3. dotfilesの使い方
#### 2.3.1. 日々の作業の中で育てる
1. 作業をしているとき、「もっと効率を上げられないか、自動化できないか」などと考えたり感じたりする。
2. そんなときネットで調べてヒントを見つけ出す。
3. 見つけたヒントをもとに自分の環境にあった設定に修正して dotfilesの当該設定ファイルに入力する。
4. 設定ファイルにはシンボリックリンクが貼られているので再起動すれば即反映されるはず。
5. gitが差分を検知しているはずなので git add, commit, pushを行い変更を remoteに反映しておく。
6. 斯くして dotfilesは日々練られ育てられていくのである。

#### 2.3.2. 環境を再構築する
1. OSをクリーンインストールしたり、新しいPCにインストールするときに dotfilesからリストアする。
2. GitHubから `git clone https://github.com/xxx/dotfiles.git` で dotfilesを持ってくる。
3. 簡単な事前準備を手作業で処理したあと makeを走らせると自動的に環境がリストアされる。
4. はい、これでもう自分の環境が構築できている。

#### 2.3.3. 注意点
* このドキュメントはあくまで私個人を前提にしたものなので、もし複数人でアカウントを共有している環境で作業しているような場合は他の人の許可を得た上でdotfilesを展開しないと混乱が生じる。(他の人がいきなり環境が変わってびっくりしてしまうため)

* 私の場合、複数端末で dotfilesを共有しているが、変更を`git push`するのは特定のメインマシンからと限定しておきサブマシンへは適宜 `git pull` して最新状態に更新するようにするとよい。もちろんブランチを使って複数管理することも可能だが私の場合はよく操作ミスして躓くことが多いので前者の対応をしている。

* パスワード、API、Token などを生のままで Githubのリポジトリで公開するのはセキュリティー上まずいので cryp関数を利用して暗号化して pushするのが一般的です。私の場合は面倒くさいので Dropboxに保存してシンボリックリンクを貼るようにしています。Dropbox自身の安全性を言い出すと限がないので割り切っています。
 
## 3. dotfilesの作り方
dotfilesってのはわかったけど、具体的な手順がわからないという人のために簡単な作り方を書説明します。

* GitHubにてdotfilesという名前のリポジトリを作成する（READMEだけは作成しておく）
* 自分のPCにdotfilesのリポジトリを git cloneしてくる（私の場合なら `~/src/github.com/minrugh/dotfiles'）
* とりあえず、使っているシェルの設定（~/.bashrc）をdotfiles以下にコピーする
* その他必要なドットファイルや設定をデレクトリー構成ごとコピーします（前項の構成を参考に…）
* あとはdotfiles以下でコミットしてpushしたら出来上がり


## 4. make実行の前の手作業
Makefileを作成し実行させる前に手作業でするべきことをまとめました。

### 4.1. USBからブートしてDebian最新版をネットインストールする

### 4.2. ユーザー名をsudoersに登録する

### 4.3. ホームのサブディレクトリを英語表記にする

### 4.4. Dropboxのインストールと設定

### 4.5. dotfileの復元


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
