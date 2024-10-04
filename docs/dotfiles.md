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

### 2.2. dotfilesの作り方
dotfilesってのはわかったけど、具体的な手順がわからないという人のために簡単な作り方を書説明します。

* GitHubにてdotfilesという名前のリポジトリを作成する（READMEだけは作成しておく）
* 自分のPCにdotfilesのリポジトリを git cloneしてくる（私の場合なら `~/src/github.com/minrugh/dotfiles'）
* とりあえず、使っているシェルの設定（~/.bashrc）をdotfiles以下にコピーする
* その他必要なドットファイルや設定をデレクトリー構成ごとコピーします（前項の構成を参考に…）
* あとはdotfiles以下でコミットしてpushしたら出来上がり

### 2.3. dotfilesの使い方
#### 2.3.1. 更新時
* 作業をしているとき、ふと「なんか生産性落ちてる気がする。。。効率を上げられないかな」と考える・感じる (たとえば、ターミナルでコマンド履歴から前のコマンドを上矢印で探してくるのめんどくさいなーとか)
* ネットで調べて効率が上がる設定を見つけ出す! (fzfを使い履歴を簡単に検索する方法がヒット。なるほど、履歴をファジーサーチできるようにしてそれをショートカットとして登録すればいいのか！)
* 該当の方法を設定ファイルに入力する (この場合だとシェルの設定、.bashrc, .zshrcを更新する)
* シンボリックリンクとなっているはずなので、gitが差分を検知しているはず
git add, commit, pushを行いremoteに反映する

#### 2.3.2. 構築時
* GitHubからgit clone https://github.com/xxx/dotfiles.git でdotfilesを持ってくる
* インストーラー(後述)を起動する
* はい、もう自分の環境が構築できている！

#### 2.3.3. 注意点
* 複数人でアカウントを共有している環境で作業している場合は他の人の許可を得た上でdotfilesを展開したほうがいい。(他の人がいきなり環境が変わってびっくりしてしまうため)

* 自分だけで使いたい場合は export HOME=/your/home/dir と自分だけのディレクトリにHOMEを変更してそこで使えば他の人への影響を防ぐことができる。(ただしシェルにログインするたび毎回 export を実行する必要がある)

* あまりガチガチに設定したくない、他の人とも共有したいという環境用に別のdotfilesを作っておくのもおすすめ。たとえば、誰もが使いたいようなgitのステータス情報やコマンドの実行時刻の情報をシェルのプロンプトに出したものを共有用のdotfilesにしておくなど。
https://github.com/yutkat/dotfiles-mini
 

## 3. make実行の前の手作業
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
