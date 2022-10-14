+++
date = "2022-09-22T21:41:06+09:00"
categories = ["tech"]
tags = ["Github","Dropbox","git"]
title = "Git と Dropbox の連携"
+++

重要なファイル群をDropboxに配置し複数端末で共有している。Dropbox だけでも多少の履歴は辿れるけれど、さらに保険をかける意味でGitHubにもリポジトリを作ってみようと思った。

Dropbox内にgitツリーを置くとややこしくなろそうなのでDropbox外の別デレクトリにtreeをおくというTipsを見つけて真似してみた。

* [Dropbox で Git のワークツリーのみを同期する](https://amano41.hatenablog.jp/entry/syncing-git-with-dropbox-but-working-tree-only?utm_source=pocket_mylist) 

```shell
$ cd ~/Dropbox/project

$ git init --separate-git-dir /path/to/project.git
Initialized empty Git repository in /path/to/project.git/
```
上記を実行すると、Dropboxデレクトリにあった `.git` デレクトリは、/path/to/project.git に移動し、もとのデレクトリには、以下の内容の `.git` というファイルが生成される。内容はリンク先を書いただけのテキストファイルなのだが、Dropboxデレクトリでは普通に `magit-status`できる。

```
gitdir: /path/to/project.git
```

## リストアするとき
もし、将来ローカルリポジトリのPCをクリーン再インスールするケースを想定してテスト用のリポジトリで試してみた。

`mkdir /path/to && cd /path/to` して`git clone priject.git`する。
データリポジトリは、Dropboxをインストールして同期すれば、自動的にリストアできるので、`.git`デレクトリのみ残してデータは削除して良い。
