+++
date = "2021-08-21T17:34:15+09:00"
categories = ["emacs"]
tags = ["emacs"]
title = "Emacs27.2をInstall"
+++
Emacs27.2の安定版がリリースされたのでInstallした。

私の場合は、makefileを下記のように修正してmakeコマンドを実行するだけで簡単にInstallできるようにしている。


```makefile
## makefile
emacs-latest: ## Install latest version of emacs
	cd ${HOME}/src;\
	wget https://ftp.gnu.org/pub/gnu/emacs/emacs-27.2.tar.gz;\
	tar -xzvf emacs-27.2.tar.gz;\
	cd emacs-27.2;\
	./configure;\
	make;\
	sudo make install;\

```

```shell
$ make emacs-latest

```
## スクリーンショット
Emacs27.2を起動し、Dashboard画面を表示させたスクショです。

![Alt Text](https://live.staticflickr.com/65535/51396304028_74698110f6_b.jpg) 

* Debianも10（buster）から11（bullseye）になったみたいなのでアップデートする予定。
