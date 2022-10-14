+++
date = "2022-10-05T14:08:49+09:00"
categories = ["docker"]
tags = [""]
title = "Dockerを利用してCGI/SSI が動作するWEBサーバーを構築"
+++
Dockerの勉強を兼ねてCGI動作確認用のローカルWEBサーバーを構築してみた。

Docker Web サーバーの構築例は、`nginx` を使ったものが多かったが、自分が利用しているレンタルサーバー（xserver）の環境にあわせる必要があったので `Apache` で構成することにした。多くのTipsのうち偶然見つけた下記の記事がわかりやすく比較的楽に導入に成功した。

* [Docker Desktop for Windows＝CGIお試し開発環境＝](https://getpocket.com/read/3714179719) 

## dockerインストール
自分の環境（Debian11）へのDockerインストールの手順は、下記`makefile` の通り。
ついでにdocker-composeもインストールして使えるようにした。

```makefile
## makefile
docker: ## Install docker
	sudo apt-get install ca-certificates lsb-release
	curl -fsSL https://download.docker.com/linux/debian/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
	echo "deb [arch=amd64 signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/debian  $$(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list
	sudo apt update
	sudo apt install docker-ce docker-ce-cli containerd.io

docker-compose: ## Install docker-compose
	sudo curl -L "https://github.com/docker/compose/releases/download/v2.0.1/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
	sudo chmod +x /usr/local/bin/docker-compose
```
## dockerコマンドをsudoなしで実行できるようにする
dockerコマンドを入力するたびにsudoをつけるのは面倒なので下記の設定を行う。

```shell
# dockerグループを作る
$ sudo groupadd docker
# dockerを操作するユーザ(ここでは今のユーザ)をdockerグループに所属させる
$ sudo gpasswd -a $USER docker
# dockerデーモンを再起動する
$ sudo restart docker
# 再ログインすると反映される
$ exit
# Dockerをブート時に自動起動させる
$ sudo systemctl enable --now docker
```

頻繁の使うことになるコマンドは、`zshrc` (または`bashrc`)に、aliasを設定しておくと更に便利

```zshrc
# zshrc
# For docker-compose
alias dcbd="docker-compose up -d --build"
alias dcup="docker-compose up -d"
alias dcdn="docker-compose down -v"
alias dcps="docker-compose ps"
alias dcex="docker container exec -it <コンテナ名> bash"
```

## dockerファイル構成
```shell
webcgi
├──server
│    ├──apache
│    │    └──000-default.conf
│    ├──php
│    │    └──php.ini
│    └──Dockerfile
├──www
│    └──html
│        ├──index.html
│        ├──info.php
│        ├──index.pl
│        ├──index.py
│        └──index.rb
└──docker-compose.yml
```
## How to use

1. download ZIP && unzip
2. mv docker-webcgi-main YOUR_PROJECT_NAME
3. cd YOUR_PROJECT_NAME
4. docker-compose up -d --build

## files

### docker-compose.yml
```shell
version: '3'
services:
  webcgi:
    container_name: webcgi

    build: ./server
    ports:
     - 80:80
    restart: always

    volumes:
       - ./www:/var/www
```
### server/apache/000-default.conf
```shell
Alias /html/ "/var/www/html/"
<Directory "/var/www/html"> 
   AllowOverride All
   Options +ExecCGI +Includes
   AddType text/html .html
   AddHandler server-parsed .html
   AddHandler cgi-script .cgi .pl .py .rb
</Directory>
```
### server/php/php.ini
```shell
[Date]
date.timezone = "Asia/Tokyo"
[mbstring]
mbstring.internal_encoding = "UTF-8"
mbstring.language = "Japanese"
[error]
display_errors = Off
display_startup_errors = Off
```
### server/dockerfile 
```shell
# dockerイメージの指定
FROM php:7.4-apache

# 設定ファイルをdockerコンテナ内のPHPとApacheに読み込ませる
COPY ./php/php.ini /usr/local/etc/php/
COPY ./apache/000-default.conf /etc/apache2/conf-available/

# インストール
# `apt-get install`で任意の自身に必要なミドルウェアを指定しインストールする
# `a2enmod rewrite`でApacheの設定を.htaccessで上書きできるようにしている

RUN apt-get update \
&& apt-get install -y --no-install-recommends \
git \
zip \
unzip \
nano \
python3 \
ruby \
&& apt-get -y clean \
&& rm -rf /var/lib/apt/lists/* \
&& a2enmod cgid \
&& a2enmod include \
&& a2enmod rewrite \
&& a2enconf 000-default

RUN useradd -o -U -m -u 1000 minoru
WORKDIR /var/www
```
