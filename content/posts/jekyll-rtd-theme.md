+++
date = "2021-08-29T07:31:24+09:00"
categories = ["emacs"]
tags = ["jekyll","github"]
title = "GitHub Pagesにサイトを開設"

+++
Emacsの設定に関する情報を備忘録としてまとめたので GitHub Pagesに置くことにした。

GitHub Pagesの場合は Jekyllプロジェクトを pushするだけで HTMLに変換してくれるらしい…ということなので試してみたらすんなり成功した。

* [minorugh.github.io](https://minorugh.github.io/) 

![Alt Text](https://live.staticflickr.com/65535/51408876556_2d6460a776_b.jpg) 

jekyllには、sphinx_rtd_theme に似た [jekyll-rtd-theme](https://jamstackthemes.dev/theme/jekyll-rtd-theme/) があるのでこれを採用した。


## Deplpy環境
この themeは特殊なのか、設定がまちがっているのかよくわからなかったが、ローカルでのプレビューはエラーが出てうまく行かなかった。

結局、git pushするだけになったので `deploy.sh` と `makefile` を書いて自動化しました。deployに成功すると自動的にプラウザーで GitHub Pqgesを開くというものです。markdownファイルの編集が終わったら `make k` するだけで自動処理します。

## deploy.sh
```shell
#!/bin/bash
git checkout master
git add -A
git commit -m "Update blog"
git push origin master
```
## makefile

```makefile
a.out: github

github:
	~/src/github.com/minorugh/minorugh.github.io/deploy.sh
	chromium https://minorugh.github.io
```
