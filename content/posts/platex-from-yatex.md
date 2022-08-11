+++
date = "2018-09-29T14:12:56+09:00"
categories = ["emacs"]
tags = ["latex"]
title = "YaTeXからplatexを使えるようにする"
+++

MacのTex環境（BasicTex）を2017basicからを2018basiにバージョンアップさせようとしてつまずいたので備忘録を残しておきます。

今どきは、Pandocなどで手軽にmd2pdfとかするのが流行りのようですが、私の場合は俳句関係での利用がメインで縦書きやルビうち、書籍としての組版などもするので目的別に txt2tex.plを作成して、makefileでtexファイルを自動生成したあとYaTeXでPDFにするようにしています。

今回は、インストール済みの古いTex環境を全て削除したうえで新たにインストールしました。

<!--more-->

- [macでtex環境を削除](https://qiita.com/tetsuo_jp/items/04a66b8f42946b5c5a5a) 


## 動作環境

- OS: macOS High Sierra 10.13.6
- Emacs: emacs-26.1-mac-7.1 （Download from [Emacs-Mac-Port](https://github.com/railwaycat/homebrew-emacsmacport/releases)） 

## BasicTeX のインストール

Texlive のフルパッケージまでは必要ないのでが容量の少ないBasicTexをインストールします。いろんな導入記事を参考にしましたが、シンプルでわかりやすかった TEX Wiki のサイトを参考にしました。 

- [TEX Wikki BasicTex](https://texwiki.texjp.org/?BasicTeX) 

### BasicTeX.pkg でインストール

[More Packages - MacTeX](http://tug.org/mactex/morepackages.html) または [ミラーサイト (MacTeX)](https://texwiki.texjp.org/?MacTeX#mirror) から BasicTeX.pkg をダウンロードします。

BasicTeX のインストールの流れは以下のとおりです。

1. ダウンロードした BasicTeX.pkg をダブルクリックしてインストールします。
2. ターミナル上で sudo tlmgr update --self --all を実行し，最新版までアップデートします。
3. ターミナル上で sudo tlmgr paper a4 を実行してデフォルトの用紙サイズを A4 に変更します。
4. **【重要】BasicTeX には標準で日本語 pTeX / upTeX が入っていません。**これらを使えるようにするために collection-langjapanese を追加でインストールします。collection-langjapanese をインストールすれば，依存関係で collection-langcjk もインストールされます。

上記ターミナル上での手順をまとめると以下のとおりです。

```shell
sudo tlmgr update --self --all 
sudo tlmgr paper a4 
sudo tlmgr install collection-langjapanese
```

## フォント関係の設定
以下の記事を参考にしました。

- [MacTeX 2018 のインストール＆日本語環境構築法](http://doratex.hatenablog.jp/entry/20180501/1525144736#5) 


### TLContrib からの追加インストール
続いて，TLContrib をレポジトリとして登録し，そこからmacOS／ヒラギノ関連のパッケージをダウンロードします。

```shell
sudo tlmgr repository add http://contrib.texlive.info/current tlcontrib
sudo tlmgr pinning add tlcontrib '*'
sudo tlmgr install japanese-otf-nonfree japanese-otf-uptex-nonfree ptex-fontmaps-macos cjk-gs-integrate-macos
```

### ヒラギノフォントの準備
続いて，macOS に標準で用意されている美しいヒラギノフォントを TeX で使用するため，次の一連のコマンドを実行します。

```shell
sudo cjk-gs-integrate --link-texmf --cleanup --force
sudo cjk-gs-integrate-macos --link-texmf --force
sudo mktexlsr
```

### ヒラギノフォントの埋め込み設定

ヒラギノフォントを埋め込んだPDFを作成するために、次のコマンドを実行します。
私の場合はmacOS 10.13 High Sierra ですが、macOS のバージョンによって実行するコマンドが異なります。macOSの他のバージョンでの設定は、前述の参考サイトの記事を見てください。


##### macOS 10.13 High Sierra の場合

```shell
sudo kanji-config-updmap-sys --jis2004 hiragino-highsierra-pron
```


以上で，(u)pLaTeX + dvipdfmx によって，美しいヒラギノフォントを埋め込んだ和文PDFを作成できるようになりました。

## 自前のスタイルファイルの置き場所

私の場合、縦書き文書での利用が多く、バイブルとして下記の書を座右においています。

- [藤田眞作著：pLaTeX2ε入門・縦横文書術](http://xymtex.my.coocan.jp/fujitas2/texlatex/index.html) 

そこには、縦組み関係のスタイルシートが多く紹介されていて [著者のホームページ](http://xymtex.my.coocan.jp/fujitas2/texlatex/index.html) から必要なものをダウンロードできます。でもそれらはわかりやすい場所に置いておきたいと考えて下記の記事を参考にして専用のディレクトリを作りました。

- [Mac OS でのスタイルファイル（.sty）の置き場所](http://ijairai.hatenablog.com/entry/2015/07/31/235043) 

## YaTeXの設定
参考までに私の Emacs:YaTeXの設定を貼っておきます。

````lisp
(use-package yatex)
(add-to-list 'auto-mode-alist '("\\.tex\\'" . yatex-mode))

;; Script for opening PDF generated from dvi in ​​Preview.app
;; Create dvpd.sh as follows and place it in /usr/local/bin
;; | #!/bin/bash
;; | name=$1
;; | dvipdfmx $1 && open -a Preview.app ${name%.*}.pdf

(setq tex-command "platex"
      dviprint-command-format "dvpd.sh %s"
      YaTeX-kanji-code nil
      YaTeX-latex-message-code 'utf-8
      Section-name "documentclass"
      makeindex-command "mendex"
      YaTeX-use-AMS-LaTeX t
      YaTeX-use-LaTeX2e t
      YaTeX-use-font-lock t)

(add-hook 'yatex-mode-hook
   (lambda()
    (use-package yatexprc)
    (local-set-key [(M c)] 'YaTeX-typeset-buffer)   ;; Type set
    (local-set-key [(M l)] 'YaTeX-lpr)))            ;; Open PDF file
````


上記の設定で何をしているかというと

- Emacsでtexファイルを開き、YaTeX-typeset-bufferを実行するとplatexが実行されてdviファイルが生成されます。続いて、YaTeX-lprを実行するとdvpd.shが呼ばれてdvi->PDFが実行され、生成されたPDFをMacのPreview.appで自動的に開きます。 
- これの自動化を実現させるためにあらかじめdvpd.shを次のように作成し、/usr/local/binに置きます。当然PATHが通っている必要があります。ターミナルで、/usr/local/binに移動し、```` chmod +x dvpd.sh```` を実行して dvpd.shをコマンドとして使うために実行権限を与えます。

````sh
#!/bin/bash
# 生成されたPDFをPreview.appで開く
name=$1
dvipdfmx $1 && open -a Preview.app ${name%.*}.pdf
# 不要ファイルを削除
rm *.au*
rm *.dv*
rm *.lo*
rm *.ou*
rm *.to*
````

以上で、快適なYaTeX & platex環境が実現します。
