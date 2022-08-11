+++
date = "2017-02-25T21:52:14+09:00"
categories = ["tech"]
tags = ["latex","perl"]
title = "PDFJモジュールをサーバーに置いてPerlでPDFを自動生成させる"
+++

## 目的
全自動で稼働させているインターネット句会の中で全員の投句が済んだあと、ランダムに並べ替えて清記表を作成する作業工程がある。

参加者はそれをプリントアウトして選句するのだけれど、俳句なので縦書き且つ美しくアウトプットさせたい。

手作業なら簡単にできるが、ネット上のシステムに自動的に作業させたい。

## PDFJモジュールを使う

PDFJが使えそうだと睨んだのでググってみた。

* http://blog.mukairiku.net/2010/01/pdfjモジュールを使ってperlから直接pdfファイルを出力しよう.html

レンタルサーバーに PDFJをインストールするわけにはいかないけど、モジュールをアップロードしてそれを呼び出す形でも上手く動きました。

```{bash}
#!/usr/local/bin/perl

# 句会場リセット用の日付ファイルを開いて読み込む
open(FILE,"../_datefrag.dat") or die "$!";
my $ctoday = <FILE>;
close(FILE);

# 今日のPDFファイルの有無チェック（重複作成させない）
my $filename = "./pdf/$ctoday\.pdf";
unless( -f $filename ) {
   &main; # なければ生成する（１日１回だけ）
}

# PDF生成サブルーチン
sub main {
# 清記データの抽出
use Encode;

my $entryfile = "../data/entry.dat"; # 清記元データ（作者名が入ってる）
my @sjisdata = (); # 抽出したデータを格納する配列（文字コードは shiftjis）

open(IN,"$entryfile"); # 前日の投句データを開く
my @log = <IN>;
close (IN);

foreach my $log (@log){
	($no,$name,$date,$mail,$comment,$pass)=split(/,/,$log);
	chomp;
	push @sjisdata, "$no $comment\n"; # 番号と俳句のみを抽出
}

# PDFJ で処理するためにデータの文字コードを変換する（shiftjis >> euc-jp）
# Encode::decode 関数で行う。
# 文字コードは、uft8 以外に、shiftjis や euc-jp が指定できる。  

my @eucdata = (); # eucデータを格納する配列
foreach my $line1 (@sjisdata){
        $line1  =   decode('shiftjis', $line1); # shiftjisで読み込み
        $line1  =   encode('euc-jp', $line1);   # euc-jpで書き出す
        push @eucdata, $line1; # 格納する
}

# PDFJ
use lib './pdfj'; # アップロードしたPDFJモジュール
use strict;
use warnings;
use utf8;
use Encode;
use PDFJ 'EUC';

# 用紙の設定( 1pt = 0.35278mm て計算)
my $PaperW  = 842;  #A4の横
my $PaperH  = 595;  #A4の縦
my $PaperSU =  72;  #上余白(20mm)
my $PaperSD =  72;  #下余白(20mm)
my $PaperSR =  72;  #右余白(20mm)
my $PaperSL =  72;  #左余白(20mm)
my $iWidth  = $PaperW - $PaperSR - $PaperSL; #印刷可能幅
my $iHeight = $PaperH - $PaperSU - $PaperSD; #印刷可能高

# フォントサイズとラインフィートの設定
my $fSize    = 12;     #フォントサイズ
my $linefeed ='170%'; #ラインフィート

# pdfオブジェクトを作成
my $pdfDoc = PDFJ::Doc->new( 1.3, $PaperW, $PaperH); # PDF ver1.3

# フォントオブジェクトを作成
my $oFont = $pdfDoc->new_font('Ryumin-Light', 'EUC-V', 'Times-Roman');
# my $oFont = $pdfDoc->new_font('Hiragino Maru Gothic ProN', 'EUC-V', 'Monaco');

# テキストスタイルオブジェクトを作成
my $oTextStyle = TStyle(font => $oFont, fontsize => $fSize, vh => 1);

# 段落スタイルのオフシェクトを作成
my $oPStyle  = PStyle(size=>$iHeight, linefeed=>$linefeed, align=>'b', postskip=>0);

# 文ことに段落の配列を作成
my @Paragraphes = ();
for my $line (@eucdata){
  chomp($line);
  $line = encode('eucjp', '　') unless $line;
  my $oTexe = Text($line, $oTextStyle);
  push @Paragraphes, Paragraph($oTexe, $oPStyle);
}

# 段落の配列をフロックにまとめる
my $oBlock = Block('R', \@Paragraphes, BStyle());

# フロックをヘーシ毎に分割してヘーシに割り付ける
for my $oB ($oBlock->break($iWidth)) {
  my $oPage = $pdfDoc->new_page();
  $oB->show($oPage, $iWidth+$PaperSL, $iHeight+$PaperSD, 'rt');
}

# PDFを出力
$pdfDoc->print("./pdf/$ctoday\.pdf");

} # sub main 終わり

# PDFファイルへのリンクをHTMLに表示させる
print "Content-type: text/html\n\n";
print "<html><meta charset=\"utf-8\">\n";
print "<body>\n";
print "<a class=\"NAVLINK\" title=\"縦書清記\" href=\"/d_kukai/seiki/pdf/$ctoday.pdf\"><i class=\"fa fa-leaf\" aria-hidden=\"true\"></i></a>\n";
print "</body>\n";
print "</html>\n";

exit(1);

```

## 課題

* 上下揃えの均等にしたい
* A4たて二段組にしたい(将来投句数が増えた時のために)

