+++
date = "2022-09-22T14:22:20+09:00"
categories = ["emacs"]
tags = ["mozc"]
title = "競合コピーが生じないようにemacs-mozc辞書を共有する方法"
+++
Mozc辞書をDropboxに置いて、そのシンボリックをそれぞれの各端末に貼って辞書共有をしている人は多いと思います。

リアルタイムで同時使用はしない…という使い方であれば何も問題ないのですが、
自分の場合は、基本メイン機のEmacsは起動しっぱなし（蓋閉じでSleep）なので、その状態でサブ機のEmacsを立ち上げると、mozcのON/OFFとは関係なく`~/Dropbox/mozc/.mozc/` に競合コピーが量産されます。

そこで、簡単な回避方法を考えてみました。大げさなタイトルですが、Tipsといえるほどのものではありません。


## ファイル体系
くどくど説明するより下図を見ていただければ、"な〜んだ" と理解いただけると思います。

メイン機の `.mozc/` は、Dropboxに保存してシンボリックリンクで使い、サブ機で使うときはEmacsを起動するたびにDropboxにある最新の `.mozc/` をコピーして使うという仕組みです。

```
※ maine-machine
~/.mozc <-- symbolic link -- ~/Dropbox/mozc/.mozc
　                               | Copy latest every time
※ sub-mchine                    |
~/.mozc <-- symbolic link -- ~/Dropbox/backup/mozc/.mozc
```

## Emacsの設定
emacsの設定ファイルはメイン機とサブ機で共有しているので、`uname -n` を条件子としてメイン機（自分の場合はe590）でない場合（サブ機のとき）は、emacsを起動したときにmozc辞書をコピーしています。シンボリックリンクは、一度貼っておけばコピーのたびに貼り直さなくても大丈夫です。

```emacs-lisp
;; Clone the mozc dictionary placed in Dropbox to Nextcloud.
(defun mozc-copy ()
 "Copy mozc for submachine."
 (interactive)
 (unless (string-match "e590" (shell-command-to-string "uname -n"))
	 (compile "cp -rf ~/Dropbox/mozc/.mozc ~/Dropbox/backup/mozc")))
(add-hook 'emacs-startup-hook 'mozc-copy)
```

## 使い方
Dropboxに配置したmozc辞書は、メイン機での単語登録や入力履歴を記憶し常に最新の状態でバックアップされます。
サブマシーンの場合は、emacsを起動するたびに最新の辞書をコピーしてそれを使うという簡単な割り切りです。

サブマシーンで単語登録したら、元辞書へ書き戻すという仕組みも考えれますが、結局は、競合コピーをどう回避するかという課題になると思うので割り切ることにしました。良い方法があれば教えてください。

メイン機、サブ機の定義は特にありません。使用頻度の高い方をメイン機として構成すればいいかなと思います。
