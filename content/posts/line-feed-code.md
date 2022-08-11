+++
date = "2021-09-12T21:00:38+09:00"
categories = ["perl"]
tags = ["perl"]
title = "Downloadした CGI Scriptを Severへ Uploadするときは改行コードを修正する必要あり"
+++

Windowsから Linuxに移行してから Webで Downloadした CGIスクリプトをそのまま FTPアプリでアップロードすると必ずエラーになる。
試行錯誤して原因は、主要な CGIスクリプトの改行コードが CRLFとなっていたことでした。

Linux環境でダウンロードして解凍するというプロセスによってそうなるのかどうかは解明できていませんが、これを LFに変えてやることで問題なくスクリプトが動作しました。

いつも何も考えずにアップレードして「あれ！」となるのでしっかり備忘録に記しておこうと考えた次第。そういえば、Mac環境のときもそうだった気がする。UNIX系の症状かも知れないのでもう少しググって調べてみよう…


たくさんファイルがある時は、いちいち Terminalを開くのは面倒なので Emacsでやっている。

```emacs-lisp
  (defun convert-encode-to-utf8 ()
	"Convert character code of the file to utf-8."
	(interactive)
	(let ((file (buffer-file-name)))
	  (if (not (use-region-p))
		  (compile (concat "nkf -w -Lu --overwrite " file)))))

  (defun convert-encode-to-sjis ()
	"Convert character code of the file to utf-8."
	(interactive)
	(let ((file (buffer-file-name)))
	  (if (not (use-region-p))
		  (compile (concat "nkf -s -Lu --overwrite " file)))))
```
