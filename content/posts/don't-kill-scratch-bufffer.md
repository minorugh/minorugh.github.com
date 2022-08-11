+++
date = "2019-03-28T16:12:24+09:00"
categories = ["emacs"]
tags = ["buffer"]
title = "Scratch bufferをKILLさせない：emacs-lock-mode"
+++
わざわざscratch bufferをkillする人はいないと思いますが、"kill-other-windows" などを定義していると、うっかりscratchも一緒に消えてしまって困ることがありますね。

あやまってscratch killしたら新しいscratchを自動再生してくれるとか、kill出来ないようにする…というTipsも多いのですが、そんな回りくどいことをしなくてEmacsの標準機能だけて簡単に対応できることがわかりました。


## emacs-lock-mode を使う##

パッケージも何もいりません。下記を設定してみてください。私は `*Messages*` も一緒に設定しています。

``` emacs-lisp
;; Set buffer that can not be killed.
(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))
(with-current-buffer "*Messages*"
  (emacs-lock-mode 'kill))
```

この選定を有効にしたあとscratchをkill bufferしようとすると、

``` emacs-lisp
Buffer "*scratch*" is locked and cannot be killed
```
と叱られます。つまりkillされないようにロックが掛けられるのです。
`emacs-lock-mode` というのはもともとemacsの内蔵コマンドにあるのです。


## Scratchを自動保存する ##

私もそうなのですが、ちょこっとした付箋的なメモをするためにscratchを使うという人も多いと思います。そんな人には、Emacsを再起動させても終了前のscratchが復帰してほしい…というニーズがあります。こうした目的のためのパッケージも何種類かありますが、私は、auto-seve-buffers-enhanced.el の機能を流用しています。このパッケージを使っているEmacserは多いと思うので、わざわざ専用のパッケージを使わずともscratchの保存再生が可能になります。

``` emacs-lisp
(use-package auto-save-buffers-enhanced)
;; Suppress Wrote's message
(setq auto-save-buffers-enhanced-quiet-save-p t)
;; Scratch buffer is also saved automatically
(setq auto-save-buffers-enhanced-save-scratch-buffer-to-file-p t)
(setq auto-save-buffers-enhanced-file-related-with-scratch-buffer "~/Dropbox/etc/emacs/scratch")
(auto-save-buffers-enhanced t)
```

## scratchを瞬時にpopup表示させる ##
`scratch-pop` という便利なパッケージもあるのですが、私にとって余り必要と思わない機能もついているのでシンプルに設定を書いてみました。

popwinを導入した上で下記をinitファイルに書き、適当なキーバインドを設定すると一発でscratch bufferがpopupし、"C-g" で隠れてくれます。

``` emacs-lisp
(defun my:pop-scratch ()
  "Popup the scratch buffer."
  (interactive)
  (setq popwin:special-display-config '(("*scratch*")))
  (display-buffer "*scratch*"))
```

## どんな使い方をしているか ##
簡単なelispの評価にscratchを使うのは当然ですが、私の場合、なにか調べたいキーワードをscratchにちょこっと書いてsearch-webで即検索…というような使い方を良くします。参考に私のhydra-search-webの設定にリンクを張っておきます。

  * [hydra-search-web](https://github.com/minorugh/emacs.d/blob/master/inits/80_search-web.el)

