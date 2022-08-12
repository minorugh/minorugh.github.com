+++
date = "2019-04-08T19:31:31+09:00"
categories = ["emacs"]
tags =["direx"]
title = "direx-project + popwinで快適なディレクトリツリー環境を構築する"

+++
![Alt Text](https://live.staticflickr.com/7901/33686197028_bbd99854f5_b.jpg)


Emacsでディレクトリツリーを表示させるパッケージにはいくつかの選択肢があります。neotreeが人気のようですが表示幅が自由にカスタマイズできないのでiconモードで使うとややストレスです。私は、direxが使いやすいので愛用しています。

例によってパッチワークですが、設定を公開します。特徴として以下の機能を持ちます。

  * フォルダー表示はビジュアルにしたいのでunicodeの絵文字を使う。
  * popwinを使うことで表示幅を自由に設定できる。(q または C−g で隠せる)
  * project内にいるなら、direx-projectを起動し、そうでなければ普通にdirexを起動する（これが気に入っています）
  * [https://blog.shibayu36.org/entry/2013/02/12/191459](https://blog.shibayu36.org/entry/2013/02/12/191459) 



## 設定 ##

``` emacs-lisp
;; direx
(use-package direx)
(setq direx:leaf-icon "  " direx:open-icon "📂" direx:closed-icon "📁")
(push '(direx:direx-mode :position left :width 35 :dedicated t)
      popwin:special-display-config)
;; use direx-project.el
;; https://blog.shibayu36.org/entry/2013/02/12/191459
(bind-key
 [f11]
 (defun direx:jump-to-project-directory ()
   "If in project, launch direx-project otherwise start direx."
  (interactive)
  (let ((result (ignore-errors
                  (direx-project:jump-to-project-root-other-window)
                  t)))
    (unless result
      (direx:jump-to-directory-other-window)))))
```
