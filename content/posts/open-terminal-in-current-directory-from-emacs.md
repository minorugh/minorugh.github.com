+++
date = "2019-03-26T10:32:42+09:00"
categories = ["emacs"]
tags = ["terminal"]
title = "Emacsからカレントディレクトリでターミナルアプリを開く"

+++

私の場合は、Emacsからターミナル作業をするときは基本eshellを使うようにしています。ただ、ごくたまに別窓でターミナルアプリを立ち上げてemacsと併行して作業したいときがありますので以下のように設定しています。

当然ながらコマンドラインでアプリが起動できるようにbashなりzshなりで設定しておくことが前提です。

## macのiterm.appを起動させる設定 ##

``` emacs-lisp
;; Launch iterm.app with Current buffer
(defun my:iterm-app ()
  "Open iterm.app with current dir."
  (interactive)
  (let ((dir default-directory))
    (shell-command (concat "open -a iterm.app " dir))))
```
