+++
date = "2017-06-26T22:50:39+09:00"
categories = ["emacs"]
tags = ["whitespace"]
title = "whitespace-modeでjaspace風に表示させる設定"
+++


Emacs24以降は、内包されたWhitespaceで全角スペースなどを明示的に表示できることは知っていたけれど、長い間使い馴染んだjaspaceのそれと同じような表示にするための設定がわからず挫折していた。

今回、emacs設定ファイルを全体的に見直すことになり、Tipsをググっていて表題のものを見つけたので試してみた。

* [参考にしたTips](http://piyolian.blogspot.jp/2011/12/emacs-whitespace-like-jaspace.html) 

```lisp
(use-package whitespace
  :diminish global-whitespace-mode
  :config
  (setq whitespace-style
    '(face
      tabs spaces newline space-mark tab-mark newline-mark))
  (let ((dark (eq 'dark (frame-parameter nil 'background-mode))))
    (set-face-attribute 'whitespace-space nil
    		:foreground (if dark "pink4" "azure3")
    		:background 'unspecified)
    (set-face-attribute 'whitespace-tab nil
    		:foreground (if dark "gray20" "gray80")
    		:background 'unspecified
    		:strike-through t)
    (set-face-attribute 'whitespace-newline nil
    		:foreground (if dark "darkcyan" "darkseagreen")))
  (setq whitespace-space-regexp "\\(　+\\)")
  (setq whitespace-display-mappings
        '((space-mark   ?　    [?□]    [?＿]) ; full-width space - square
          (newline-mark ?\n    [?\xAB ?\n])))   ; eol - right quote mark
  (setq whitespace-global-modes '(not dired-mode tar-mode))
  (global-whitespace-mode 1))


```
