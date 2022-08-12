+++
date = "2017-11-17T15:22:25+09:00"
categories = ["emacs"]
tags = ["makefile"]
title= "Makefile の Suspicious line XXX. Save anyway を抑制する"
+++
Emacsのmakefile-modeで行頭のtabを挿入するたびに "Suspicious line XXX. Save anyway?" というポップアップ・プロンプトが出る。
この現象は、auto-save-buers との関係らしく、以下のおまじないを設定することででなくなった。

<!--more-->
```lisp
;;; Makefile の Suspicious line XXX. Save anyway を抑制する
(add-hook 'makefile-mode-hook
    (function
        (lambda ()
            (fset 'makefile-warn-suspicious-lines 'ignore))))
```
