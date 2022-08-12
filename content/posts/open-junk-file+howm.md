+++
date = "2017-07-17T16:35:47+09:00"
categories = ["emacs"]
tags = ["howm"]
title= "open-junk-file+howmでサンプルコードテスト環境を構築"
+++
ぼくはプログラマーではないけど、作業の自動化などを図るためにときどきパッチワークコードを試すことがある。
この目的のために多くの人は、open-junk-file.elを使っているみたいなので素直に見習う。

サンプルコードの場合、うまく動くことが確認できたらあとは使い捨てになるので保存場所にはさほど神経質にならなくても良いが、あとからの検索を用意にするためにぼくの場合は、howmフォルダーに置いている。検索は、helm-agでもいいのだけどhowmの一覧機能や検索機能がとても使いやすく優れているからだ。

## open-junk-file.el
メモのファイル名は、[timestamp.拡張子]という使い方が多いみたいだけど、ちょっと判りにくいので、ぼくの場合は、20170717.hoghoge.plという感じにしてる。

```lisp
;;--------------------------------------------------------------------------------
;; open-junk-file.el                                      last updated: 2017/07/17
;;--------------------------------------------------------------------------------
(use-package open-junk-file
  :bind ("C-c j" . open-junk-file)
  :config (setq open-junk-file-format "~/Dropbox/howm/junk/%Y/%Y%m%d."))

;; Automatically insert tags according to extension
(defvar open-junk-ext-tags-alist
  '(("el" ";;" "ELISP")
    ("pl" "#" "PERL")
    ("py" "#" "PYTHON")
    ("rb" "#" "RUBY")))
(defadvice open-junk-file
  (after open-junk-file-insert-howm-comment-advice activate)
  "After open-junk-file, insert a tag into the opened buffer
to be searched by howm."
  (let* ((ext (replace-regexp-in-string "^.*\\.\\([^\\.]+\\)$" "\\1" buffer-file-name))
         (asc (assoc ext open-junk-ext-tags-alist))
         (prefix (cadr asc))
         (tag (caddr asc)))
    (insert prefix)
    (insert " %" tag)))
```

## quickrun.el

コードを試運転したりデバッグするときに quickrun.el を入れて併用すると最強となる。

```lisp
;;-------------------------------------------------------------------------
;; 20_quicrun.el                                   last updated: 2017/07/05
;;-------------------------------------------------------------------------
(use-package quickrun
  :bind ("C-c q" . quickrun) ;; ファイルを実行する
        ("C-c c" . quickrun-compile-only))
```
