+++
date = "2019-09-04T17:03:29+09:00"
categories =["emacs"]
tags = ["buffer"]
title = "Scratch bufferを付箋として使う"
+++

![Alt Text](https://live.staticflickr.com/65535/48753577921_70897ea372_b.jpg) 

Emacsのscratchバッファーを簡易メモとして使うために作られたパッケージはいくつかありますが、設定だけで実現できる簡単なものを紹介します。必要な手順は以下の通りです。

1. Scratch buffer を kill させない。
2. Scratch buffer の内容を記憶させる。
3. ワンキーで Scratch bufferを表示させる。

## Scratch buffer を kill させない ##
特にpackageを導入せずともemacsの標準機能で実現できます。

``` emacs-lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set buffer that can not be killed.

(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))
```
設定反映後、scratch bufferを `kill-buffer` すると `Buffer "*scratch*" is locked and cannot be killed` とmessageがでます。

## Scratch buffer の内容を記憶させる ##
`persistent-scratch` というpackageもありますが、大方のemacserは使っていると思う `auto-save-buffers-enhanced` の設定で実現できます。

``` emacs-lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-save-buffers-enhanced

(setq auto-save-buffers-enhanced-quiet-save-p t)
;; scratch bufferを `~/.emacs.d/scratch` に保存
(setq auto-save-buffers-enhanced-save-scratch-buffer-to-file-p t)
(setq auto-save-buffers-enhanced-file-related-with-scratch-buffer
      (locate-user-emacs-file "scratch"))
(auto-save-buffers-enhanced t)
```

## ワンキーで Scratch bufferを表示させる ##
popwinの機能を使います。`my:pop-scratch` を起動するとscratch bufferがpopupします。メモしたあと `C-g` で隠せるので便利です。

``` emacs-lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Popup the scratch buffer

(bind-key
 "s-x"
 (defun my:pop-scratch ()
   "Popup the scratch buffer."
   (interactive)
   (setq popwin:special-display-config '("*scratch*"))
   (display-buffer "*scratch*")))
```

