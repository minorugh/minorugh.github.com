+++
date = "2018-01-10T20:36:33+09:00"
categories = ["emacs"]
tags = ["hydra"]
title= "hydraをミニメニューとして使う"
+++
hydraは、関連するコマンドを複数定義して、 それらのコマンドを連続して簡単に呼び出せるようにするツールです。基本的にはsmartrepの機能と同じですが、パラメーターによっていろいろ応用できるようなので試してみました。メニュー内容をエコーエリアに表示させることもできるのでGUIとしても優れています。

<!--more-->

## Dired menu
![Alt Text](https://goo.gl/C4GVjH) 

目的のファイルを開くのに、```find-file```やdired-jumpなどからアクセスするのが一般的だけれど、Dropboxやemacs設定ファイルなどよく使うフオルダーは一発で開けれるほうが便利です。

パラメーター（:exit t）を設定すると、いづれかの選択キーを押したら自動的にメニューが消えてくれます。

```lisp
;; for my-dired
;; =======================================
(defhydra hydra-dired (:hint nil :exit t)
   "
_r_: Restart  _g_: GitHub  _t_: Tramp  _j_: Dired  _e_: Easy-hugo  _d_: Dropbox  _w_: Webdir  _i_: Inits  _c_:howm
"
  ("e" easy-hugo)
  ("c" howm-create)
  ("r" restart-emacs)
  ("t" counsel-tramp)
  ("i" my/init-dir)
  ("d" my/dropbox)
  ("w" my/www-dir)
  ("g" my/github)
  ("j" dired-jump)
  ("q" keyboard-quit))
(bind-key "M-." 'hydra-dired/body)

```
## window menu

![Alt Text](https://goo.gl/oiodAL) 

こちらは、Emacsのwindow管理のメニューです。パラメター（:exit t）  は設定していないのでほかのコマンドが実行されるまでは、メニュー画面を持続します。

smartrepと同じ機能ですね。

```lisp
;; for window control
;; ==========================================
(defhydra hydra-window (:hint)
  "
[Window]  _0_: Del  _1_: Del-all  _2_: ◀▶  _3_: ▲▼  _{_: ←  _}_: →  _~_: ↓  _=_: ↑ _+_: BL  _o_: Split  _s_: Swap
"
  ("s" swap-buffers)
  ("o" other-window-or-split)
  ("0" delete-window)
  ("1" delete-other-windows)
  ("2" split-window-below)
  ("3" split-window-right)
  ("{" shrink-window-horizontally)
  ("}" enlarge-window-horizontally)
  ("~" enlarge-window)
  ("=" shrink-window)
  ("+" balance-windows))
(key-chord-define-global ",," 'hydra-window/body)

;; Other-window-or-sprit
(defun other-window-or-split ()
  "Other-window-or-sprit."
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
    (other-window 1))
(bind-key "C-q" 'other-window-or-split)
```

## 参考サイト

https://github.com/abo-abo/hydra

http://emacs.rubikitch.com/hydra/
