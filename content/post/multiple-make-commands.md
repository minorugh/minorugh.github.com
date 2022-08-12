+++
date = "2019-03-04T14:20:41+09:00"
categories = ["emacs"]
tags = ["makefile","hydra"]
title = "Hydraで複数のmakeコマンドを自在に使う"
+++

ホームページ管理のためにEmacsをワークデスクとして色んな作業をしています。その中で何度も繰り返し行う作業はMakeファイルに書いて限りなく自動化して、それをEmacsから実行しています。

```elisp
M-x compile
```
とすると、
```
Compile command: make -k
```
とミニバッファーにでます。ここでリターンキーを押すと、makeコマンドが実行されます。

makeファイルの中の特定のブロックを実行したい時、例えば更新されたファイルをサーバーにアップロードしてホームページを更新する時は、```make -k``` を消して ```make upftp``` と書き換えてリターンキーを押すとupftpコマンドが実行されるという仕組みです。

通常この使用法でも大した問題はないのですが、compile-commandの内容を書き換えて実行すると、次回に```M-x compile```コマンドを実行したとき ```make -k``` にはならず、先程の書き換えたものがデフォルトに変わってしまうのです。

そこで いろいろな引数でmakeコマンドを使い分けるためい、hydraで設定してみました。

## 設定

```emacs-lisp
;;; 50_hydra-compile.el --- 50_hydra-compile.el
;;; Commentary:
;;; Code:
;;(setq debug-on-error t)

(bind-key
 [f2]
 (defhydra hydra-compile (:color red :hint nil)
  "
╭─────────────────────────────────────┐
│ 🗿 Compile:
     make:_a_ll  _u_pftp  _m_ove  _b_klog  _g_it
     ---------------------------------
     make:-_k_   _c_lean
└──────────────────────────────────────┘"
   ("a" my:make-all :exit t)
   ("u" my:make-upftp :exit t)
   ("m" my:make-move :exit t)
   ("b" my:make-bklog :exit t)
   ("g" my:make-git :exit t)
   ("k" my:make-default)
   ("c" my:make-clean)))

(defun my:make-default ()
  "Make command default."
  (interactive)
  (setq compile-command "make -k")
  (my:compile))

(defun my:make-upftp ()
  "Make command for upftp."
  (interactive)
  (setq compile-command "make up")
  (my:compile))

(defun my:make-all ()
  "Make command for all."
  (interactive)
  (setq compile-command "make -k && make up")
  (my:compile))

(defun my:make-move ()
  "Make command for move."
  (interactive)
  (setq compile-command "make mv")
  (my:compile))

(defun my:make-bklog ()
  "Make command for bklog."
  (interactive)
  (setq compile-command "make bk")
  (my:compile))

(defun my:make-git ()
  "Make command for git."
  (interactive)
  (setq compile-command "make git")
  (my:compile))

(defun my:make-clean ()
  "Make command for clean."
  (interactive)
  (setq compile-command "make clean")
  (my:compile))

(defun my:compile ()
  "Restore compile command after recompile."
  (interactive)
  (recompile)
  (setq compile-command "make -k"))

;; Local Variables:
;; byte-compile-warnings: (not free-vars callargs)
;; End:
;;; 50_hydra-compile ends here
```
## 更に使いやすくする
`compile` や  `shell-command` を実行すると結果のログが表示されます。その都度閉じるのが面倒なので `popwin` で表示させるようにします。popwin windowは "C-g" で閉じることができます。emacsでは何かと "C-g" を使うことが多いので、私はキーハックアプリで `command_L` に割り当ててワンキーで実行できるようにしています。

``` emacs-lisp
(use-package popwin)
(popwin-mode 1)

;; M-!
(push "*Shell Command Output*" popwin:special-display-config)

;; M-x compile
(push '(compilation-mode :noselect t) popwin:special-display-config)
```
