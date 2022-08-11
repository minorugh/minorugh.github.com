+++
date = "2019-09-01T22:46:23+09:00"
categories = ["emacs"]
tags = ["hydra"]
title = "EmacsのFunction key設定を公開"
+++
あまり役に立つTipsではありませんが、自分の設定を公開します。みなさんの「私の場合は…」というのを教えていただけると嬉しいです。

### F1：help-command
F1は、Deaultでいろんなhelp-commadへのprifixとして設定されているのでそのまま使います。which-key.el を導入することで各コマンドのガイドがミニバファーに表示されるので便利です。
![Alt Text](https://live.staticflickr.com/65535/48658798686_6c5227888f_b.jpg)

``` emacs-lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which-key
(require 'which-key)
(add-hook 'after-init-hook #'which-key-mode)
```

### F2：hydra-compile
一般的には此処に、`M-x compile`を割り当てている人が多いと思います。私はいろんな作業をmakefaileで自動化しているので目的に応じてコマンドが使えるようにhydraでメニューを設定して割り当てています。 

![Alt Text](https://live.staticflickr.com/65535/48663159803_02857a9beb_b.jpg) 

``` emacs-lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compile

(bind-key
 [f2]
 (defhydra hydra-compile (:color red :hint nil)
   "
 🗿 Compile: make _k_  _a_ll  _u_pftp  _m_ove  _b_klog  _g_it  _c_lean   🐾 "
   ("k" my:make-k :exit t)
   ("a" my:make-all :exit t)
   ("u" my:make-upftp :exit t)
   ("m" my:make-move :exit t)
   ("g" my:make-git :exit t)
   ("b" my:make-bklog :exit t)
   ("c" my−make-clean)))
```
- [hydra-compile の詳細設定はこちら](https://github.com/minorugh/emacs.d/blob/master/inits/08_compile.el) 

### F3：iconify-or-deiconify-frame
emacsclient使用時という条件下でフレームのポップアップ/最小化をtoggleさせます。

``` emacs-lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; iconify-or-deiconify-frame

(bind-key "<f3>" 'iconify-or-deiconify-frame)
```

### F4：Toggle current buffer and *scratch* buffer.
カレントバッファーとScrtchバッファーとをtoggleさせます。

``` emacs-lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Toggle current buffer and `*scratch*` buffer

(defvar toggle-scratch-prev-buffer nil)
(defun toggle-scratch()
  "Toggle current buffer and *scratch* buffer."
  (interactive)
  (if (not (string= "*scratch*" (buffer-name)))
      (progn
	(setq toggle-scratch-prev-buffer (buffer-name))
	(switch-to-buffer "*scratch*"))
    (switch-to-buffer toggle-scratch-prev-buffer)))
(bind-key "<f4>" 'toggle-scratch)
```

### F5：quickrun
ごくたまに `perl` や `ruby` などのミニスクリプトを自作することもあるのでquickrunで簡単に試運転できるようにしています。

``` emacs-lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; quickrun
(require 'quickrun)
(bind-key [f5] 'quickrun)
```

### F6：counsel commands
多機能なcounselのコマンド群をキーバインドしてもとても覚えきれませんね。
よく使う主要コマンドだけキーバインドし、あとはF6を叩くとcounsel-M-xが勝手に絞り込んでくれるので必要なコマンドを絞り込んでセレクトできます。
![Alt Text](https://live.staticflickr.com/65535/48659043901_ed297c5081_b.jpg) 

``` emacs-lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; counsel

(bind-key "<f6>" (lambda ()
		   (interactive)
		   (counsel-M-x "^counsel ")))

```
- [counsel の詳細設定はこちら](https://github.com/minorugh/emacs.d/blob/master/inits/04_counsel.el) 



### F7：calendar
リタイアしてからは、EmacsでのGTDはやめてしまいましたが、作業中にカレンダーをチラ見したいときもあります。Calfwまでは必要ないので標準機能のcalendarを使っています。F7を押すことで、表示/非表示をトグルします。

![Alt Text](https://live.staticflickr.com/65535/48659029836_932b26293e_b.jpg) 

``` emacs-lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calendar

(use-package calendar
  :commands calendar
  :bind (("<f7>" . calendar)
	 :map calendar-mode-map
	 ("n" . calendar-forward-day)
	 ("b" . calendar-backward-day)
	 ("<f7>" . calendar-exit))
  :config
  (setq calendar-mark-holidays-flag t))

```
- [calendar の詳細設定はこちら](https://github.com/minorugh/emacs.d/blob/master/inits/10_ui.el) 


### F8：neotree-toggle
定番の設定ですね。

![Alt Text](https://live.staticflickr.com/65535/48663514508_0498d52ca2_b.jpg) 

- [neotree の詳細設定はこちら](https://github.com/minorugh/emacs.d/blob/master/inits/30_neotree.el)


### F9：display-line-numbers-mode
linum-modeは重いので使ってなかったのですが、Emacs26以降になって動作の軽い `display-line-numbers-mode`が使えるようになったので、F9でtoggleしています。

``` emacs-lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display-line-numbers-mode (Emacs 26.0.50 and newer versions)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(bind-key [f9] 'display-line-numbers-mode)
```
### F10：dashboard
私のEmacsはkillすることがないので、起動画面にしているdashoardも消さないようにして、Emacsでの作業をリセットするときはdashboardに戻るようにしています。また戻るときには併せてkill-other-bufferしてます。

``` emacs-lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unkillable-scratch
(require 'unkillablr-scratch)
(setq unkillable-buffers '("^\\*scratch*\\*$" "^\\*Messages\\*$" "^\\*dashboard\\*$"))
(add-hook 'after-init-hook 'unkillable-scratch 1)
```

![Alt Text](https://live.staticflickr.com/65535/48645786476_70d4295c83_b.jpg) 


``` emacs-lisp
(bind-key
 "<f10>"
 (defun my:dashboard ()
  "Switch buffer to Dashboard."
  (interactive)
  (switch-to-buffer "*dashboard*")
  (kill-other-buffers)))
```
- [dashboard の詳細設定はこちら](https://github.com/minorugh/emacs.d/blob/master/inits/01_dashboard.el) 

### F11：undo-tree-visualize

undo/redoは、いろいろ試しましたが直感的なundo-treeが一番使いやすいと私は思います。
`undo-tree-undo` `undo-tree-redo` をそれぞれキーバインドし、F11で `undo-tree-visualize` をtoggleしています。 

![Alt Text](https://live.staticflickr.com/65535/48668689381_8603349e37_b.jpg) 


``` emacs-lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; undo-tree

(use-package undo-tree
  :bind (("C-_" . undo-tree-undo)
	 ("M-_" . undo-tree-redo)
	 ("<f11>" . undo-tree-visualize)
	 :map undo-tree-visualizer-mode-map
	 ("<f11>" . undo-tree-visualizer-quit))
  :hook (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-timestamps t
	undo-tree-visualizer-diff t
	undo-tree-enable-undo-in-region nil
	undo-tree-auto-save-history nil
	undo-tree-history-directory-alist
	`(("." . ,(concat user-emacs-directory "undo-tree-hist/"))))
  :config
  ;; FIXME: `undo-tree-visualizer-diff' is a local variable in *undo-tree* buffer.
  (defun undo-tree-visualizer-show-diff (&optional node)
    ;; show visualizer diff display
    (setq-local undo-tree-visualizer-diff t)
    (let ((buff (with-current-buffer undo-tree-visualizer-parent-buffer
		  (undo-tree-diff node)))
	  (display-buffer-mark-dedicated 'soft)
	  win)
      (setq win (split-window))
      (set-window-buffer win buff)
      (shrink-window-if-larger-than-buffer win)))
  (defun undo-tree-visualizer-hide-diff ()
    ;; hide visualizer diff display
    (setq-local undo-tree-visualizer-diff nil)
    (let ((win (get-buffer-window undo-tree-diff-buffer-name)))
      (when win (with-selected-window win (kill-buffer-and-window))))))

```

### F12：darkroom-mode

私のEmacsは文章書きがメインなのでDarkroom-modeをF12のtoggleで使っています。darkroom-mode時は、`flycheck-mode` `git-gutter-mode` をOFFにします。


``` emacs-lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Darkroom

(use-package darkroom
  :bind (([f12] . my:darkroom-mode-in)
	 :map darkroom-mode-map
	 ([f12] . my:darkroom-mode-out ))
  :config
  (defun my:darkroom-mode-in ()
    "Darkroom mode in."
    (interactive)
    (display-line-numbers-mode 0)
    (flycheck-mode 0)
    (git-gutter-mode 0)
    (darkroom-mode 1))
  (defun my:darkroom-mode-out ()
    "Darkroom mode out."
    (interactive)
    (darkroom-mode 0)
    (git-gutter-mode 1)
    (flycheck-mode 1)
    (display-line-numbers-mode 1)))
```


