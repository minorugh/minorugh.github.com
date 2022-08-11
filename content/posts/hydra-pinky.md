+++
date = "2019-08-01T12:27:34+09:00"
categories = ["emacs"]
tags = ["hydra"]
title = "view-modeを超えるhydra-pinkyの設定"
+++

私のEmacs設定は、プログラマー [masasam](https://github.com/masasam)さんのブログ「 [Solist Work Blog](https://solist.work/blog/)」から多くのことを吸収して日々成長しています。 
そのブログの中にある[Emacs Pinkyをhydraで解決する](https://solist.work/blog/page/3/) の記事からhydra-pinkyの設定を参考にして更に使いやすくカスタマイズしてみました。


## Screen Shot
![hydra-pinky](https://live.staticflickr.com/65535/48441308482_2210006a97_b.jpg) 

hydra-pinky を発動するとミニバッファーに赤文字のメニューがでてきます。 hydra発動中はこの表示が続くのでわかりやすいです。 赤い表示のキーをタイプしている間はいつまでたってもhydraは解除されません。
hydraが終了する条件は、定義された赤いキー以外をタイプするか、青文字の "q"：quit を押した場合です。

つまりhydra-pinkyは、発動中はvew-modeに似た動作をし、そこから抜けると普通の編集モードに戻る…という仕組みです。
複雑なキーバインドを覚える必要もなくGUIでスピーディーにキー操作できるのはとてもありがたいです。
私自身はもともとブラインドタッチを使えないのでEmacs-pinkyの他にもいろいろhydraを活用しています。

[HydraでEmacsのキーバインド問題を解消](https://qiita.com/minoruGH/items/3776090fba46b1f9c228) 

## sequential-command
[rubikitch/sequential-command](https://github.com/rubikitch/sequential-command) 

Emacsを再起動して、C-a C-a C-aすると、行頭にいって、バッファーの頭にいって、元に戻る。
また、C-e C-e C-eすると行頭にいって、バッファーの頭にいって、元に戻るというシンプルな機能ですが、なにげに便利です。

pinkyでは、"a","e"キーに割り当てています。

## window-toggle-division
Windowの縦分割、横分割をtoggleで切り替えします。

## iflipb：バッファー移動
バッファー移動は、`next-buffer` `previous-buffer` を使ってもいいのですが、不要なbufferはignoreしてくれるfilipbを使います。
[Emacs:タブを使わない究極のバッファー移動](https://qiita.com/minoruGH/items/aa96e92c1434f87940d6) 

## 矢印キーの扱い
Emacserの方から見ると邪道だと言われそうですが、私はカーソル移動に矢印キーを使うことも多いです。基本は、h,j,k,lを使うようにしていてもうっかり矢印キーに触れるとhydra-pinkyが消えてしまうので、pinky発動中は矢印関係の操作で消えないようにしています。


## 設定
```emacs-lisp
;; sequential-command
(use-package sequential-command-config
  :commands sequential-command-setup-keys
  :hook (after-init . sequential-command-setup-keys))

;; other-window-or-split
(bind-key
 "C-q"
 (defun other-window-or-split ()
   "If there is one window, open split window.
If there are two or more windows, it will go to another window."
   (interactive)
   (when (one-window-p)
     (split-window-horizontally))
   (other-window 1)))

;; window-toggle-division
(defun window-toggle-division ()
  "Replace vertical <-> horizontal when divided into two."
  (interactive)
  (unless (= (count-windows 1) 2)
    (error "Not divided into two!"))
  (let ((before-height)
        (other-buf (window-buffer (next-window))))
    (setq before-height (window-height))
    (delete-other-windows)
    (if (= (window-height) before-height)
        (split-window-vertically)
      (split-window-horizontally))
    (other-window 1)
    (switch-to-buffer other-buf)
    (other-window -1)))

;; iflipb
(setq iflipb-wrap-around t)
(setq iflipb-ignore-buffers (list "^[*]" "^magit" "dir]$"))

;; Hydra-pinky
(bind-key [f11] 'hydra-pinky/body)
(key-chord-define-global
 "::"
 (defhydra hydra-pinky (:color red :hint nil)
   "
 ：_0_._1_._2_._3_._o_._S_._x_   ：_j_._k_._h_._l_._c_._a_._e_._b_._v_._SPC_._w_._s_._/_   ：_n_._p_._u_._t_   ：_<_-_:_-_>_   ：_q_uit"
   ;; window
   ("0" delete-window)
   ("1" delete-other-windows)
   ("2" split-window-below)
   ("3" split-window-right)
   ("o" other-window-or-split)
   ("S" window-swap-states)
   ("x" window-toggle-division)
   ;; page
   ("a" seq-home)
   ("e" seq-end)
   ("j" next-line)
   ("k" previous-line)
   ("l" forward-char)
   ("h" backward-char)
   ("c" recenter-top-bottom)
   ("<down>" next-line)
   ("<up>" previous-line)
   ("<right>" forward-char)
   ("<left>" backward-char)
   ("<C-up>" backward-paragraph)
   ("<C-down>" forward-paragraph)
   ("<C-left>" left-word)
   ("<C-right>" right-word)
   ("b" scroll-down-command)
   ("v" scroll-up-command)
   ("SPC" set-mark-command)
   ("w" avy-goto-word-1)
   ("s" swiper-isearch-region)
   ;; git
   ("n" git-gutter:next-hunk)
   ("p" git-gutter:previous-hunk)
   ("u" git-gutter:popup-hunk)
   ("t" git-gutter:toggle-popup-hunk)
   ;; buffer
   ("/" kill-buffer)
   (":" counsel-switch-buffer)
   ("<" iflipb-previous-buffer)
   (">" iflipb-next-buffer)
   ;; quit
   ("q" nil)))
```
