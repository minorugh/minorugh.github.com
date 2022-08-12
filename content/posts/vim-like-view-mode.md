+++
date = "2018-01-04T19:09:15+09:00"
categories = ["emacs"]
tags = ["vim"]
title= "Emacsのview-modeでvimのコマンドを使えるようにする"
+++

vimを少し触り始めて、Emacsのview-modeでも簡単な編集操作は可能にしたいと思うようになったので、
viper-modeとかEvilを使わないで、view-modeだけでvimライクな編集機能を実現させるべく試運転中です。

<!--more-->
まずは、私のview-mode設定をご覧ください。

```elisp
;; View-mode key map
(use-package keys-in-view-mode)
(with-eval-after-load 'view
  (bind-keys :map view-mode-map
             ("h" . backward-char)
             ("j" . next-line)
             ("k" . previous-line)
             ("l" . forward-char)
             ("J" . scroll-down-line)
             ("K" . scroll-up-line)
             ("b" . scroll-down)
             ("0" . beginning-of-line)
             ("e" . end-of-line)
             ("g" . goto-line)
             ("G" . View-goto-percent)
             ("d" . dired-jump)
             ("m" . magit-status)
             ("v" . vc-diff)
             ("n" . neotree-toggle)
             ("s" . swiper-migemo)
             (":" . save-buffer)
             ("u" . vim-undo)
             ("r" . vim-redo)
             ("%" . vim-jump-brace)
             ("w" . forward-word+1)
             ("W" . backward-word)
             ("o" . vim-o)
             ("O" . vim-O)
             ("y" . copy-region-as-kill)
             ("Y" . vim-copy-line)
             ("p" . vim-p)
             ("P" . vim-P)
             ("x" . vim-del-char)
             ("c" . vim-del-char-to-insert)
             ("X" . vim-backward-kill-line)
             ("D" . vim-kill-line)
             ("C" . vim-kill-line-to-insert)
             ("a" . vim-forward-char-to-insert)
             ("A" . vim-end-of-line-to-insert)
             ("i" . view-mode)
             ("I" . vim-beginning-of-line-to-insert)
             ("L" . pdf-preview-Buffer))
(key-chord-define view-mode-map "cc" 'vim-kill-whole-line-to-insert)
(key-chord-define view-mode-map "dd" 'vim-kill-whole-line)
(key-chord-define view-mode-map "gg" 'View-goto-line))
;; In my case, use eikana.app to remap mac's fn key to "S-<f12>" only in emacs I will.
;; This allows you to toggle view-mode with one key
(bind-key "S-<f12>" 'view-mode)

; Change mode-line color in view-mode
(use-package viewer)
(setq viewer-modeline-color-view "dark red")
(viewer-change-modeline-color-setup)

;; Add hl-line-mode to view-mode
(when (functionp 'hl-line-mode)
  (add-hook 'view-mode-hook '(lambda () (hl-line-mode 1)))
  (defadvice view-mode-disable (after disable-hl-line-mode activate)
    (hl-line-mode -1)))

;; Open-file from ivy-switbuffer in view-mode
(defun ivy--switch-buffer-action (buffer)
  (with-ivy-window
    (if (zerop (length buffer))
        (switch-to-buffer
         ivy-text nil 'force-same-window)
      (let ((virtual (assoc buffer ivy--virtual-buffers))
            (view (assoc buffer ivy-views)))
        (cond ((and virtual
                    (not (get-buffer buffer)))
               ;; (find-file (cdr virtual)))
               (view-file (cdr virtual)))
              (view
               (delete-other-windows)
               (let (
                     ;; silence "Directory has changed on disk"
                     (inhibit-message t))
                 (ivy-set-view-recur (cadr view))))
              (t
               (switch-to-buffer
                buffer nil 'force-same-window)))))))

;; Neotree
(use-package neotree
  :config
  ;; Customize to open file in view-mode
  (defun neo-open-file (full-path &optional arg)
    (neo-global--select-mru-window arg)
    (view-file full-path)))
(setq neo-show-hidden-files t)
(setq neo-smart-open t)

;; Pdf-preview (printing in Mac environments)
(when (eq system-type 'darwin)
(with-eval-after-load 'view
(use-package pdf-preview)
(setq pdf-preview-preview-command "open -a Preview.app")
(setq ps-line-number t)))
```

## Vimライクなキーバインド
基本的にview-modeでは不可能だったことを可能にするのが目的なので、Emacsの機能で十分なことまで、vimライクにこだわることは無意味だと思います。
とりあえず以下のコマンドを使えるようにしています。


|目的            |コマンド|挙動                                 |
|:---------------|:------:|:------------------------------------- | 
|インサートモード|a       |カーソルの一つ右からインサートモードへ |
|                |i       |カーソルの位置からインサートモードへ   |
|                |A       |行末に移動しインサートモードへ         |
|                |I       |行頭に移動しインサートモードへ         |
|                |o       |次行を新規行として挿入してインサートモードへ |
|                |O       |現在行に新しい行を追加してインサートモードへ |
|                |cc      |一行を削除してインサートモードへ     |
|移動            |j       |下に移動                             |
|                |k       |上に移動                             |
|                |h       |左に移動                             |
|                |l       |右に移動                             |
|                |0       |行頭へ                               |
|                |e       |行末へ (vim : $ 相当)                |
|                |w       |次の単語の先頭へ                     |
|                |W       |前の単語の先頭へ (vim : b 相当)      |
|                |%       |対応する括弧への移動                 |
|                |b       |前のページへ (vim : C-b 相当)        |
|                |SPC     |次のページへ (vim : C-f 相当)  … view-modeの基本機能       |
|                |g       |指定行へジャンプ (emacs : M-g g)     |
|                |gg      |ページの先頭に移動                   |
|                |G       |ページの最後に移動                   |
|削除            |dd      |カーソルがある行を削除               |
|                |x       |一文字削除                                       |
|                |X       |カーソル位置から行頭まで削除(オリジナル)         |
|                |D       |カーソル位置から行末まで削除                     |
|                |c       |一文字消してインサートモードへ |
|                |C       |カーソル位置から行末まで削除してインサートモードへ |
|undo            |u       |undo                                 |
|redo            |r       |redo (vim : Ctrl+r 相当)             |
|コピー          |y       |yank(コピー)                         |
|                |Y       |今いる行をyank                       |
|ペースト        |p(小文字)|カーソルの右にyankをペースト         |
|                |P(大文字)|カーソルの下にyankした行をペースト   |


## させたい挙動を定義する

別ファイルkeys-in-view-mode.elに必要な関数を定義して読み込ませています。
view-modeを解除して必要な作業をし終わったらまたview-modoに戻すだけです(^^)

まだ不完全な挙動もあるので、さらなる検証が必要ですが、
elispビギナーのわたしでも書ける程度の簡単なものですから、詳しい方ならもっと複雑な挙動も設定できると思います。


```lisp
;; keys-in-view-mode.el
;; ==============================
;; like a
(defun vim-forward-char-to-insert ()
  (interactive)
  (view-mode 0)
  (forward-char 1)
  (message "edit-mode !"))
;; like A
(defun vim-end-of-line-to-insert ()
  (interactive)
  (view-mode 0)
  (end-of-line)
  (message "edit-mode !"))
;; like I
(defun vim-beginning-of-line-to-insert ()
  (interactive)
  (view-mode 0)
  (beginning-of-line)
  (message "edit-mode !"))
;; like cc
(defun vim-kill-whole-line-to-insert ()
  (interactive)
  (view-mode 0)
  (kill-whole-line)
  (open-line 1)
  (backward-line)
  (beginning-of-line)
  (message ":kill-whole-line and edit-mode !"))
;; like dd
(defun vim-kill-whole-line ()
  (interactive)
  (view-mode 0)
  (kill-whole-line)
  (view-mode 1)
  (message "kill-whole-line"))
;; like D
(defun vim-kill-line ()
  (interactive)
  (view-mode 0)
  (kill-line)
  (view-mode 1)
  (message "kill-line"))
;; like C
(defun vim-kill-line-to-insert ()
  (interactive)
  (view-mode 0)
  (kill-line)
  (message "kill-line and edit-mode !"))
;; like o
(defun vim-o ()
  (interactive)
  (view-mode 0)
  (forward-line)
  (open-line 1)
  (beginning-of-line)
  (message "edit-mode !"))
;; like O
(defun vim-O ()
  (interactive)
  (view-mode 0)
  (open-line 1)
  (beginning-of-line)
  (message "edit-mode !"))
;; like x
(defun vim-del-char ()
  (interactive)
  (view-mode 0)
  (delete-char 1)
  (view-mode 1)
  (message "delete-char"))
;; like c
(defun vim-del-char-to-insert ()
  (interactive)
  (view-mode 0)
  (delete-char 1)
  (message "delete-char and edit mode !"))
;; like u
(defun vim-undo ()
  (interactive)
  (view-mode 0)
  (undo-tree-undo)
  (view-mode 1)
  (message "undo !"))
;; like C-r
(defun vim-redo ()
  (interactive)
  (view-mode 0)
  (undo-tree-redo)
  (view-mode 1)
  (message "redo !"))
;; like Y
(defun vim-copy-line (arg)
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))
;; like P
(defun vim-P ()
  (interactive)
  (view-mode 0)
  (beginning-of-line)
  (yank)
  (beginning-of-line)
  (forward-line -1)
  (view-mode 1)
  (message "yank !"))
;; like p
(defun vim-p ()
  (interactive)
  (view-mode 0)
  (yank)
  (view-mode 1)
  (message "yank !"))
;; like w
(defun forward-word+1 ()
  (interactive)
  (forward-word)
  (forward-char))
;; like %
(defun vim-jump-brace()
  "Jump to correspondence parenthesis"
  (interactive)
  (let ((c (following-char))
				(p (preceding-char)))
		(if (eq (char-syntax c) 40) (forward-list)
			(if (eq (char-syntax p) 41) (backward-list)
				(backward-up-list)))))
;; Delete from cursor position to beginning-of-line
(defun vim-backward-kill-line (arg)
  "Kill chars backward until encountering the beginning of line."
  (interactive "p")
  (view-mode 0)
  (kill-line 0)
  (view-mode 1)
  (message "backward-kill-line"))

(provide 'keys-in-view-mode)
;;; keys-in-view-mode.el ends here
```

## View-modeにもともと備わっているキーバインド

- Backspace : 1ページ分前スクロールする
- z : 1ページ分スクロールする
- d : 半ページ分スクロールする(この設定では、dired-jumpに変更してます)
- u : 半ページ分前スクロールする(この設定では、undoに変更してます)
- < : バッファー先頭へ移動する
- > : バッファー末尾へ移動する
- RET : 1行スクロールする
- y : 1行前スクロールする(この設定では、 copy-region-as-killに変更してます)
- / : 前方向検索をする
- \ : 後方向検索をする
- . : マーク
- n : 次検索をする(この設定では、neotreeに変更してます)
- p : 前検索をする(この設定では、yankに変更してます)
- s : iserch-forward(この設定では、swiper-migemoに変更してます)
- r : iserch-backward(この設定では、redoに変更してます)
- q : Quit
- ? : ヘルプ
- h : ヘルプ(この設定では、backward-charに変更してます)

いづれも設定で上書きすれば変更できます。



## Emacsにおけるview-mode環境の設定

いろんな方のTipsを見ると、概ね下記の事例が多いです。

- view-modeで開きたいモードをadd-hookで定義する
- find-fileコマンドをview-modeで開くように設定する

わたしも試しましたが、いづれもいろいろと制約が生じて使いづらいです。
特にfind-fileをview-modeで開く設定にすると何かと不都合が生じるので、私の場合は、以下のようにしています。

```
1. diredのRETキーをview-modeで開く設定にする。vキーの機能をRETに割り当てるだけです。
2. 編集モードで開く機能は、oキーに割当てます。
3. find-fileでファイルを開くときは、編集モードで開く（当然新規ファイルも編集モード）
```

## Switch−buffer / Neotreeからもview-modeでファイルを開きたい

switch-bufferや、neotreeは、find-fileでファイルを開きますので、このモードからのみview-modeで開くためにはカスタマイズが必要です。

現状、ivy-switch-bufferの場合は、ivy--switch-buffer-actionの関数、neotreeの場合は、neo-open-fileの関数の中にある、find-file コマンドをview-fileコマンドに書き換えて対応していますが、updateで変わってしまう可能性もあるので、もう少しスマートな方法を模索中です。


## Evilではだめなの?

Evilも使ってみました。面白いとは思いましたが、Emacsモードとvimモードとの行き来が煩わしく使う気にはなりませんでした。
そこまでこだわるならvimそのものを使うほうが賢明だと思うからです。
EmacsとVimの比較論をするつもりはありませんが文章書きがメインなわたしの場合は、Emacsの方が遥かに使いやすいです。

ただ、ファイルの閲覧中に検索とかの作業をしていて、うっかりゴミを書き込んでしまうというようなことが多く、とくに設定ファイルの場合は致命傷になります。やむなくVimを起動して修正する…そんなシーンに何度も悩まされました。view-mode優先で使うことを思いついたのはそこが原点です。

メインはEmacs、非常用としてVimという住み分けが今のわたしの環境です。

## おまけの機能

#### ※view-modeのときだけ有効になる機能を幾つか付加しています。

- mode-lineの色を変える
- hl-line-modeを有効にする
- neotreeを使えるようにする
- 印刷できるようにする(環境によって設定は変わります。わたしの場合はMac)

#### ※あるとかえって紛らわしいキーに自分用の機能を割り当てています。

- "s" iserchは使わないので、swiperを使えるようにする。
- "d" vim likeな "d" は先ず使うことがないので、よく使うdired-jumpにしている。
- "m" 運よく空いてたのでよく使うmagit-statusを割り当てる。
- "n" iserch付属の機能(次検索をする)も使わないので、neotreeに割り当てる。
- " : " view-modeで編集するとauto-saveされないこともあるので、" : " にsave-bufferを割り当てました。

これらはみな好みの問題だと思うのであくまで参考です。

