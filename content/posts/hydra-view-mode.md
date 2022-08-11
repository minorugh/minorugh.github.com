+++
date = "2018-04-30T18:16:35+09:00"
categories = ["emacs"]
tags = ["hydra"]
title= "Hydraを使ったview-mode設定"
+++
以前にご紹介した下記の記事を、hydra.elを使って更に使いやすく工夫してみた。

* [Emacsのview-modeでvimのコマンドを使えるようにする](https://snap.textgh.org/201801041902/) 

まずは、設定ファイルの全容を貼り付けます。

```lisp
;; Change mode-line color in view-mode
(use-package viewer)
(setq viewer-modeline-color-view "dark red")
(viewer-change-modeline-color-setup)

;; View-mode key map
(use-package keys-in-view-mode)
(with-eval-after-load 'view
  (bind-key "h" 'backward-char view-mode-map)
  (bind-key "j" 'next-line view-mode-map)
  (bind-key "k" 'previous-line view-mode-map)
  (bind-key "l" 'forward-char view-mode-map)
  (bind-key "b" 'scroll-down view-mode-map)
  (bind-key "0" 'beginning-of-line view-mode-map)
  (bind-key "e" 'end-of-line view-mode-map)
  (bind-key "g" 'goto-line view-mode-map)
  (bind-key "G" 'View-goto-percent view-mode-map)
  (bind-key "d" 'view-kill-whole-line view-mode-map)
  (bind-key "m" 'magit-status view-mode-map)
  (bind-key "v" 'vc-diff view-mode-map)
  (bind-key "," 'howm-remember view-mode-map)
  (bind-key "u" 'view-undo view-mode-map)
  (bind-key "r" 'view-redo view-mode-map)
  (bind-key "%" 'view-jump-brace view-mode-map)
  (bind-key "w" 'forward-word+1 view-mode-map)
  (bind-key "W" 'backward-word view-mode-map)
  (bind-key "o" 'new-line-below-insert view-mode-map)
  (bind-key "y" 'copy-region-as-kill view-mode-map)
  (bind-key "Y" 'view-copy-line view-mode-map)
  (bind-key "p" 'paste-at-cursor view-mode-map)
  (bind-key "P" 'paste-at-down-line view-mode-map)
  (bind-key "x" 'view-del-char view-mode-map)
  (bind-key "X" 'view-backward-kill-line view-mode-map)
  (bind-key "D" 'kill-end-of-line view-mode-map)
  (bind-key "a" 'forward-char-to-insert view-mode-map)
  (bind-key "A" 'end-of-line-to-insert view-mode-map)
  (bind-key "i" 'view-mode view-mode-map)
  (bind-key "I" 'beginning-of-line-to-insert view-mode-map)
  (bind-key "L" 'pdf-preview-buffer view-mode-map)
  (bind-key ":" 'view-mode view-mode-map)
  (key-chord-define view-mode-map "gg" 'View-goto-line))

;; Add hl-line-mode to view-mode
(when (functionp 'hl-line-mode)
  (add-hook 'view-mode-hook '(lambda () (hl-line-mode 1)))
  (defadvice view-mode-disable (after disable-hl-line-mode activate)
    (hl-line-mode -1)))

;; Customize to open file in view-mode
(defun neo-open-file (full-path &optional arg)
  (neo-global--select-mru-window arg)
  (view-file full-path))

;; PDF-preview
  (with-eval-after-load 'view
    (use-package pdf-preview)
    (setq pdf-preview-font-rescale-factor 1.2)
    (when (eq system-type 'darwin)
      (setq pdf-preview-preview-command "open -a Preview.app"))
    (when (eq system-type 'gnu/linux)
      (setq pdf-preview-preview-command "evince"))
    (setq ps-line-number t))

;; hydra-view-mode
(defhydra hydra-view-mode (:hint nil :exit t)
  "
^Insert mode^        ^Move^            ^Delete^            ^Editing^             ^Utl
^^^^^^------------------------------------------------------------------------------------------
_a_: after cursor  _spc_: next page    _d_: one line       _u_: undo  _r_: redo    _m_: magit-status
_i_: at the cursor   _b_: prev page    _x_: one char       _y_: copy region      _v_: vc-diff
_A_: at end line     _w_: next word    _X_: to begin line  _Y_: copy line        _n_: neotree
_I_: at begin line   _W_: prev word    _D_: to end line    _p_: paste at cursor  _L_: PDF preview
_o_: new line below  _%_: match parens                   _P_: paste down line  _,_: remember
"
  ;; Insert mode
  ("a" forward-char-to-insert)
  ("i" view-mode)
  ("A" end-of-line-to-insert)
  ("I" beginning-of-line-to-insert)
  ("o" new-line-below-insert)
  ;; Move
  ("b" scroll-down)
  ("spc" scroll-up)
  ("w" forward-word+1)
  ("W" backward-word)
  ("%" view-jump-brace)
  ;; Delete
  ("d" view-kill-whole-line)
  ("x" view-del-char)
  ("X" view-backward-kill-line)
  ("D" kill-end-of-line)
  ;; Editing
  ("u" view-undo)
  ("r" view-redo)
  ("y" copy-region-as-kill)
  ("Y" view-copy-line)
  ("p" paste-at-cursor)
  ("P" paste-at-down-line)
  ;; Utl
  ("m" magit-status)
  ("v" vc-diff)
  ("n" neotree-toggle)
  ("," howm-remember)
  ("L" pdf-preview-buffer)
  ("q" nil "leave"))

(define-key view-mode-map "." 'hydra-view-mode/body)

```
## hydraで対話式にコマンドを使えるようにする
![Alt Text](https://c1.staticflickr.com/1/868/26930372267_800476a1d7_b.jpg) 

view-modeのkey-mapを覚えてしまえば必要ないのですが、ボケ始めた頭が混乱することもあるので、「.」を押すだけでhydraメニューがpopupします。

hydraを使ったミニバッファーメニューは、"nil" を渡すまで永続的に残りますが、`:exit` オプションを書くことで、目的のキーをセレクトした瞬間にメニューが消えるので便利です。

## view-mode 用のkey定義ファイル

```keys-in-view-mode.el``` を作成して、use-packegeで読み込ませています。

```lisp
;; Key bind for view-mode-vim-like
;; ================================
;; like a
(defun forward-char-to-insert ()
  (interactive)
  (view-mode 0)
  (forward-char 1)
  (message "edit-mode !"))
;; like A
(defun end-of-line-to-insert ()
  (interactive)
  (view-mode 0)
  (end-of-line)
  (message "edit-mode !"))
;; like I
(defun beginning-of-line-to-insert ()
  (interactive)
  (view-mode 0)
  (beginning-of-line)
  (message "edit-mode !"))
;; like dd
(defun view-kill-whole-line ()
  (interactive)
  (view-mode 0)
  (kill-whole-line)
  (save-buffer)
  (view-mode 1)
  (message "kill-whole-line"))
;; like D
(defun kill-end-of-line ()
  (interactive)
  (view-mode 0)
  (kill-line)
  (save-buffer)
  (view-mode 1)
  (message "kill-line"))
;; like o
(defun new-line-below-insert ()
  (interactive)
  (view-mode 0)
  (forward-line)
  (open-line 1)
  (beginning-of-line)
  (message "edit-mode !"))
;; like x
(defun view-del-char ()
  (interactive)
  (view-mode 0)
  (delete-char 1)
  (save-buffer)
  (view-mode 1)
  (message "delete-char"))
;; undo
(defun view-undo ()
  (interactive)
  (view-mode 0)
  (undo-tree-undo)
  (save-buffer)
  (view-mode 1)
  (message "undo !"))
;; redo
(defun view-redo ()
  (interactive)
  (view-mode 0)
  (undo-tree-redo)
  (save-buffer)
  (view-mode 1)
  (message "redo !"))
;; like Y
(defun view-copy-line (arg)
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))
;; like p
(defun paste-at-cursor ()
  (interactive)
  (view-mode 0)
  (yank)
  (save-buffer)
  (view-mode 1))
;; like P
(defun paste-at-down-line ()
  (interactive)
  (view-mode 0)
  (beginning-of-line)
  (yank)
  (beginning-of-line)
  (forward-line -1)
  (save-buffer)
  (view-mode 1)
  (message "yank !"))
;; like w
(defun forward-word+1 ()
  (interactive)
  (forward-word)
  (forward-char))
;; like %
(defun view-jump-brace ()
  "Jump to correspondence parenthesis"
  (interactive)
  (let ((c (following-char))
				(p (preceding-char)))
		(if (eq (char-syntax c) 40) (forward-list)
			(if (eq (char-syntax p) 41) (backward-list)
				(backward-up-list)))))

;; Delete from the cursor position to the beginning of the line : like X
(defun view-backward-kill-line (arg)
  "Kill chars backward until encountering the end of a line."
  (interactive "p")
  (view-mode 0)
  (kill-line 0)
  (save-buffer)
  (view-mode 1)
  (message "backward-kill-line"))

(provide 'keys-in-view-mode)
;;; keys-in-view-mode.el ends here:
```

## 今後の方向性を考える
view-modeでvimのコマンドを使えるようにすることを目標に工夫してきましたが、emacs自身の設定で便利に使える機能まであえてvim-likeにする必要はないと思います。

例えば、カーソル移動の機能、「行頭、行末、ページの先頭、ページの最後」は、sequeqtial-command というパッケージを導入していて使い慣れているのでview-modeの機能からは省いてもいいかなと思っています。

```lisp
;; sequential-command
(use-package sequential-command-config)
(sequential-command-setup-keys)
```
* C-aを連続で打つことで行の先頭→ページの先頭→元の位置とカーソルが移動する。
* C-eを連続で打つことで行の最後→ページの最後→元の位置とカーソルが移動する。

また、view-modeから抜けないで削除やyank&pasteする機能もあれば便利かも知れませんが、いろんな削除パターンから編集モードへ移行するコマンド群は不要だと思います。そんなコマンドを覚えるくらいなら、編集モードへ抜けてから自由にすればいいからです。

## まとめ
このTipsの目的は、Emacsをvim-likeに使うことではなくて、ファイルの閲覧をview-modeで扱うときの利便性を高めることです。
emacsをvimのように使うというようなTipsも多いですが、そんなことをするくらいならvimを使えばいいからです。

私は、emacsとvimとの両刀使いです。基本はemacsですが、dotfileやmakefileを扱うときは、vimを使うことが多いです。




