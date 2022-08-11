+++
date = "2017-06-09T20:40:03+09:00"
categories = ["emacs"]
tags = ["howm"]
title ="メモ機能に特化した超シンプルなhowm+Dropboxの設定"
+++

メモ機能に特化したhowmの設定を紹介します。

* GTDとしては使わないのでメニュー画面は使いません。
* メニュー画面は使いませんが検索などの基本機能は全て使えます。
* 新規投稿と一覧表示のみ直接使えるようにkey-bindしています。
* ファイルは、スマホや他の端末からも共有出来るようにDropboxで管理します。
* メモは、markdown-mode で書きます。理由は後述します。

## howm-memoの設定

```lisp
;*************************************************************************
;; howm-memo.el                       last updated: 2017/06/09
;*************************************************************************
;; M-x package-install howm
;; (use-package key-chord)
;; (use-package use-package-chords
;;   :config (key-chord-mode 1))
;;-----------------------------------------------------------------------
(use-package howm
  :init
  (setq howm-view-title-header "#"
        howm-directory "~/Dropbox/howm"
        howm-file-name-format "%Y/%m/%Y-%m%d-%H%M.md"
  :config
  (setq howm-view-split-horizontally t      ;; 一覧バッファと内容バッファを横に並べる
        howm-view-summary-persistent nil    ;; RET でファイルを開く際一覧バッファを消す
  :chords ((",," . howm-create)             ;; メモを書く
           ("@@" . howm-list-all)))         ;; メモ一覧を開く

;; 空になったファイルを尋ねずに自動削除：howm以外でも有効
(defun my:delete-file-if-no-contents ()
  (when (and (buffer-file-name (current-buffer))
             (= (point-min) (point-max)))
    (delete-file
     (buffer-file-name (current-buffer)))))
(if (not (memq 'my:delete-file-if-no-contents after-save-hook))
    (setq after-save-hook
          (cons 'my:delete-file-if-no-contents after-save-hook)))

;***********************************************************************
; end of file

```
