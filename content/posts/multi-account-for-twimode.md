+++
date = "2018-10-15T14:19:10+09:00"
categories = ["emacs"]
tags = ["twitter"]
title = "複数アカウトでtwittering-modeを使う "
+++
EmacsのTwittering-modeで複数アカウントを扱うという課題に挑戦してみました。アカウント毎にログイン用のgpgファイルを生成し、基本設定部分をその都度リッセトしながら切り替えているだけです。意外と簡単に実現できたので備忘録として残しておきます。設定を見ていただければ、"な〜んだ(^o^)" という落ちでしょうね。
<!--more-->

## 設定

```emacs-lisp
;; Clear existing twit buffers
(defun my/reload-twit ()
  (mapcar
   (lambda (buffer)
     (twittering-deactivate-buffer buffer)
     (kill-buffer buffer))
   (twittering-get-buffer-list))
  (twittering-unregister-killed-buffer)
  ;; Clear variables
  (setq twittering-private-info-file-loaded nil)
  (setq twittering-account-authorization nil)
  (setq twittering-oauth-access-token-alist nil)
  (setq twittering-buffer-info-list nil)
  (setq twittering-timeline-data-table (make-hash-table :test 'equal))
  (twit))

(defun my/twit-1 ()
  (interactive)
  (setq twittering-private-info-file (expand-file-name "~/.twittering-mode.gpg"))
  ;; timeline to read on startup
  (setq twittering-initial-timeline-spec-string '("minoruGH" ":mentions" ":home"))
  (my/reload-twit))

(defun my/twit-2 ()
  (interactive)
  (setq twittering-private-info-file (expand-file-name "~/Dropbox/dotfiles/twittering-mode2.gpg"))
  ;; timeline to read on startup
  (setq twittering-initial-timeline-spec-string '("gospelhaiku" ":mentions" ":home"))
  (my/reload-twit))

;; Twitterring-mode settings
(use-package twittering-mode)
(setq twittering-use-master-password t)

;; 以降、必要な共通設定を書きます。

```

## Twitterの公式アプリで複数アカウントを切り替える
紐付けのメールアドレスさえ確保できれば、複数アカウントの取得は何も問題ありませんが、
Emacsのtwittering-modeで複数環境を構築するには、アカウントごとにgpgファイルを生成させる必要があります。

そのために公式アプリで複数アカウントを切り替えられる用に設定しておくと便利です.

- [複数アカウントを公式アプリで切り替える方法](https://pc-etcetera.com/twitter-9/) 

## アカウントごとの twittering-mode.gpg を作る
いろんなやり方があると思いますが、私が試した最も手っ取り早い方法を紹介しておきます。

1. 上記で紹介した「複数アカウントを公式アプリで切り替える」準備ができたら、まずChromeとかのプラウザを起動し、まずサブのアカウントでログインしておきます。
2. 次にEmacsからtwittering-modeを起動するとプラウザがログイン中のTwitterとの連携を聞いてくるので承認するとPINコードが表示されるのでmini-bufferにコピーしてEnterします。
3. つづいてgpgのパスフレーズを聞かれるので任意のパスワードを入力（2回）するとTimelineが表示されます。
4. いちどEmacsを閉じて、~/.twittering-mode.gpg が作成されているか確認し、無事生成されていたらそのファイルをどこか別の場所に移動します。コピーではなく移動です。
5. 上記の設定例では、~/Dropbox/dotfiles/ に移動して、twittering-mode2.gpg にリネームしています。
6. プラウザでログインしているサブアカウントをログアウトして、メインのアカウントにログインし直します。
7. 以後、サブアカウントのときと同様の手順で、~/.twittering-mode.gpg を生成させます。
8. 以上で準備完了です。

Emacsを再起動して、 ````M-x my/twit-1```` / ````M-x my/twit-2```` と切り替えてみましょう。Emacsを起動するたびにパスワードを聞かれるのはやむを得ませんが、快適に切り替わるはずです。

私の場合、混乱しないためにメイン/サブアカウントとも同じパスワードにしています。不安であれば定期的に変えればいいだけです。





