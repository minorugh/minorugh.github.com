+++
date = "2022-10-12T13:21:38+09:00"
categories = ["emacs"]
tags = ["translate"]
title = "Google-translateからDeepl-translateに乗り換えた話"
+++
Emacsから翻訳するのにGoogle-translateを愛用していたが、Slackのemacs-jpでGoogleよりも高性能だと評判の
[DeePL](https://www.deepl.com/ja/translator) 
を知ったので、早速導入してみた。

## EmacsからDeePLを使う
"EmacsからDeePLを使う" で調べると下記3種類のTipsが見つかった。いづれもリージョン選択範囲を翻訳させるものである。

### 1. masatoi/deepl.el
* https://qiita.com/masatoi0/items/26c7384749393a1bf8fb

翻訳結果はミニバッファーに表示され、同じ内容がクリップボードにコピーされる。

オリジナルのコード
（[deepl.el](https://gist.github.com/masatoi/ec90d49331e40983427025f8167d01ee) ）
は、`load-file` して使う仕様になっていたので、`el-get` でパッケージインストールできるようにアレンジしたものを自分のGitHubに置いている。DeePLにユーザー登録してAPIキーの取得が必要。

* [minorugh/deepl-translate](https://github.com/minorugh/deepl-translate) 

### 2. lorniu/go-translate.el
* https://hangstuck.com/emacs-deepl/

翻訳結果は、ウインドウ分割でOther-window-bufferに表示される。Google-translateとの同時使用が可能なので比較して選べる。

buffer表示のほかにposframeやpopup-windowなどカスタマイズできる。
Melpaからパッケージインストールできるが、こちらもDeePLユーザー登録してAPIキーの取得が必要。

### 3. kdmsnr/deepl-translate.el
* https://gist.github.com/kdmsnr/7209a98e9ccacec449011e11f46fdcf7

brows-urlを使ってWEB版のDeePL翻訳を表示させるもの。リージョン選択範囲が自動で貼り付けられる。
パッケージツールではないので、設定コードを`init.el` に貼り付けるだけで良く、APIキーも不要。

## DeepL APIの認証キーの取得
EmacsからDeePLを使うためには、ユーザー登録をしてAPI認証キーを取得する必要がある。手順は下記の通り。
詳細は、[こちら](https://hangstuck.com/emacs-deepl/#toc4) がわかりやすい。 

* 公式サイトにアクセスします。
[DeepL](https://www.deepl.com/translator) 
* サイトの上の方に「API」と書いてあるところがあるのでそこをクリックします。
* 無料で登録するをクリックしてアカウント登録を済ませます。
* 登録後ログインしてアカウント情報を見るとAPI認証キーも表示されるのでコピーします。

## Emacsの設定
とりあえず全部設定して使い分けてみることにした。

```emacs-lisp
;; Deepl translation appears in minibuffer.
;; Also, the same content is copied to the clipboard
(leaf deepl-translate
  :el-get minorugh/deepl-translate
  :bind ("C-t" . deepl-translate)
  :custom (deepl-auth-key . "XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXX:XX"))


;; Deepl translate with go-translate
(leaf go-translate
  :ensure t
  :bind ("C-c t" . gts-do-translate)
  :config
  (setq gts-translate-list '(("en" "ja") ("ja" "en")))
  (setq gts-default-translator
		(gts-translator
		 :picker (gts-noprompt-picker)
		 :engines (list
				   (gts-deepl-engine :auth-key "XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXX:XX" :pro nil)
				   (gts-google-engine))
 		 :render (gts-buffer-render))))


;; Deepl translation on web page
(leaf my:deeple-traqslate
  :bind ("C-c d" . my:deepl-translate)
  :preface
  (defun my:deepl-translate (&optional string)
	(interactive)
	(setq string
          (cond ((stringp string) string)
				((use-region-p)
				 (buffer-substring (region-beginning) (region-end)))
				(t
				 (save-excursion
				   (let (s)
					 (forward-char 1)
					 (backward-sentence)
					 (setq s (point))
					 (forward-sentence)
					 (buffer-substring s (point)))))))
	(run-at-time 0.1 nil 'deactivate-mark)
	(browse-url
	 (concat
      "https://www.deepl.com/translator#en/ja/"
      (url-hexify-string string)
      ))))
```

## 評価
1. の`deepl.el` は、設定ファイルなどに書く短いコメント類の翻訳に適している。結果が気に入ればそのまま即yankで貼り付けできるので便利だ。自分的には、一番利用機会が多い。

2. の`go-tranlate.el` は、Google-translateと併用表示させて、気に入ったほうを採用する…という感じに使える。
buffer表示できるので長文の場合でも見やすい。カスタマイズ性が高く自分好みにアレンジできる点も優れている。

3. の`my:deepl-translate` は、brows-urlでDeePLサイトを表示させるもので、心なしかレスポンスも悪い。何らかのトラブルでAPIアクセスができなくなったときの予備的な位置づけになるかと思う。

蛇足だが、[DeepLのChrome拡張機能](https://www.deepl.com/ja/chrome-extension)を追加して有効化しておくと Chrome画面上でリージョン選択後、右クリックで「選択したテキストで翻訳」からも利用できて便利だ。


