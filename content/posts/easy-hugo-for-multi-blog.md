+++
date = "2017-11-22T17:12:36+09:00"
categories = ["hugo","emacs"]
tags = ["blog"]
title= "easy-hugoでマルチブログライフを愉しむ"
+++

easy-hugo の作者 masasam さんが、我儘な私のリクエストに応えて数々のアップデートをしてくださいましたので、感謝の気持ちをこめて紹介させていただきます。
easy-hugo は、package-install できますが、頻繁に更新されているので、[作者のページ](https://github.com/masasam/emacs-easy-hugo) で最新情報をチェックされるといいです。

<!--more-->
![Alt Text](https://c2.staticflickr.com/2/1857/42814510480_df6feb8a6e_b.jpg)

## easy-hugo の特徴
easy-hugo は、hugo によるブログ運用を限りなく自動化することに特化した Lisp です。

- マルチブログに対応
- 対話式の GUI メニューで、初心者にもわかりやすい
- さまざまなユーザーニーズが配慮され
ているので、拡張性、Customize 性に富んでいる

## hugo によるマルチブログ環境
公開を目的としたブログは一つ、多くても二つというのが一般的で、私の場合もそうでしたが、
wordpress から hugo へ移行してみて、あまりの快適さに書庫代わりのようなブログが次々と増えていったのです。

- Emacsな日々の備忘録（Clmemo）
- 俳句とエッセイの記録（essay blog)
- 撮りためた写真の記録（Photo Blog）

私の場合は、全てのドメインとファイルを xserver で管理していますが、esay-hugo では、Github、Amazon S3、Google Cloud Storage などへの Deploy 設定も用意されています。

esay-hugo は、Emacs の上でそれらを渡り歩き、いとも簡単にマルチブログを管理できるのです。

## 拡張設定：ファイル名とハイパーリンクを自動生成させる
version 0.25以降のhugoでは、default.mdに ```title = "{{ replace .TranslationBaseName "-" " " | title }}"``` と書いておくと拡張子を除外したファイル名がそのままタイトルになりますが、私の場合は、下記のように最初にpublishしたときのタイムスタンプを付すようにしました。

```
201805021200.日本語ファイル名.md
```
また、hugoの場合、公開されるページのurlは、blogurl/post/failename というハイパーリンクになります。長いファイル名や日本語がハイパーリンクになるのは抵抗あるので、私の場合、blogurl/publishしたときのタイムスタンプになるようにしています。

この二つを実現するためにカスタマイズした関数を設定に書きました。
```lisp
;; easy-hugo-replace-key
(defun easy-hugo-replace-key (key val)
  (save-excursion
    (goto-char (point-min))
    ;; quoted value
    (if (and (re-search-forward (concat key " = \"") nil t)
             (re-search-forward "[^\"]+" (line-end-position) t))
        (or (replace-match val) t) ;; ensure we return t
      ;; unquoted value
      (when (and (re-search-forward (concat key " = ") nil t)
                 (re-search-forward ".+" (line-end-position) t))
        (or (replace-match val) t)))))

;; Customize the newpost function
(defun my-easy-hugo-newpost (post-file)
  "Customize the newpost function.
Automatically create file name with timestamp and embed timestamp in hyperlink"
  (interactive (list (read-from-minibuffer
                      "Filename: "
                      `(,easy-hugo-default-ext . 1) nil nil nil)))
  ;; Add a time stamp to the beginning of post-file
  (let ((filename (concat "post/" (format-time-string "%Y%m%d%H%M.") post-file))
        (file-ext (file-name-extension post-file)))
    (easy-hugo-with-env
     (when (file-exists-p (file-truename (concat "content/" filename)))
       (error "%s already exists!" (concat easy-hugo-basedir "content/" filename)))
     (call-process "hugo" nil "*hugo*" t "new" filename)
     (find-file (concat "content/" filename))
     ;; Automatically write title and url in file header
     (easy-hugo-replace-key "title" (file-name-sans-extension post-file))
     (easy-hugo-replace-key "url" (format-time-string "%Y%m%d%H%M"))
     (goto-char (point-max))
     (save-buffer))))

```
上記コマンドを実行することで新規投稿時にファイルのヘッダーに必要な情報が自動的に書き込まれます。

この機能に対応させるための default.md は以下の通りです。

##### archetypes/default.md
```toml
+++
 date = "{{ .Date }}"
 tags = [""]
 title = " "
 url = " "
+++
```

## 設定ファイル
設定ファイルの全容は、以下のとおりです。

試行錯誤を繰り返し、今も日進月歩ですので、最新版は、[私の Github](https://goo.gl/UyzLJA) に置いてあります。 

```lisp
(use-package easy-hugo
  :init
  ;; Main blog (=blog1)
  (setq easy-hugo-basedir "~/Dropbox/Web/textgh/snap/"
        easy-hugo-url "https://snap.textgh.org"
        easy-hugo-sshdomain "xsrv"
        easy-hugo-root "/home/minorugh/textgh.org/public_html/snap/"
        easy-hugo-previewtime "300")
  ;; Bloglist
  (setq easy-hugo-bloglist
       ;; blog2 setting
        '(((easy-hugo-basedir . "~/Dropbox/Web/textgh/essay/")
          (easy-hugo-url . "https://essay.textgh.org")
          (easy-hugo-sshdomain . "xsrv")
          (easy-hugo-root . "/home/minorugh/textgh.org/public_html/essay/"))
       ;; blog3 setting
         ((easy-hugo-basedir . "~/Dropbox/Web/textgh/hl/")
          (easy-hugo-url . "https://hl.textgh.org")
          (easy-hugo-sshdomain . "xsrv")
          (easy-hugo-root . "/home/minorugh/textgh.org/public_html/hl/"))
       ;; blog4 setting
         ((easy-hugo-basedir . "~/Dropbox/Web/textgh/ryo/")
          (easy-hugo-url . "https://ryo.textgh.org")
          (easy-hugo-sshdomain . "xsrv")
          (easy-hugo-root . "/home/minorugh/textgh.org/public_html/ryo/"))))

  ;; Sort-publishday on startup
  (setq easy-hugo--sort-publishday-flg 1)
  (setq easy-hugo--sort-char-flg nil)
  (setq easy-hugo--sort-time-flg nil)

  ;; Customize for my help menu
  (setq easy-hugo-help-line 4)
  (setq easy-hugo-help
  "n .. New blog post    r .. Rename file     p .. Preview          g .. Refresh
d .. Delete post      a .. Seach blog ag   P .. Publish server   e .. Edit easy-hugo
S .. Sort char        s .. Sort time       < .. Previous blog    > .. Next blog
N .. No help [tab]    . .. Next postdir    o .. Open base dir    v .. View other window
"))
;; end of use-package settings //////////////////////////////////////

;; Key-settings
(bind-key [tab] 'easy-hugo-no-help easy-hugo-mode-map)
(bind-key [?\r] 'easy-hugo-view easy-hugo-mode-map) ;; enter key
(bind-key "v" 'easy-hugo-view-other-window easy-hugo-mode-map)
(bind-key "o" 'easy-hugo-open-basedir easy-hugo-mode-map)
(bind-key "r" 'easy-hugo-rename easy-hugo-mode-map)
(bind-key "n" 'my-easy-hugo-newpost easy-hugo-mode-map)
(bind-key "u" 'easy-hugo-sort-publishday easy-hugo-mode-map)
(bind-key "e" 'my/edit-easy-hugo easy-hugo-mode-map)
(bind-key "C-c e" 'easy-hugo)
(bind-key "C-c p" 'easy-hugo-preview)
(bind-key "C-c P" 'easy-hugo-publish)
(bind-key "C-c t" 'easy-hugo-complete-tags)

;; Modified sort-char function
(defun my-easy-hugo-sort-char ()
  "Sort article by characters in descending order."
  (interactive)
    (setq easy-hugo--sort-time-flg nil)
    (setq easy-hugo--sort-publishday-flg nil)
    (setq easy-hugo--sort-char-flg 2)
    (easy-hugo))

;; easy-hugo-replace-key
(defun easy-hugo-replace-key (key val)
  (save-excursion
    (goto-char (point-min))
    ;; quoted value
    (if (and (re-search-forward (concat key " = \"") nil t)
             (re-search-forward "[^\"]+" (line-end-position) t))
        (or (replace-match val) t) ;; ensure we return t
      ;; unquoted value
      (when (and (re-search-forward (concat key " = ") nil t)
                 (re-search-forward ".+" (line-end-position) t))
        (or (replace-match val) t)))))

;; Modified hugo-newpost function
(defun my-easy-hugo-newpost (post-file)
  "Customize the newpost function.
Automatically create file name with timestamp and embed timestamp in hyperlink"
  (interactive (list (read-from-minibuffer
                      "Filename: "
                      `(,easy-hugo-default-ext . 1) nil nil nil)))
  ;; Add a time stamp to the beginning of post-file
  (let ((filename (concat "post/" (format-time-string "%Y%m%d%H%M.") post-file))
        (file-ext (file-name-extension post-file)))
    (easy-hugo-with-env
     (when (file-exists-p (file-truename (concat "content/" filename)))
       (error "%s already exists!" (concat easy-hugo-basedir "content/" filename)))
     (call-process "hugo" nil "*hugo*" t "new" filename)
     (find-file (concat "content/" filename))
     ;; Automatically write title and url in file header
     (easy-hugo-replace-key "title" (file-name-sans-extension post-file))
     (easy-hugo-replace-key "url" (format-time-string "%Y%m%d%H%M"))
     (goto-char (point-max))
     (save-buffer))))

;; Required to use hugo-complete-tags
(use-package popup)

;; Edit this file
(defun my/edit-easy-hugo ()
  (interactive)
  (find-file "~/Dropbox/emacs.d/inits/80_easy-hugo.el"))

;;; 80_easy-hugo.el ends here
```

## 最後に
easy-hugo のファイル一覧は、デフォルトでは、記事のタイムスタンプ順に並びます。この設定の場合、記事の内容を修正、更新する度に、ファイルの並びが変わることになります。


私の場合は、ファイル一覧は、常に記事を作成した時系列順に並べておきたいので、デフォルトのファイルの並びを投稿日時順になるように設定しています。

デフォルトで投稿日時順でsortさせるための設定は以下のとおりです。

```lisp
;; Sort-publishday on startup
  (setq easy-hugo--sort-publishday-flg 1)
  (setq easy-hugo--sort-char-flg nil)
  (setq easy-hugo--sort-time-flg nil)
```

### masasam さんに感謝
ご紹介した設定の全ては、easy-hugo の作者、masasam さんのご指導によるものです。

Emacs-Lisp のイロハも知らなかった初心者の私ですが、easy-hugo と関わったお陰で、片言ながらも Lisp の読み書きができるようになり、パッチワークでカスタマイズできるまでになりました。

masasam さんに感謝、easy-hugo に感謝です。

