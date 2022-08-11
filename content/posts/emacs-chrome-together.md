+++
date = "2018-12-03T22:38:54+09:00"
categories = ["emacs"]
tags = ["chrome"]
title = "EmacsとChromeとのシームレスな連携"
+++

Emacsの設定にハマりだすとメールやインターネットプラウザなど全てのワークをEmacsで完結させよう…と考えるようになります。私もそんな一人でした。あれこれと設定を試すプロセスもまた楽しいからです。でもEmacsつながりでご縁ができたあるプログラマーの方のブログ [Solist Work Blog](https://solist.work/blog/) を読んで少しずつ考え方が変わってきました。

気付かされたことは、全てをEmacsで扱うために窮屈な使い方をするより、作業デスクとしてのEmacsと使い慣れたシステムやアプリとが、シームレスな感覚で連携できることのほうが遥かに意義があるということでした。まだまだ試行錯誤中で十分にまとまった記事にはなっていませんが備忘録として公開します。


## 外部モニターを併用する
これは必須ではないと思いますが実際に使ってみると実に快適です。私の場合は、メインマシンMacbook Pro に 24インチの外部モニターを接続しています。解像度とかにこだわらなければ 
[１~２万円で十分実用できるモニター](https://www.google.co.jp/search?q=%E5%A4%96%E9%83%A8%E3%83%A2%E3%83%8B%E3%82%BF%E3%83%BC+%EF%BC%92%EF%BC%94%E3%82%A4%E3%83%B3%E3%83%81&source=univ&tbm=shop&tbo=u&sa=X&ved=0ahUKEwi05fi8oYXfAhWCQN4KHVjVAlYQsxgILA&biw=1920&bih=1018) が購入できます。正面にMacbookのEmacs画面を見ながら右サイドの外部ディスプレーでChromeを開いている…というのが定番のスタイルです。



![emacs-chrome](https://c1.staticflickr.com/5/4909/45255050955_fea3e33025_b.jpg) 

Macの場合、２画面のデスクトップを自在に切り替えできます。私の場合 ctrl+1 <-> ctrl+2 で切り替えできるように設定して、A画面でEmacs、B画面でiTermというような使い方ができます。外部ディスプレーの方は、ほぼChrome専用という使い方ですが、[BetterTouchTool](https://folivora.ai/) というアプリを使えば表示画面の移動などが自在にできるので便利です。



## EmacsとChromeとのシームレスな連携
Emacsでブログの文章を書いたり、プログラミングを試行したりしているとき、作業の手を止めることなく検索できたり必要な資料の載っているWEBページを一発で閲覧できるとすごく便利です。一方、Chrome（プラウザ）の textarea に長文の入力が必要なときは、Emacsで入力できるほうがストレスが少ないです。以下はそのための設定です。

### <i class="fa fa-search"></i> 色々WEB検索するための設定

Melpaから [search-web](https://github.com/tomoya/search-web.el) というPackageをインストールします。このパッケージは、[google-this](URL )と同じコンセプトで、regionやカーソルのある部分の単語など空気を読んで検索してくれます。

各検索サイトがどのような引数で修理しているかさえ解れば自由にカスタマイズして応用できます。WEBページからkeywordを入力して検索するとその結果がURLボックスに表示されます。表示されたURLから引数を分析すればいいのです。私の場合、俳句でよく使う 古語検索/類語検索/季語検索などを登録しています。

```emacs-lisp
(use-package search-web)
(add-to-list 'search-web-engines '("weblio" "http://weblio.jp/content/%s" nil))
(add-to-list 'search-web-engines '("kobun" "http://kobun.weblio.jp/content/%s" nil))
(add-to-list 'search-web-engines '("ruigo" "http://thesaurus.weblio.jp/content/%s" nil))
(add-to-list 'search-web-engines '("kigo" "http://www.mysai.net/cgi-bin/kigo_search.cgi?txtKigo=%s&rdoSelect=1" nil))
(add-to-list 'search-web-engines '("github" "https://github.com/search?utf8=✓&q=%s&ref=simplesearch" nil))
(add-to-list 'search-web-engines '("qitta" "https://qiita.com/search?q=%s" nil))
(add-to-list 'search-web-engines '("post" "https://postcode.goo.ne.jp/search/q/%s/" nil))
(add-to-list 'search-web-engines '("earth" "https://earth.google.com/web/search/%s/" nil))

(bind-key
 "s-s"
 (defhydra hydra-search-web (:hint nil :exit t)
   "
[Search]  _a_:Amazon  _g_:Google  _m_:Maps  _e_:Earth  _h_:GitHub  _q_:Qitta  _w_:Weblio  _p_:Post  _b_:古文  _r_:類語"
  ("a" (search-web-dwim "amazon jp"))
  ("e" (search-web-dwim "earth"))
  ("g" (search-web-dwim "google"))
  ("m" (search-web-dwim "google maps"))
  ("h" (search-web-dwim "github"))
  ("q" (search-web-dwim "qitta"))
  ("w" (search-web-dwim "weblio"))
  ("p" (search-web-dwim "post"))
  ("k" (search-web-dwim "kigo"))
  ("b" (search-web-dwim "kobun"))
  ("r" (search-web-dwim "ruigo"))))
```

### <i class="fa fa-bookmark-o"></i> お気に入りのページを開いたりアプリを起動させたりするための設定

Melpaにあるpackage [hydra](https://github.com/abo-abo/hydra) を使うことでお好みのミニバッファーメニューを構築できます。主にEmacsの編集をしているときに必要となるサイトを設定しておくと便利です。私の場合、Emacsから起動できたら便利かなと思うApplicationもいくつか設定しています。

![Alt Text](https://c1.staticflickr.com/5/4915/44353090030_e646922131_b.jpg) 

```emacs-lisp
(key-chord-define-global
 "kl"
 (defhydra hydra-browse (:hint nil :exit t)
   "
 ^Shop^           ^SNS^           ^Repos^          ^GH^           ^Favorite^      ^Others^        ^Applications^
 ^^^^^^------------------------------------------------------------------------------------------------
 _a_: Amazon      _t_: Twitter    _g_: minorugh    _h_: HOME      _j_: Jorudan    _D_: Dropbox    _c_: Calendar
 _r_: Rakuten     _u_: Youtube    _0_: my gist     _d_: d.kukai   _n_: News       _x_: Xserver    _l_: ForkLift
 _y_: Yodobashi   _f_: Flickr     _1_: masasam     _m_: m.kukai   _w_: Weather    _q_: Qiita      _@_: AirMail
 _k_: Kakaku      _e_: Evernote   _2_: gitbucket   _b_: BBS       _s_: SanyoBas   _p_: Pocket     _i_: iTerm
"
   ("a" (browse-url "https://www.amazon.co.jp/"))
   ("r" (browse-url "https://www.rakuten.co.jp/"))
   ("y" (browse-url "https://www.yodobashi.com/"))
   ("k" (browse-url "http://kakaku.com/"))
   ("t" (browse-url "https://twitter.com"))
   ("u" (browse-url "https://www.youtube.com/channel/UCnwoipb9aTyORVKHeTw159A/videos"))
   ("f" (browse-url "https://www.flickr.com/photos/minorugh/"))
   ("e" (browse-url "https://www.evernote.com/client/web#?an=true&n=02ab918a-18a7-472b-a166-835a922d3fad&s=s278&"))
   ("g" (browse-url "https://github.com/minorugh/emacs.d"))
   ("0" (browse-url "https://gist.github.com/minorugh"))
   ("1" (browse-url "https://github.com/masasam/dotfiles/tree/master/.emacs.d"))
   ("2" (browse-url "http://localhost:8080/minoru"))
   ("h" (browse-url "http://gospel-haiku.com/"))
   ("d" (browse-url "http://gospel-haiku.com/d_kukai/"))
   ("m" (browse-url "http://gospel-haiku.com/m_kukai/"))
   ("b" (browse-url "http://gospel-haiku.com/danwa/"))
   ("B" (browse-url "https://app.box.com/folder/0"))
   ("j" (browse-url "https://www.jorudan.co.jp/"))
   ("n" (browse-url "https://news.yahoo.co.jp/"))
   ("w" (browse-url "https://tenki.jp/week/6/31/"))
   ("s" (browse-url "http://www.sanyo-bus.co.jp/pdf/180913.pdf"))
   ("x" (browse-url "https://www.xserver.ne.jp/login_server.php"))
   ("D" (browse-url "https://www.dropbox.com/home/emacs.d"))
   ("q" (browse-url "https://qiita.com/minoruGH"))
   ("v" (browse-url "https://drive.google.com/drive/u/0/my-drive"))
   ("P" (browse-url "https://photos.google.com/?pageId=none"))
   ("p" (browse-url "https://getpocket.com/a/queue/list/"))
   ("." hydra-dired/body)
   ("/" hydra-works/body)
   ("c" my/calendar-app)
   ("l" my/forklift-app)
   ("@" my/amail-app)
   ("i" my/iterm-app)))

;; Launch for Mac applications
(defun my/calendar-app ()
  "Launch for calendar.app."
  (interactive)
  (shell-command "open -a calendar.app"))
(defun my/forklift-app ()
  "Launch for forklift.app."
  (interactive)
  (shell-command "open -a forklift.app"))
(defun my/amail-app ()
  "Launch for airmail3.app."
  (interactive)
  (shell-command "open -a 'airmail 3.app'"))
(defun my/iterm-app ()
  "Launch for iterm.app."
  (interactive)
  (shell-command "open -a iterm.app"))
```

### <i class="fa fa-edit"></i> ChromeのテキストエリアをEmacsでリアルタイム編集するための設定

Google Chromeに拡張機能
[Atomic Chrome](https://chrome.google.com/webstore/detail/atomic-chrome/lhaoghhllmiaaagaffababmkdllgfcmc) 
をインストールします。つづいて、EmacsにMelpaから[atmic-chrome.el](https://github.com/alpha22jp/atomic-chrome)をパッケージインストールしてinit.elに下記の設定を追加します。私の場合、markdownでも書けるように設定しています。

```emacs-lisp
;; atomic-chrome
(atomic-chrome-start-server)
(setq atomic-chrome-default-major-mode 'markdown-mode)
```

![Alt Text](https://c1.staticflickr.com/5/4823/31230493847_0856377333_b.jpg) 



## ChromeからEmacsへ色々Captureする

Chromeに拡張機能 [Org Capture](https://chrome.google.com/webstore/detail/org-capture/kkkjlfejijcjgjllecmnejhogpbcigdc) 
をインストールします。導入の詳細は
[このページ](https://github.com/sprig/org-capture-extension) を見よ…とのことで、いろいろ設定が必要なように書かれているのですが、私の場合はMac側の設定は何もしない状態でも動きました。

また、org-capture-templates の設定例も書かれていましたがうまく動かなところもあってので少し変えました。WEBページを開いた状態でChromeの拡張機能のアイコンをクリックするとorg-captureの入力画面が起動しWEBページの情報が自動的にCaptureされます。

### <i class="fa fa-copy"></i> region選択してCaptureする

WEBページの必要なCODEの箇所などをregion選択してCaptureアイコンをクリックすると capture-template の "p" の設定に従って反応します。下記の画像ではTitleを入力するためにミニバッファーが入力待ちになっています。

![Alt Text](https://c1.staticflickr.com/5/4854/45258811415_99060f8ae9_b.jpg)


### <i class="fa fa-link"></i> 何も選択しないでCaptureする（bookmarkするだけ）

region選択なしでChromeのCaptureアイコンをクリックした場合は、"L" の設定に従って反応します。

![Alt Text](https://c1.staticflickr.com/5/4911/44354397670_5d66b85f8c_b.jpg) 

Captureされたリンクにカーソルを置いて、"C-c o" でプラウザを開くことができます。

org-modeの設定は、『[効率よくプログラミングを覚えるためにしていること](https://solist.work/blog/posts/information-organizize-method/) 』の記事で紹介されているものをベースにして、orgファイルを閲覧するためのミニバッファーメニューを [hydra](https://github.com/abo-abo/hydra)で設定しました。viewモードで開くほうがキー操作が楽ですし、Tabキーでの折り畳み操作に便利なようにカーソルをバッファーの先頭行へ移動させています。

```emacs-lisp
(use-package org)
(setq org-log-done 'time)
(setq org-use-speed-commands t)
(setq org-src-fontify-natively t)
(setq org-agenda-files '("~/Dropbox/org/"))
(setq calendar-holidays nil)

(bind-key "C-c a" 'org-agenda)
(bind-key "C-c c" 'org-capture)
(bind-key "C-c o" 'org-open-at-point) ;; Open link

(setq org-capture-templates
      '(("p" "Code capture with Chrome" entry (file+headline "~/Dropbox/org/code.org" "Code")
	 "* %^{Title} \nSOURCE: %:link\nCAPTURED: %U\n\n #+BEGIN_QUOTE emacs-lisp\n%i\n#+END_QUOTE\n" :prepend t)
	("L" "Link capture with Chrome" entry (file+headline "~/Dropbox/org/links.org" "Links")
         "* [[%:link][%:description]] \nCAPTURED: %U\nREMARKS: %?" :prepend t)
	("m" "Memo" entry (file+headline "~/Dropbox/org/memo.org" "Memo")
	 "* %? %U" :prepend t)
	("r" "Remember" entry (file+headline "~/Dropbox/org/remember.org" "Remember")
	 "* %? %i" :prepend t)
	("t" "Task" entry (file+headline "~/Dropbox/org/task.org" "Task")
	 "** TODO %? \n   SCHEDULED: %^t \n" :prepend t)
	("f" "Future Task" entry (file+headline "~/Dropbox/org/task_future.org" "Future")
	 "** TODO %? \n" :prepend t)))

(setq org-refile-targets
      (quote (("~/Dropbox/org/archives.org" :level . 1)
	      ("~/Dropbox/org/remember.org" :level . 1)
	      ("~/Dropbox/org/memo.org" :level . 1)
	      ("~/Dropbox/org/task.org" :level . 1))))


;; Open GTD files with hydra
(bind-key
 "C-;"
 (defhydra hydra-gtd (:hint nil :exit t)
   "
[GTD] _a_:Archives  _m_:Memo  _c_:Code  _l_:Links  _r_:Remember  _t_:Task  _f_:Future_Task"
  ("a" my/archives-org)
  ("m" my/memo-org)
  ("c" my/code-org)
  ("l" my/links-org)
  ("r" my/remember-org)
  ("t" my/task-org)
  ("f" my/future-org)
  ("." hydra-dired/body)
  ("," hydra-browse/body)
  ("q" keyboard-quit)))

;; Open GTD files in view-mode
(defun my/archives-org ()
  "Open archives.org."
  (interactive)
  (view-file "~/Dropbox/org/archives.org")
  (goto-char (point-min)))
(defun my/code-org ()
  "Open code.org."
  (interactive)
  (view-file "~/Dropbox/org/code.org")
  (goto-char (point-min)))
(defun my/links-org ()
  "Open links.org."
  (interactive)
  (view-file "~/Dropbox/org/links.org")
  (goto-char (point-min)))
(defun my/remember-org ()
  "Open remember.org."
  (interactive)
  (view-file "~/Dropbox/org/remember.org")
  (goto-char (point-min)))
(defun my/memo-org ()
  "Open memo.org."
  (interactive)
  (view-file "~/Dropbox/org/memo.org")
  (goto-char (point-min)))
(defun my/future-org ()
  "Open task_futurear.org."
  (interactive)
  (view-file "~/Dropbox/org/task_future.org")
  (goto-char (point-min)))
(defun my/task-org ()
  "Open archives.org."
  (interactive)
  (view-file "~/Dropbox/org/task.org")
  (goto-char (point-min)))
```
### <i class="fa fa-bookmark-o"></i> ブックマークの管理について考えてみる

『[Chromeのブックマークの整理法](https://solist.work/blog/posts/chrome/)』
の記事がとても参考になりました。私の場合、次のような棲み分けをしています。

- Chromeから頻繁にアクセスするWEBページ 
&ensp;<i class="fa fa-arrow-right"></i> ブックマークバーに入れる（アイコンのみにするとたくさん入れられる）
- あとでもう一度見たい…というような個別の記事 
&ensp;<i class="fa fa-arrow-right"></i> Pocketに入れる（ゴミ屋敷にならないように時々掃除する）
- Emacsでの作業に関連があって情報資源として残しておきたいもの 
&ensp;<i class="fa fa-arrow-right"></i> org-captureする（これも時々棚卸し）

ChromeのブックマークバーやPocketに保存したものはスマホにも自動的に同期されます。org-caputerしたものは [Orgzly](https://play.google.com/store/apps/details?id=com.orgzly&hl=ja) をインストールしてDropboxで同期しておけばスマホで見ることができます。GitHubもスマホアプリで閲覧できます。このようにスマホから自宅Workのチラ見ができようにしておくことはとても重要です。パソコンと睨めっこしていて思考が行き詰まったとき、バスや電車での移動中にリラックスしてスマホでチラ見ているとふと柔軟な発想が授かる…という経験を何度もしています。きちんとしたコンセプトのもとにブックマークを整理しておいて、いつでもどんな場所からでもすぐに見つけられるようにしておくというのはとても大事なことだと思います。


