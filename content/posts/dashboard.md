+++
date = "2017-10-16T15:09:06+09:00"
categories = ["emacs"]
tags = ["dashboard"]
title= "Emacsのスタート画面をイケメンにする"
+++
100分の1秒でもemacsの起動を早くしようと試行錯誤している一方、起動のたびに真黒なScratch画面と向きあっていると気分も暗くなってくる。せめて初期画面くらいは、ホットするようなものにしたい…ということで、dashbord.elを導入してみた。

<!--more-->
![Alt Text](https://c2.staticflickr.com/2/1869/43955677834_da8873c195_b.jpg) 

## dashbord.elを入れる

```emacs-lisp
M-x package install dashbord.el
```

<!-- ![Alt Text](https://c1.staticflickr.com/5/4468/37784565211_10ca0426a6_c.jpg) -->

設定は以下の通り

```emacs-lisp
;; Set the title
(when (eq system-type 'darwin)
  (setq dashboard-banner-logo-title
        (concat "GNU Emacs " emacs-version " kernel "
                (car (split-string (shell-command-to-string "uname -r")))  " x86_64 Mac OS X "
                (car(split-string (shell-command-to-string "sw_vers -productVersion") "-")))))
(when (eq system-type 'gnu/linux)
  (setq dashboard-banner-logo-title
        (concat "GNU Emacs " emacs-version " kernel "
                (car (split-string (shell-command-to-string "uname -r")))  " x86_64 Debian GNU/Linux "
                (car (split-string (shell-command-to-string "cat /etc/debian_version") "_")))))

;; Set the banner
(setq dashboard-startup-banner "~/Dropbox/emacs.d/emacs.png")
(dashboard-setup-startup-hook)
(setq dashboard-page-separator "\n\f\f\n")
(setq dashboard-items '((recents  . 15)))
```

Recent listは、Tab(Shift+Tab)キーを押すことで選択できる。

