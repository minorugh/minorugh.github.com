+++
date = "2019-04-02T12:11:22+09:00"
categories = ["emacs"]
tags = ["dashboard"]
title = "DashboardにQiitaの新着リスト（tag:emacs）を表示させる"
+++
![Alt Text](https://live.staticflickr.com/7874/32576377777_6a8b349d0b_b.jpg)

下記のTipsを参考にしてEmacsのDashboard画面にQiitaの新着一覧を表示させる事ができました。

  * [Dashboardで起動画面を素敵にしよう](https://qiita.com/hyakt/items/f4468facec0478e06f7a)


## 設定 ##

この設定例では、emacsタグで絞り込んだ一覧を表示するようにカスタマイズしています。

``` emacs-lisp
;; Use request.el for API call
(use-package request)

;; Item display function
(defun dashboard-qiita-insert-list (list-display-name list)
  "Render LIST-DISPLAY-NAME and items of LIST."
  (dashboard-insert-heading list-display-name)
  (mapc (lambda (el)
          (insert "\n    ")
          (widget-create 'push-button
                         :action `(lambda (&rest ignore)
                                    (browse-url ,(cdr (assoc 'url el))))
                         :mouse-face 'highlight
                         :follow-link "\C-m"
                         :button-prefix ""
                         :button-suffix ""
                         :format "%[%t%]"
                         (decode-coding-string (cdr (assoc 'title el)) 'utf-8))) list))

;; Function to get and display articles
(defun dashboard-qiita-insert (list-size)
  "Add the list of LIST-SIZE items from qiita."
  (request
   ;; (format "https://qiita.com/api/v2/items?page=1&per_page=%s" list-size)
   (format "https://qiita.com/api/v2/tags/emacs/items?page=1&per_page=%s" list-size)
   :sync t
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (dashboard-qiita-insert-list "Qiita(emacs-tag):" data)))))

;; add an article to the dashboard
(add-to-list 'dashboard-item-generators '(qiita . dashboard-qiita-insert))
(setq dashboard-items '((qiita . 15)))

```

## 参考 ##

Emacs:Dashboardの詳細設定はGithubに公開しています。

* [github/minoruGH](https://github.com/minorugh/emacs.d/blob/master/inits/01_dashboard.el)
