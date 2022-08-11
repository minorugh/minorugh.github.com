+++
date = "2019-07-11T16:32:15+09:00"
categories = ["emacs"]
tags = ["swiper"]
title = "swiper-region / swiper-isearch-region"
+++

swiper/swiper-isearchをregion選択からも使えるようにするための設定。isearchではmigemoも使えるようにした。

`(with-eval-after-load 'ivy'` としているのは、`(use-package avy-migemo-e.g.swiper)`を遅延ロードさせてEmacsの起動時間を短縮するため。


## 設定
```emacs-lisp
(defun swiper-isearch-region ()
  "If region is selected `swiper-isearch' with the keyword selected in region.
If the region isn't selected `swiper-isearch'."
  (interactive)
  (if (not (use-region-p))
      (swiper-isearch-migemo)
    (deactivate-mark)
    (swiper-isearch (buffer-substring-no-properties
		     (region-beginning) (region-end)))))

(defun swiper-region ()
  "If region is selected `swiper' with the keyword selected in region.
If the region isn't selected `swiper'."
  (interactive)
  (if (not (use-region-p))
      (swiper)
    (deactivate-mark)
    (swiper (buffer-substring-no-properties
	     (region-beginning) (region-end)))))

(with-eval-after-load 'ivy
  (use-package avy-migemo-e.g.swiper)
  (defun swiper-isearch-migemo ()
    "Using migemo with `swiper-iserach'."
    (interactive)
    (avy-migemo-mode 1)
    (swiper-isearch)
    (avy-migemo-mode 0)))

```
## avy-migemo でエラー発生

avy、swiper、counsel の最近の仕様変更により関数名などが変わったため現状ではエラーが出るようになった。その対応のために下記のPRが出されているがまだマージされていないようなので、自分で差し替えて使っている。

- https://github.com/momomo5717/avy-migemo/pull/8
- https://github.com/tam17aki/avy-migemo 

