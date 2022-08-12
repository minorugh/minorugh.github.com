+++
date = "2019-02-21T10:59:02+09:00"
categories = ["tech"]
tags = ["mac","css"]
title = "iPadでBootstrapのnavbarをcollapseさせる"
+++

CSSフレームワークのBootstrapを使って、ナビゲーション（navbar）を導入する場合、Bootstrapのデフォルトだと、iPadでnavbarがcollapseしてくれません。メニュー項目が多くなってくると、collapseしてくれないとレイアウト崩れが起きるので対策しました。

iPadでBootstrapのnavbarをcollapseさせるには、CSSに下記のコードを追加すればOKです。

```css
/* navbar collapse for iPad (bootstrap) */
@media (max-width: 991px) {
    .navbar-header {
        float: none;
    }
    .navbar-toggle {
        display: block;
    }
    .navbar-collapse {
        border-top: 1px solid transparent;
        box-shadow: inset 0 1px 0 rgba(255,255,255,0.1);
    }
    .navbar-collapse.collapse {
        display: none!important;
    }
    .navbar-nav {
        float: none!important;
        margin: 7.5px -15px;
    }
    .navbar-nav>li {
        float: none;
    }
    .navbar-nav>li>a {
        padding-top: 10px;
        padding-bottom: 10px;
    }
    .navbar-text {
        float: none;
        margin: 15px 0;
    }
    .navbar-collapse.collapse.in {
        display: block!important;
    }
    .collapsing {
        overflow: hidden!important;
    }
}

```

## 参考サイト

- https://qiita.com/yn01/items/2ff25dfeb0bc51257f87
