+++
date = "2021-12-30T12:03:29+09:00"
categories = ["tech"]
tags = ["jQuery","javascript","iframe"]
title = "iframeの高さを取得し自動調整する"

+++

レスポンシブ Webデザインでは、iframeの高さが PCとスマホ・タブレットで結構変わってしまうことも多くて事前に高さを決め打ちで指定できません。

そこで、JavaScript（jQuery）を使って iframeの高さを自動的に設定してくれるスクリプトを探しました。

親ページと子ページのそれぞれで設定の必要な Tipsが多い中、
親ページの設定のみで動作するものを見つけて試したところ実に快適だったのでこのスクリプトを採用することにした。

## HTMLで準備すること

高さを自動調整したい iframeの classに「autoHeight」を付けるだけです。

```html
<iframe src="http://www.example.com/" class="autoHeight"></iframe>
```

## iframeの高さを自動的に調整してくれるスクリプト

jQueryを読み込んだ状態で以下のスクリプトを実行します。

```javascript
(function(window, $){
	$(window).on("load",function(){
		$('iframe.autoHeight').each(function(){
			var D = $(this).get(0).contentWindow.document;
			var innerHeight = Math.max(
				D.body.scrollHeight, D.documentElement.scrollHeight,
				D.body.offsetHeight, D.documentElement.offsetHeight,
				D.body.clientHeight, D.documentElement.clientHeight
				);
			$(this).removeAttr("height").css('height', innerHeight + 'px');
		});
	});
})(window, jQuery);
```
ページに配置された画像などの読み込みが完了した時点で iframeの高さを調整します。

同じページ内に複数の iframeがあっても、正常に高さを設定できます。

`class="autoHeight"` と書くだけで `scrolling="no" frameborder="no"` とかを書く必要なくとてもシンプルな優れものでした。

## 参考サイト

* [【 jQuery】 iframeの高さを取得し自動調整するスクリプト（Microsoft Edgeにも対応）](https://jdash.info/archives/jQuery_iframe_auto_height_script) 
