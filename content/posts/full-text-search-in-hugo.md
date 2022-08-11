+++
date = "2018-01-15T20:42:43+09:00"
categories = ["hugo"]
tags = ["lunr","search"]
title= "JavaScriptを使ってHugoサイト内に全文検索を取り付けてみた"
+++

<h3 style="clor:red; margin-bottom:3em;">
PS:仕様を変えましたので、現在この仕組みはこのブログでは動作していません。2020.10.24</h3>


素敵なTipsを見つけたので、静的サイトジェネレータHugoを使って生成したコンテンツに全文検索を取り付けてみました。
クライアント側のJavaScriptを使って日本語のキーワードも検索可能です。
まずは、完成したページをご覧ください。→ [サイト内全文検索](/search/) 

<!--more-->
導入については、以下のページに詳しく解説されているので、その通りに進めれば大丈夫ですが、わたしが実際におこなった手順をご紹介しておきます。

[Hugo に全文検索を取り付けた](http://rs.luminousspice.com/hugo-site-search/#i-1) 


## インデックスファイルの生成 

全文検索用のインデックスファイルindex.js を書き出すためのテンプレートファイルを作成し、ダミーの投稿を使って書き出させるという仕組みです。

### インデックスファイルのテンプレート

下記のファイルを作成し、 ```layouts/js/single.html``` に配置します。

##### single.html

```html
 var data = [{{ range $index, $page := where .Site.Pages "Section" "post"}}
 {{ if ne $index 0 }},{{ end }}{
 url: "{{ $page.Permalink }}",
 title: "{{ $page.Title }}",
 content: "{{ .PlainWords }}"
 }{{ end }}]
```

### インデックスファイルを生成する空の投稿

次に、空の投稿ファイル（indexjs.md）を作成します。

postフォルダではなくて固定ページのファイルを置いているpagesフォルダがいいと思います。

##### indexjs.md
```md
+++
 date =  "2016-03-21T14:35:52+09:00"
 type =  "js"
 url = "index.js"
+++
```
ここまで準備できたらpublishして、```index.js```にアクセスしてみます。
わたしの場合は、https://snap.textgh.org/index.js になります。


## 検索ページの作成
次に、作成したインデックスファイル index.js を検索するユーザーインタフェイスを作ります。  

### 検索ページテンプレートの作成
検索ページ用のファイル（single.html）を作り、```layouts/search/single.html```に配置します。

##### single.html
```html
{{ partial "head.html" . }}
<div class="container">
<h1>サイト内全文検索</h1>

<ul>
<li>キーワードを入力するとリアルタイムで検索を始めます。
<li>入力欄フォーカス中は、エンターキー、矢印キーでベージ送りします。
</ul>

<script src="https://snap.textgh.org/index.js" charset="utf-8"></script>
<style>
dd{
	margin:0;
	padding:0 0 1em 0.5em;
	width:90%;
}
dd span{
	font-size:80%;
	color:#888;
}
dd b{
	color:#666600;
	background-color:#ffffdd;
	font-weight:bold;
	border:1px solid #bbbb00;
	margin:0 2px 0 2px;
	padding:0 2px 0 2px;
}
#navi{
	margin:0.5rem 0;
	line-height:2rem;
}
#navi span{
	border-top:1px solid #d8d8d8;
	border-bottom:1px solid #d8d8d8;
	padding: 0.33rem 0.66rem;
	cursor:pointer;
	word-wrap:break-word;
}
#navi span.selected{
	background: #D3EDF7;
}
#navi span:first-child{
	border-left:1px solid #d8d8d8;
	border-top-left-radius: 0.4rem;
	border-bottom-left-radius: 0.4rem;
}
#navi span:last-child{
	border-right:1px solid #d8d8d8;
	border-top-right-radius: 0.4rem;
	border-bottom-right-radius: 0.4rem;
}

#searchbox input{
	font-size: 1;
    padding: .3em;
    margin-left:2em;
	margin-bottom: 1em;
}
@media (max-width: 15em) {
	#navi{
		width:300px;
	}
}
</style>

<div id="searchbox">
<input type="text" id="q" onkeyup="do_find(this.value)" onkeydown="key(event.keyCode)" autocomplete="off" placeholder="サイト内を検索"> <span class="fa fa-search" aria-hidden="true"></span><span id="stat"></span>
<div id="navi"></div>
<div id="result"></div>
</div>
<script>
window.onload=function(){
	gid("q").focus();
}

{
	$_ = String.prototype;
	
	$_.mReplace = function(pat,flag){
		var temp = this;
		if(!flag){flag=""}
		for(var i in pat){
			var re = new RegExp(i,flag);
			temp = temp.replace(re,pat[i])
		}
		return temp;
	};
}

{
	$_ = Date.prototype;
	
	$_.format = "yyyy-mm-dd HH:MM:SS";
	$_.formatTime = function(format){
		var yy;
		var o = {
			yyyy : ((yy = this.getYear()) < 2000)? yy+1900 : yy,
			mm   : this.getMonth() + 1,
			dd   : this.getDate(),
			HH   : this.getHours(),
			MM   : this.getMinutes(),
			SS   : this.getSeconds()
		}
		for(var i in o){
			if (o[i] < 10) o[i] = "0" + o[i];
		}
		return (format) ? format.mReplace(o) : this.format.mReplace(o);
	}
}
</script>
<script>
var start = new Date().getTime();
var bodylist = [];
var st = gid("stat");
var re = gid("result");
var nv = gid("navi");
var max = 5;
var KC = {
	enter: 13,
	left : 37,
	right: 39,
	up   : 38,
	down : 40
};
function gid(id){
	return document.getElementById(id);
}
function ignore_case(){
	var a = arguments;
	return "[" + a[0] + a[0].toUpperCase() + "]"
}
function do_find(v){
	if(this.lastquery == v){return}
	this.lastquery = v;
	var re = find(v);
	if(re.length){
		pagenavi(re);
		view(re)
	}
}
function key(c){
	switch(c){
		case KC.enter: mv(1);break;
		case KC.left : mv(-1);break;
		case KC.right: mv(1);break;
		case KC.up   : mv(-1);break;
		case KC.down : mv(1);break;
	}
}
function find(v){
	var query = v;
	if(!v){return []}
	var aimai;
	if(query){


		aimai = query.replace(/[a-z]/g,ignore_case);
		try{
			reg = new RegExp(aimai,"g");
		}catch(e){
			reg = /(.)/g;
		}
	}else{
		reg = /(.)/g;
	}
	var start = new Date().getTime();
	var result = [];
	for(var i=0;i<data.length;i++){
		
		var s = bodylist[i];
		var res = reg.exec(s);
		if(!res){continue}
		var len = res[0].length;
		var idx = res.index;
		if(idx != -1){
			result.push([i,idx,len]);
		}
	}
	if(result.length){
		st.innerHTML = result.length +"件見つかりました。";
	}
	var end = new Date().getTime();

	console.log("Find:"+ (end-start) + " ms");
	return result;
}
function time2date(time){
	if(!this.cache){this.cache = {}};
	if(this.cache[time]) return this.cache[time];
	var d = new Date(time*1000);
	this.cache[time] = d.formatTime("yyyy年mm月dd日");
	return this.cache[time];
}
function snippet(body,idx,len){
	var start = idx - 20;
	return [
		body.substring(start,idx),
		,"<b>"
		,body.substr(idx,len)
		,"</b>"
		,body.substr(idx+len,50),
	].join("");
}
function pagenavi(result){
	var len = result.length;
	var ct = Math.ceil(len/max);
	var buf = [];
	for(var i=0;i<ct;i++){
		buf.push(
			"<span onclick='view(\"\","
			,i+1
			,");sw(",i,")'>"
			,i+1
			,"</span>"
		);
	}
	nv.innerHTML = buf.join("");
	sw(0);
}
function sw(t){
	var span = nv.getElementsByTagName("span");
	for(var i=0;i<span.length;i++){
		span[i].className = (i==t)?"selected":"";
	}
}
function mv(to){
	var span = nv.getElementsByTagName("span");
	var current;
	if(!span.length){return}
	for(var i=0;i<span.length;i++){
		if(span[i].className == "selected"){
			current = i;break;
		}
	}
	var moveto = current+to;
	if(moveto < 0){return}
	if(moveto > span.length-1){moveto=0}
	sw(moveto);
	view("",moveto+1)
}
function view(result,offset){
	if(!offset){offset = 1}
	if(!result){
		result = this.last.reverse();
	}else{
		this.last = result;
	}
	var r = result.reverse();
	var buf = ["<dl>"];
	var count = 0;
	for(var i=(offset-1)*max;i<r.length;i++){
		count++;
		if(count > max){break}
		var num = r[i][0];
		var idx = r[i][1];
		var len = r[i][2];
		with(data[num]){
			buf.push(
				"<dt><a href='",url,"'>"
				,title||"無題","</a>"
				,"<dd>"



				,snippet(bodylist[num],idx,len)
			);
		}
	}
	re.innerHTML = buf.join("");
}
for(var i=0;i<data.length;i++){
	bodylist.push(data[i].title+ " " +data[i].content);
}
var bodyidx = bodylist.join("<>");
var end = new Date().getTime();

console.log("Index:"+ (end-start) + " ms");
</script>

	<noscript><p class="notice">注意: この検索機能は JavaScript を使用しています。</p></noscript>

</div>
</div>

{{ partial "footer.html" . }}

```
### 検索ページの作成

検索ページを配置するための投稿（search.md）を作成します。

これも、固定ページフォルダpagesに置くのがいいでしょう。

##### search.md
```markdown
+++
 date = "2016-03-05T21:10:52+01:00"
 type =  "search"
 url =  "search"
 title =  "全文検索"
+++
```
ここまでできたら、もう一度publishして、検索ページにアクセスしてみてください。

このサイトの場合は、以下のページになります。

https://snap.textgh.org/search/

ここまで手間を掛けずとも、googleなどの外部検索サービスを使えば簡単にできることなのかもしれませんが、
自前でも簡単に導入できて、レスポンスもよいのでしばらく使ってみようと思います。


## 参考サイト
[Hugo に全文検索を取り付けた](http://rs.luminousspice.com/hugo-site-search/#i-1) 

