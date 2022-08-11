+++
date = "2017-05-17T21:15:12+09:00"
categories = ["emacs"]
tags = ["mozc","mac"]
title = "mozc_emacs_helper on macOS Sierra"
+++
macOS Sierra をクリーンインストールしたので mozc_emacs_helper を使えるようにした。google日本語入力と連携できるようにビルドできるという以下のサイトの Tips を参考にしたが、ちょっと解りにくかったので備忘録として整理しておく。

* [emacs with mozc on macos sierra](https://gist.github.com/ynkjm/7d6f22bb4338f84b1b287bf9abe79001) 

## 準備

Xcodeが必要と言うことだけどコマンドラインツールのみで大丈夫だった。

````bash
xcode-select --install
````

ninja が要るらしいので homebrew でインストールしておく。

````bash
brew install ninja
````




## mozc をダウンロードする
作業ディレクトリはどこでもいいが、僕は基本的にdesktopで作業することが多い。


````bash
cd desktop
git clone https://github.com/google/mozc.git -b master --single-branch --recursive
````

## Mac の google日本語入力と連携させるためにソースを修正

以下のファイルを作ってパッチをかけてもいいが、簡単なのでぼくは直接修正した。

````vc
diff --git a/src/build_mozc.py b/src/build_mozc.py
index a56aaaf..d419f49 100644
--- a/src/build_mozc.py
+++ b/src/build_mozc.py
@@ -167,6 +167,8 @@ def GetGypFileNames(options):
   # Include subdirectory of win32 and breakpad for Windows
   if options.target_platform == 'Windows':
     gyp_file_names.extend(glob.glob('%s/win32/*/*.gyp' % SRC_DIR))
+  elif options.target_platform == 'Mac':
+    gyp_file_names.extend(glob.glob('%s/unix/emacs/*.gyp' % SRC_DIR))
   elif options.target_platform == 'Linux':
     gyp_file_names.extend(glob.glob('%s/unix/*/*.gyp' % SRC_DIR))
     # Add ibus.gyp if ibus version is >=1.4.1.
diff --git a/src/mac/mac.gyp b/src/mac/mac.gyp
index 76b540d..2ee4006 100644
--- a/src/mac/mac.gyp
+++ b/src/mac/mac.gyp
@@ -586,7 +586,6 @@
             ['branding=="GoogleJapaneseInput"', {
               'dependencies': [
                 'DevConfirmPane',
-                'codesign_client',
               ],
             }],
           ],
           
````
## コンパイル

````bash
cd mozc/src
GYP_DEFINES="mac_sdk=10.12 mac_deployment_target=10.12" python build_mozc.py gyp --noqt --branding=GoogleJapaneseInput
$ python build_mozc.py build -c Release unix/emacs/emacs.gyp:mozc_emacs_helper
````

## 動作確認

以下のように表示されれば成功！

````bash
echo -e '(0 CreateSession)\n(1 SendKey 1 hiragana)\n(2 SendKey 1 hiragana)\n(3 SendKey 1 97)' | out_mac/Release/mozc_emacs_helper
((mozc-emacs-helper . t)(version . "2.19.2643.101")(config . ((preedit-method . roman))))
((emacs-event-id . 0)(emacs-session-id . 1)(output . ()))
((emacs-event-id . 1)(emacs-session-id . 1)(output . ((id . "4087100232139049092")(mode . hiragana)(consumed . nil)(key . ((special-key . kana)))(status . ((activated . t)(mode . hiragana)(comeback-mode . hiragana))))))
((emacs-event-id . 2)(emacs-session-id . 1)(output . ((id . "4087100232139049092")(mode . hiragana)(consumed . nil)(key . ((special-key . kana)))(status . ((activated . t)(mode . hiragana)(comeback-mode . hiragana))))))
````

## インストールする
パスの通ったところにコピーするだけ。

````bash
sudo cp out_mac/Release/mozc_emacs_helper /usr/local/bin
````

## Emacs で使えるようにする
mozc.el をコピーするか Package install する。

````basu
cp mozc/src/unix/emacs/mozc.el "YOUR EMACS ELISP PATH"
````

## init.elに設定を書く
* mozc-popup.el をパッケージインストールして変換候補を表示させる。
* mozc の on/off でカーソルの色を変える（mozc-el-extensions）

```lisp
(require 'mozc)
(setq default-input-method "japanese-mozc")
(setq mozc-helper-program-name "mozc_emacs_helper")

(require 'mozc-popup)
(setq mozc-candidate-style 'popup) ; select popup style.

;; Changing the color of the cursor with on/off
(require 'mozc-cursor-color)

```

```lisp
M-x mozc-mode
```

で使える。toggle-input-method をキーバインドしてもよい。


## 使用感
google日本語の辞書、ユーザ辞書ともに完全に連携できるのでとても便利。


