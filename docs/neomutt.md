---
layout: default
title: Neomutt Configuration
---

# Neomutt Configuration
![emacs](https://minorugh.github.io/img/neomutt.png)


```codesession
~/
│
├── .abook/
│     └─ addressbook
│
├── .mutt/
│     ├─ dracula.muttrc
│     ├─ certificates
│     ├─ mailcap
│     ├─ password.rc
│     └─ signature.rc
│
└── .muttrc

```
## .muttrc
``` dotenv
############################################################
# Neomutt Configuration  2024.10.10
# ~/.muttrc
# @author Minoru Yamada <minorugh@gmail.com>

# Sources for Auth Configuration ---------------------------
source ~/.mutt/password.rc

# For japanese Environment
# see "https://emaillab.org/mutt/1.4/doc/usage-japanese.ja.html#id2432623"
set send_charset="iso-2022-jp:utf-8"
set thorough_search=yes    ## enable body search in Japanese
set rfc2047_parameters=yes ## filename for japanese

# Basic Configuration --------------------------------------
set folder = "imaps://imap.gmail.com:993"
set spoolfile = "+INBOX"
set imap_check_subscribed
set hostname = gmail.com
set mail_check = 60
set timeout = 300
set imap_keepalive = 300
set postponed = "+[GMail]/Drafts"
set record = "+[GMail]/Sent Mail"
set header_cache=~/.mutt/cache/headers
set message_cachedir=~/.mutt/cache/bodies
set certificate_file=~/.mutt/certificates
set signature =~/.mutt/signature
set move = no
set include
set auto_tag = yes
ignore "Authentication-Results:"
ignore "DomainKey-Signature:"
ignore "DKIM-Signature:"
hdr_order Date From To Cc
alternative_order text/plain text/html *
auto_view text/html

# mailcap
set mailcap_path = ~/.mutt/mailcap

# Print Command
set print_command="nkf -em | e2ps -size 11 -head muttmail | lpr"

# notify-send ----------------------------------------------
set new_mail_command="notify-send --icon='/usr/share/pixmaps/neomutt.xpm' 'New Email' '%n new messages, %u unread.' &"

# Open URL using w3m ---------------------------------------
macro index,pager \cb ": unset wait_key; set pipe_decode\n|w3m\n: \
set wait_key; unset pipe_decode\n" "call w3m to extract URLs out of a message"

# Change the following line to a different editor you prefer.
set editor="emacsclient"
#set editor="vim"

# addressbook use Abook
set query_command= "abook --mutt-query '%s'"
macro index,pager  a "<pipe-message>abook --add-email-quiet<return>" "Add this sender to Abook"
bind editor        <Tab> complete-query

# Sidebar Patch --------------------------------------------
set sidebar_format   = "%B%?F? [%F]?%* %?N?%N/?%S"
#set sidebar_visible = yes
set sidebar_width    = 28

# Status Bar -----------------------------------------------
set status_chars  = " *%A"
set status_format = "---[ Folder: %f ]---[%r%m messages%?n? (%n new)?%?d? (%d to delete)?%?t? (%t tagged)? ]---%>-%?p?( %p postponed )?---"

# Index View Options ---------------------------------------
set index_format="%4C %Z %<[y?%<[d?%[%H:%M           ]&%[%m/%d (%a) %H:%M]>&%[%Y/%m/%d %H:%M]> %-15.15L (%?l?%4l&%4c?) %s"
set sort = threads                         # like gmail
set sort_aux = reverse-last-date-received  # like gmail
set uncollapse_jump                        # don't collapse on an unread message
set sort_re                                # thread based on regex
set reply_regexp = "^(([Rr][Ee]?(\[[0-9]+\])?: *)?(\[[^]]+\] *)?)*"

# Pager View Options ---------------------------------------
set pager_index_lines =  8 # number of index lines to show
set pager_context = 3      # number of context lines to show
set pager_stop             # don't go to next message automatically
set menu_scroll            # scroll in menus
set tilde                  # show tildes like in vim
unset markers              # no ugly plus signs
set quote_regexp = "^( {0,4}[>|:#%]| {0,4}[a-z0-9]+[>|]+)+"
alternative_order text/plain text/enriched text/html

# Saner copy/move dialogs
macro index C "<copy-message>?<toggle-mailboxes>" "copy a message to a mailbox"
macro index M "<save-message>?<toggle-mailboxes>" "move a message to a mailbox"

# Sidebar Navigation ---------------------------------------
bind index,pager ]   sidebar-next
bind index,pager [   sidebar-prev
bind index,pager ^   sidebar-open
bind index,pager \cv sidebar-toggle-visible

# Index Key Bindings ---------------------------------------
bind index,attach g first-entry       # Go to first entry
bind index,attach G last-entry        # Go to last entry
bind index,pager  f next-page         # Go to next page
bind index,pager  b previous-page     # Go to previous page

# Pager Key Bindings ---------------------------------------
bind pager g top                      # Go to the top of the email
bind pager G bottom                   # Go to the bottom of the email
bind pager j next-line                # Scroll down one line
bind pager k previous-line            # Scroll up one line
bind pager h exit                     # Exit Menu
bind pager l display-toggle-weed      # header suppression switching
bind index,pager F forward-message    # Forwarding Mail

# Color settings for mutt --------------------------------
#source ~/.mutt/dracula.muttrc
source ~/.mutt/nord.muttrc


# .muttrc ends here
############################################################
```


導入の記録と設定をここにまとめていく。

## Makefile
Debian12で使った Makefailです。

``` makefile
neomutt: ## Init neomutt mail client with abook
	sudo apt install neomutt abook w3m
	mkdir -p ${HOME}/.mutt
	ln -vsf {${PWD},${HOME}}/.muttrc
	ln -vsf {${PWD},${HOME}}/.w3m/keymap
	for item in mailcap certifcates dracula.muttrc nord.muttrc; do
		ln -vsf {${PWD},${HOME}}/.mutt/$$item; done
	for item in password.rc signature; do ln -vsf {${HOME}/Dropbox/backup/mutt,${HOME}/.mutt}/$$item; done
	test -L ${HOME}/.abook || rm -rf ${HOME}/.abook
	ln -vsfn {${HOME}/Dropbox/backup/mutt,${HOME}}/.abook

mutt.sh: ## Change startup directory so that attachments are stored in ~/Downloads.
	sudo ln -vsfn ${PWD}/bin/neomutt.sh /usr/local/bin
	sudo chmod +x /usr/local/bin/neomutt.sh
	ln -vsfn {${PWD},${HOME}}/.local/share/applications/neomutt.desktop

dracula-theme: ## Install dracula theme for gnome-terminal
	cd ${HOME}/Downloads
	git clone https://github.com/dracula/gnome-terminal
	cd gnome-terminal && ./install.sh
	rm -fr ${HOME}/Downloads/gnome-terminal

nord-theme: ## Install dracula theme for gnome-terminal
	sudo apt install dconf-cli uuid-runtime
	cd ${HOME}/Downloads
	git clone https://github.com/nordtheme/gnome-terminal.git
	cd gnome-terminal/src && ./nord.sh
	rm -fr ${HOME}/Downloads/gnome-terminal

```

### 認証情報ファイルの作成
多くの導入Tipsでは認証情報を設定したファイルをgpg化することを勧めていますが面倒なので私の場合は `~/Dropbox/backup/mutt` に保存した生ファイルからシンボリックリンクして使っています。Dropboxの管理者からは見えてしまうことになりますが割り切ることにしました。

内容は、`.mutt`内の `password-sample.rc` `signature-sample` を参照して作成します。

### neomuttのインストールと設定 
認証ファイルの作成、保存が終わったらあとはターミナルから`make neomutt`するだけで neomuttのinstallが完了します。

### gnome-terminal のthemeを変更する
* nemuttのcolor設定は、dracura.muttrc nead.rc の二種類からどちらかを選べるようにしています。
* その設定は、`~/neomuttrc` の最終行にあります。
* その場合、gnome-terminal のcolor-theme もそれぞれ選んだ方に合わせる必要があります。

### MuttのURLをw3mを使って開く
Webでヒットするmutt導入Tipsはどれも古く、特に urlviewを使った仕組みはあまり快適ではありません。
メール本文のURLにはリンクははられているのでマウスで`右クリック→リンクを開く` を選べばプラウザアプリが起動します。

ただ、キーボードだけで操作できる/する…というのがMuttの基本コンセプトなので調べてたら素敵なTipsが見つかりました。
* [MuttのURLをw3mを使って開く](https://hanagurotanuki.blogspot.com/2015/04/mutturlw3m.html?utm_source=pocket_shared#google_vignette) 

``` dotenv
macro index,pager \cb ": unset wait_key; set pipe_decode\n|w3m\n: \
set wait_key; unset pipe_decode\n" "call w3m to extract URLs out of a message"
```
Tips内の説明では、`C-b`でw3m画面にしたあと[:]を叩き、URLを選んで[Enter]でW3mで見るという仕組みですがこれも極めて使い勝手が悪いです。

w3m画面からは Alt-Shift-m でw3mに設定した外部プラウザが開く…という機能があるのでこれを活用します。
手順は以下のとおりです。

1. `C-b`で開いたw3m上で[o]を押すとw3mの設定画面がでます。
2. 雑多な設定グループに「全てのページのURL風の文字列をリンクする」をyesにします。
3. 外部プログラムの「外部プラウザ2」に google-chrome（またはfirefox）を書き込みます。
4. 全て修正したら [OK]ボタンを押して設定画面を閉じます。

以上の設定が済んだらneomuttを再起動して動作を確認してみましょう。
メール表示から`C-b`でw3m画面に変わるので開きたいURLを選んで `Alt+Shift-m` で外部プラウザが立ち上がります。



