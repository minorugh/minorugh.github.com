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
set new_mail_command="notify-send --icon='/usr/share/pixmaps/neomutt.xpm'\
'New Email' '%n new messages, %u unread.' &"

# Open URL using w3m ---------------------------------------
macro index,pager \cb ": unset wait_key; set pipe_decode\n|w3m\n: \
set wait_key; unset pipe_decode\n" "call w3m to extract URLs out of a message"

# Tips from "https://hanagurotanuki.blogspot.com/2015/04/mutturlw3m.html?utm_source=pocket_shared#google_vignette"
# -----------------------------------------------------------
# After typing Ctrl-B to launch w3m, type : (a colon) to tell w3m to convert URL strings to links.
# w3m's Option Setting Panel (type o) and set the "Treat URL-like strings as links in all pages" to YES.
# Then you won't have to keep typing : as you page through the message.
# ----------------------------------------------------------
# Open w3m url in an external plaza--------------------------
# Open w3m, type o to open the Option Setting Panel,
# scroll down to "External Program Settings" and set the "External Browser" or "Second External 
# Browser" to "firefox" or "google-chrome". Select "OK" to save the setting.
# To use the External Browser, type M to view the current page or <Esc>M to follow a link; 

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
set date_format = "%m/%d"
set index_format = "[%Z]  %D  %-20.20F  %s"
set sort = threads                         # like gmail
set sort_aux = reverse-last-date-received  # like gmail
set uncollapse_jump                        # don't collapse on an unread message
set sort_re                                # thread based on regex
set reply_regexp = "^(([Rr][Ee]?(\[[0-9]+\])?: *)?(\[[^]]+\] *)?)*"

# Pager View Options ---------------------------------------
set pager_index_lines = 10 # number of index lines to show
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
bind index,pager f next-page          # Go to next page
bind index,pager b previous-page      # Go to previous page

# Pager Key Bindings ---------------------------------------
bind pager g top                      # Go to the top of the email
bind pager G bottom                   # Go to the bottom of the email
bind pager j next-line                # Scroll down one line
bind pager k previous-line            # Scroll up one line
bind pager h exit                     # Exit Menu
bind pager l display-toggle-weed      # header suppression switching
bind index,pager F forward-message    # Forwarding Mail

# # Color settings for mutt --------------------------------
source ~/.mutt/color/dracula.rc

# .muttrc ends here
############################################################

```


導入の記録と設定をここにまとめていく。

