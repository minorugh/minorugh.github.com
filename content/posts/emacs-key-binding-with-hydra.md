+++
date = "2019-03-25T16:37:37+09:00"
categories = ["emacs"]
tags = ["hydra"]
title = "HydraでEmacsのキーバインド問題を解消する"
+++

できるだけデフォルトを上書きしないようにと配慮しながらEmacsのキーバインドを考えるとなるとなかなか難しいですね。うまく割り当てできたとしてもとても覚えて使いこなすのは至難です。その問題を解消して視覚的で使いやすいUIを提供してくれるのが `Hydra` です。

`Hydra` はとても奥が深くまだまだ使いこなせないのですが、勉強を兼ねていろいろ設定しましたので備忘録を兼ねてご紹介します。こうすればなお良い…というようなアドバイスを頂けると嬉しいです。


設定の大部分は、hydraの作者である[abo-abo氏](https://github.com/abo-abo/hydra)のTipsを参考にしました。

  * [Hydra Wiki](https://github.com/abo-abo/hydra/wiki)

## Hydra-melpa ##

![Alt Text](https://farm8.staticflickr.com/7872/46740221354_14cd554345_b.jpg) 


emacsのパッケージ管理はいろいろあるようですが、私はMelpaを利用して、必要なパッケージは、[init.el](https://github.com/minorugh/emacs.d/blob/master/init.el "init.el") に書いて自動インストールされるようにしているのですが、メンテナンス（package-install、update、remove） のためのコマンドをhydraで設定しています。

`M-x package-list-packege` でリストを表示させてもいいのですが "U" で `update` すると全てが対象になって思わぬトラブルに遭遇することもあるので、`package-utils` というパッケージを使っています。

``` emacs-lisp
;; use package-utils.el
(defhydra hydra-melpa (:color red :hint nil)
   "
Package: _u_pdate  _r_emove  _i_nstall"
   ("u" package-utils-upgrade-by-name)
   ("r" package-utils-remove-by-name)
   ("i" package-install))

```

## Hydra-git-gutter ##

![Alt Text](https://farm8.staticflickr.com/7910/32521618977_d29bd033c3_b.jpg) 

emacsの設定ファイル群は、Gitで管理していますので、`magit` は必要不可欠です。

編集中は、git-gutterが便利なのでまとめてhydraに設定しています。

``` emacs-lisp
;; 02_git.el --- 02_git.el
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;; magit
(autoload 'magit-status "magit" nil t)
(defadvice magit-status (around magit-fullscreen activate)
  "Magit-status always in fullr screen."
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))
(bind-key [f8] 'magit-status)

;; git-gutter
(global-git-gutter-mode t)
(defun git-gutter:toggle-popup-hunk ()
  "Toggle git-gutter hunk window."
  (interactive)
  (if (window-live-p (git-gutter:popup-buffer-window))
      (delete-window (git-gutter:popup-buffer-window))
    (git-gutter:popup-hunk)))

;; Hydra
(bind-key
 "s-g"
 (defhydra hydra-git-gutter (:color red :hint nil)
   "
_m_agit  _b_lame  _d_ispatch  _t_imemachine  |  hunk: _p_revious  _n_ext  _s_tage  _r_evert  pop_u_p  _SPC_:toggle"
   ("m" magit-status :exit t)
   ("b" magit-blame :exit t)
   ("t" git-timemachine :exit t)
   ("d" magit-dispatch :exit t)
   ("p" git-gutter:previous-hunk)
   ("n" git-gutter:next-hunk)
   ("s" git-gutter:stage-hunk)
   ("r" git-gutter:revert-hunk)
   ("u" git-gutter:popup-hunk)
   ("SPC" git-gutter:toggle-popup-hunk)))

;; Local Variables:
;; byte-compile-warnings: (not free-vars callargs)
;; End:
;;; 02_git.el ends here
```

## Hydra-counsel ##

![Alt Text](https://farm8.staticflickr.com/7879/40497283063_7891bbb92f_b.jpg) 


私のemacsの補完インターフェイスは、脱Helmにこだわっています。

  * [君は誰とEmacsる？ (補完インターフェイス紹介篇)](https://qiita.com/tadsan/items/33ebb8db2271897a462b)

counselはとても優秀で便利なのですがキーバインドが覚えられないのでhydraにしました。頻繁に使うオーソドックなものは別にキーバインドするので、hydraからは除外しています。

``` emacs-lisp
;;; 05_counsel.el --- 05_counsel.el
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-use-selectable-prompt t)
(setq enable-recursive-minibuffers t)
(bind-key "M-:" 'ivy-switch-buffer)
(bind-key "M-s" 'swiper-for-region-or-swiper)
(bind-key "C-s" 'swiper-migemo)
(bind-key "M-x" 'counsel-M-x)
(bind-key "C-x C-f" 'counsel-find-file)

;; hydra-counsel
(bind-key
 "C-/"
(defhydra hydra-counsel (:hint nil :exit t)
   "
Counsel: describ-_f_unction   _i_nfo-lookup-symbol    _d_iscbinds      _y_ank-pop    _l_ocate      _a_g
         describ-_v_ariable   find-_L_ibrary          _u_nicode-char   _m_ark-ring   _g_it-grep    _r_g"
("d" counsel-descbinds)
("y" counsel-yank-pop)
("m" counsel-mark-ring)
("f" counsel-describe-function)
("v" counsel-describe-variable)
("L" counsel-find-library)
("i" counsel-info-lookup-symbol)
("u" counsel-unicode-char)
("g" counsel-git-grep)
("i" counsel-git)
("a" counsel-ag)
("r" counsel-rg)
("l" counsel-locate)))

;; swiper-for-region-or-swiper
(defun swiper-for-region-or-swiper ()
  "If the region is active, swiper-for-region.
If the region is inactive, swiper."
  (interactive)
  (if (region-active-p)
      (swiper (buffer-substring
               (region-beginning) (region-end)))
    (swiper)))

;; swiper-migemo
(use-package avy-migemo-e.g.swiper)
(defun swiper-migemo ()
  "Using migemo with Swiper."
  (interactive)
  (avy-migemo-mode 1)
  (swiper)
  (avy-migemo-mode 0))


;; counsel-tramp
(setq tramp-default-method "ssh")

;; Local Variables:
;; byte-compile-warnings: (not free-vars callargs)
;; End:
;;; 05_counsel.el ends here
```

## Hydra-dired ##

![Alt Text](https://farm8.staticflickr.com/7823/46740221464_793fb5eb10_b.jpg) 


diredのキーバインドはよく使うのですぐに覚えますから、あえてhydraにこだわることもないでしょう。お気に入りのデレクトリを一発で開いたり、trampを起動させたり、macアプリのfinder.app（ファイラー）や iterm（ターミナルアプリ）を起動させたりというショートカット機能も加えて遊んでみました。

dired-modeから "." でtoggle表示させているところが味噌です。

``` emacs-lisp
;;; 30_dired.el --- 30_dired.el
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;; Dired files deleted by trash and no ask recursive
(setq delete-by-moving-to-trash t
      dired-recursive-copies 'always
      dired-recursive-deletes 'always)

(with-eval-after-load 'dired
  (bind-key "RET" 'dired-open-in-accordance-with-situation dired-mode-map)
  (bind-key "e" 'wdired-change-to-wdired-mode dired-mode-map)
  (bind-key "o" 'dired-view-file-other-window dired-mode-map)
  (bind-key "[" 'dired-hide-details-mode dired-mode-map)
  (bind-key "a" 'dired-list-all-mode dired-mode-map)
  (bind-key "q" 'dired-dwim-quit-window dired-mode-map)
  (bind-key "W" 'dired-get-fullpath-filename dired-mode-map)    ;; w:file name only
  (bind-key "z" 'dired-zip-files dired-mode-map)    ;; Create an archive containing the marked files
  (bind-key "<left>" 'dired-up-alternate-directory dired-mode-map)
  (bind-key "<right>" 'dired-open-in-accordance-with-situation dired-mode-map)
  (bind-key "C-x C-j" 'dired-jump))


;; direx
(setq direx:leaf-icon "  " direx:open-icon "▾ " direx:closed-icon "▸ ")
(push '(direx:direx-mode :position left :width 30 :dedicated t)
      popwin:special-display-config)

(bind-key [f11] 'direx:jump-to-project-directory)
(defun direx:jump-to-project-directory ()
  "If in project, launch direx-project otherwise start direx."
  (interactive)
  (let ((result (ignore-errors
                  (direx-project:jump-to-project-root-other-window)
                  t)))
    (unless result
      (direx:jump-to-directory-other-window))))


;; When dired opened in two windows, move or copy in the other dired
(setq dired-dwim-target t)


;; Recursively copy directory
(setq dired-recursive-copies 'always)


;; Dired with directory first
(use-package ls-lisp)
(setq ls-lisp-use-insert-directory-program nil ls-lisp-dirs-first t)


;; Add [Dir] to the directory buffer
(defun dired-my-append-buffer-name-hint ()
  "Append a auxiliary string to a name of dired buffer."
  (when (eq major-mode 'dired-mode)
    (let* ((dir (expand-file-name list-buffers-directory))
           ;; Add a drive letter for Windows
           (drive (if (and (eq 'system-type 'windows-nt)
                           (string-match "^\\([a-zA-Z]:\\)/" dir))
                      (match-string 1 dir) "")))
      (rename-buffer (concat (buffer-name) " [" drive "dir]") t))))
(add-hook 'dired-mode-hook 'dired-my-append-buffer-name-hint)


;; Toggle listing dot files in dired
;; https://github.com/10sr/emacs-lisp/blob/master/docs/elpa/dired-list-all-mode-20161115.118.el
(when (require 'dired-list-all-mode nil t)
   (setq dired-listing-switches "-lhFG"))


;; Quit-window according to screen division
(defun dired-dwim-quit-window ()
  "`quit-window 'according to screen division."
  (interactive)
  (quit-window (not (delq (selected-window) (get-buffer-window-list)))))


;; Get fullpath-filename with W (for file name only w)
(defun dired-get-fullpath-filename ()
  "Copy file name (full path) of cursor position."
  (interactive)
  (kill-new (dired-get-filename))
  (message (dired-get-filename)))


;; File are opened in separate buffer, directories are opened in same buffer
;; http://nishikawasasaki.hatenablog.com/entry/20120222/1329932699
(defun dired-open-in-accordance-with-situation ()
  "Files are opened in separate buffers, directories are opened in the same buffer."
  (interactive)
  (let ((file (dired-get-filename)))
    (if (file-directory-p file)
        (dired-find-alternate-file)
      (dired-find-file))))


;; View-file-other-window
;; http://y0m0r.hateblo.jp/entry/20120219/1329657774
(defun dired-view-file-other-window ()
  "View-file other window."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
	(or (and (cdr dired-subdir-alist)
		 (dired-goto-subdir file))
	    (dired file))
      (view-file-other-window file))))


;; Move to higher directory without make new buffer
(defun dired-up-alternate-directory ()
   "Move to higher directory without make new buffer."
   (interactive)
   (let* ((dir (dired-current-directory))
          (up (file-name-directory (directory-file-name dir))))
     (or (dired-goto-file (directory-file-name dir))
         ;; Only try dired-goto-subdir if buffer has more than one dir.
         (and (cdr dired-subdir-alist)
              (dired-goto-subdir up))
         (progn
           (find-alternate-file up)
           (dired-goto-file dir)))))


;; Automatic deletion for empty files (Valid in all modes)
;; https://uwabami.github.io/cc-env/Emacs.html#org57f6557
(defun my:delete-file-if-no-contents ()
  "Automatic deletion for empty files."
  (when (and (buffer-file-name (current-buffer))
             (= (point-min) (point-max)))
    (delete-file
     (buffer-file-name (current-buffer)))))
(if (not (memq 'my:delete-file-if-no-contents after-save-hook))
    (setq after-save-hook
          (cons 'my:delete-file-if-no-contents after-save-hook)))


;; Hide .DS_Store when dotfiles listed
(when (eq system-type 'darwin)
(use-package dired-x)
(setq dired-omit-mode t)
(setq-default dired-omit-files-p t)
(setq dired-omit-files "^\\.DS_Store\\|^\\.dropbox"))


;; zip file can be expanded with Z key
(eval-after-load "dired-aux"
   '(add-to-list 'dired-compress-file-suffixes
                 '("\\.zip\\'" ".zip" "unzip")))


;; Create an archive containing the marked files
;; https://stackoverflow.com/questions/1431351/how-do-i-uncompress-unzip-within-emacs
(defun dired-zip-files (zip-file)
  "Create an archive containing the marked files."
  (interactive "sEnter name of zip file: ")
  ;; create the zip file
  (let ((zip-file (if (string-match ".zip$" zip-file) zip-file (concat zip-file ".zip"))))
    (shell-command
     (concat "zip "
             zip-file
             " "
             (concat-string-list
              (mapcar
               '(lambda (filename)
                  (file-name-nondirectory filename))
               (dired-get-marked-files))))))
  (revert-buffer))
(defun concat-string-list (list)
   "Return a string which is a concatenation of all elements of the list separated by spaces"
   (mapconcat '(lambda (obj) (format "%s" obj)) list " "))


;; hydra-dired
(define-key dired-mode-map
  "."
  (defhydra hydra-dired (:hint nil :color pink)
    "
_+_ mkdir   _v_iew         _m_ark         _z_ip     _w_ get filename    _r_oot-dir   _T_erminal
_C_opy      view _o_ther   _U_nmark all   un_Z_ip   _W_ get fullpath    _h_ome-dir   _F_inder
_D_elete    open _f_ile    _u_nmark       _s_ort    _g_ revert buffer   _b_ook-dir   counsel-_T_ramp
_R_ename    ch_M_od        _t_oggle       _e_dit    _[_ hide detail     _d_ropbox   _._togggle hydra
"
  ("[" dired-hide-details-mode)
  ("+" dired-create-directory)
  ("RET" dired-open-in-accordance-with-situation :exit t)
  ("f" dired-open-in-accordance-with-situation :exit t)
  ("C" dired-do-copy)   ;; Copy all marked files
  ("D" dired-do-delete)
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("o" dired-view-file-other-window :exit t)
  ("?" dired-summary :exit t)
  ("R" dired-do-rename)
  ("a" dired-list-all-mode)
  ("g" revert-buffe)
  ("e" wdired-change-to-wdired-mode :exit t)
  ("s" dired-sort-toggle-or-edit)
  ("T" counsel-tramp :exit t)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file :exit t)
  ("w" dired-copy-filename-as-kill)
  ("W" dired-get-fullpath-filename)
  ("z" dired-zip-files)
  ("Z" dired-do-compress)
  ("b" my:book-dir)
  ("r" my:root-dir)
  ("h" my:home-dir)
  ("d" my:dropbox)
  ("F" my:finder-app)
  ("T" my:iterm-app)
  ("q" nil)
  ("." nil :color blue)))

(defun my:root-dir ()
  "Open root dir."
  (interactive)
  (find-file "/"))
(defun my:home-dir ()
  "Open home dir."
  (interactive)
  (find-file "~/"))
(defun my:dropbox ()
  "Open home dir."
  (interactive)
  (find-file "~/Dropbox"))
(defun my:book-dir ()
  "Open book dir."
  (interactive)
  (find-file "~/Dropbox/book/"))
(defun my:finder-app ()
  "Launch for finder.app."
  (interactive)
  (shell-command "open -a finder.app"))
(defun my:iterm-app ()
  "Launch for iterm.app."
  (interactive)
  (shell-command "open -a iterm.app"))

;; Local Variables:
;; byte-compile-warnings: (not free-vars callargs)
;; End:
;;; 30_dired.el ends here
```

## Hydra-markdowm ##

![Alt Text](https://farm8.staticflickr.com/7908/40497282853_49d2f6da08_b.jpg) 


私の場合、ブログのDraftやIdeaメモ類は全てhowmで管理し、markdown記法で書いています。（詳細は後述）

emacsのmarkdown-modeはとても便利なのですがinsertコマンドの全てのキーバインドを覚えるのは至難です。yasunippetを使うという選択肢もあると思うのですが、せっかくのmarkdowm-modeなのでhydraで便利に使えるようにしてみました。

ついでに、md2html,md2pdf,md2docx,previewなども設定しています。


``` emacs-lisp
;;; 40_markdown.el --- 40_markdown.el
;;; Commentary:
;;; Code:
;;(setq debug-on-error t)

(defhydra hydra-markdown (:hint nil :exit t)
   "
^Format^      ^Insert^        ^Head.Foot^     ^Code.Link^      ^Move^           ^Pndoc^       ^Draft
^^^^^^--------------------------------------------------------------------------------------------
_s_torong     _b_lockquote    H1~H6:_a_uto    _c_ode block     _p_romote        _H_tml        h_@_w_m_
italic:_/_    pre:_:_         _f_ootnote      code i_n_line    _d_emote         _P_DF         _e_asy-hugo
リスト:_._    _t_able         _r_eference     _l_ink           _j_:move-up      _D_ocx        _h_atenablog
取消線:_x_    hr:_-_          _i_mage         _u_ri            _k_:move-down    Pre_v_iew     _q_iita"
   ("s" markdown-insert-bold)
   ("/" markdown-insert-italic)
   ("-" markdown-insert-hr)
   ("x" markdown-insert-strike-through)
   ("b" markdown-insert-blockquote)
   (":" markdown-insert-pre)
   ("t" markdown-insert-table)
   ("c" markdown-insert-gfm-code-block)
   ("n" markdown-insert-code)
   ("K" markdown-insert-kbd)
   ("a" markdown-insert-header-dwim)
   ("e" easy-hugo)
   ("q" (browse-url "https://qiita.com/minoruGH"))
   ("@" my:howm-list-all)
   ("m" hydra-memo/body)
   ("h" hydra-hatena/body)
   ("1" markdown-insert-header-atx-1)
   ("2" markdown-insert-header-atx-2)
   ("3" markdown-insert-header-atx-3)
   ("4" markdown-insert-header-atx-4)
   ("5" markdown-insert-header-atx-5)
   ("6" markdown-insert-header-atx-6)
   ("." markdown-insert-list-item)
   ("i" markdown-insert-imaget)
   ("l" markdown-insert-link)
   ("u" markdown-insert-uri)
   ("f" markdown-insert-footnote)
   ("r" markdown-insert-reference-link-dwim)
   ("p" markdown-promote)
   ("d" markdown-demote)
   ("j" markdown-move-down)
   ("k" markdown-move-up)
   ;; Pndoc
   ("H" md2html :exit t)
   ("P" md2pdf :exit t)
   ("D" md2docx :exit t)
   ("v" markdown-preview :exit t))


;; Syntax higlight code blocks
(setq markdown-fontify-code-blocks-natively t)

(add-hook 'markdown-mode-hook
          '(lambda () (outline-minor-mode t)))

(defun md2html ()
  "Generate pdf from currently open markdown."
  (interactive)
  (let ((filename (buffer-file-name (current-buffer))))
    (shell-command-to-string
     ;; Place of templatete : ~/.pandoc/template/md2html.html
     (concat "pandoc -f markdown -t html --template=md2html "
	     filename
	     " > "
	     (file-name-sans-extension filename)
             ".html"))))

;; Use wkhtmltopdf without latex
(defun md2pdf ()
  "Generate pdf from currently open markdown."
  (interactive)
  (let ((filename (buffer-file-name (current-buffer))))
    (shell-command-to-string
     (concat "pandoc "
	     filename
	     " -f markdown -t html5 -o "
	     (file-name-sans-extension filename)
             ".pdf"))
    (when (eq system-type 'gnu/linux)   ;; for Debian
      (shell-command-to-string
       (concat "evince "
               (file-name-sans-extension filename)
               ".pdf")))
    (when (eq system-type 'darwin)      ;; for macOS
      (shell-command-to-string
       (concat "open -a preview.app "
               (file-name-sans-extension filename)
	       ".pdf")))))

(defun md2docx ()
  "Generate docx from currently open markdown."
  (interactive)
  (let ((filename (buffer-file-name (current-buffer))))
    (shell-command-to-string
     (concat "pandoc "
	     filename
	     " -t docx -o "
	     (file-name-sans-extension filename)
	     ;; ".docx -V mainfont=IPAPGothic -V fontsize=16pt --toc --highlight-style=zenburn"))
	     ".docx -V mainfont=IPAPGothic -V fontsize=16pt --highlight-style=zenburn"))
    (when (eq system-type 'gnu/linux)
      (shell-command-to-string
       (concat "xdg-open "
               (file-name-sans-extension filename)
               ".docx")))
    (when (eq system-type 'darwin)
      (shell-command-to-string
       (concat "open -a pages.app "
               (file-name-sans-extension filename)
               ".docx")))))

;; markdown-preview like github ...C-c C-c p
(setq markdown-command "pandoc"
      markdown-command-needs-filename t
      markdown-content-type "application/xhtml+xml"
      markdown-css-paths '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"
			   "http://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css")
      markdown-xhtml-header-content "
<meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>
<style>
body {
  box-sizing: border-box;
  max-width: 740px;
  width: 100%;
  margin: 40px auto;
  padding: 0 10px;
}
</style>
<script src='http://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>
<script>
document.addEventListener('DOMContentLoaded', () => {
  document.body.classList.add('markdown-body');
  document.querySelectorAll('pre[lang] > code').forEach((code) => {
    code.classList.add(code.parentElement.lang);
    hljs.highlightBlock(code);
  });
});
</script>
")

;; Local Variables:
;; byte-compile-warnings: (not free-vars callargs)
;; End:
;;; 40_markdown-mode.el ends here
```

## Hydra-view-mode ##

![Alt Text](https://farm8.staticflickr.com/7898/46548095055_c9935358c6_b.jpg) 


長いコードなどを閲覧する時に便利なようにview-modeを設定しています。

一文字削除、一行削除、undoなどvim風な簡単な機能もつけました。magitやgit-gutterも使えます。大げさな割にはそれほど使わないのですが、まあ自己満足の世界です(^o^)。

``` emacs-lisp
;;; 40_view-mode.el --- 40_view-mode.el
;;; Commentary:
;;; Code:
;;(setq debug-on-error t)

;; Change modeline color in view-mode
(use-package viewer)
(setq viewer-modeline-color-view "#852941")
(viewer-change-modeline-color-setup)


;; Setting to utilize view-mode
(key-chord-define-global "::" 'view-mode)
(add-hook 'view-mode-hook
	  (lambda ()
	    (define-key view-mode-map "i" 'View-exit)
	    (define-key view-mode-map ":" 'View-exit)
	    (define-key view-mode-map "g" 'beginning-of-buffer)
	    (define-key view-mode-map "G" 'end-of-buffer)
	    (define-key view-mode-map "e" 'end-of-line)
	    (define-key view-mode-map "a" 'beginning-of-line)
	    (define-key view-mode-map "b" 'scroll-down-command)
	    (define-key view-mode-map "D" 'my/view-kill-whole-line)
	    (define-key view-mode-map "u" 'my/view-undo)
	    (define-key view-mode-map "X" 'my/view-del-char)
	    (define-key view-mode-map "w" 'my/view-forward-word+1)
	    (define-key view-mode-map "W" 'backward-word)
	    (define-key view-mode-map "s" 'swiper-for-region-or-swiper)
	    (define-key view-mode-map "t" 'git-timemachine)
	    (define-key view-mode-map "v" 'vc-diff)
	    (define-key view-mode-map "[" 'forward-list)
	    (define-key view-mode-map "]" 'backward-list)
	    (define-key view-mode-map "l" 'goto-line)
	    (define-key view-mode-map ";" 'recenter-top-bottom)
	    (define-key view-mode-map "m" 'magit-status)
	    (define-key view-mode-map "B" 'magit-blame)
	    (define-key view-mode-map "j" 'git-gutter:next-hunk)
	    (define-key view-mode-map "k" 'git-gutter:previous-hunk)
	    (define-key view-mode-map "r" 'git-gutter:revert-hunk)
	    (define-key view-mode-map "S" 'git-gutter:stage-hunk)
	    (define-key view-mode-map "p" 'git-gutter:popup-hunk)
	    (define-key view-mode-map "," 'hydra-window/body)
	    (define-key view-mode-map "_" 'delete-other-windows)
	    (define-key view-mode-map "." 'hydra-view-mode/body)))


;; Function to edit in view-mode
(defun my/view-forward-word+1 ()
  "Forward word+1 in view mode."
  (interactive)
  (forward-word)
  (forward-char))
(defun my/view-kill-whole-line ()
  "Kill whole line in view mode."
  (interactive)
  (view-mode 0)
  (kill-whole-line)
  (save-buffer)
  (view-mode 1)
  (message "kill-whole-line and save!"))
(defun my/view-del-char ()
  "Delete character in view mode."
  (interactive)
  (view-mode 0)
  (delete-char 1)
  (save-buffer)
  (view-mode 1)
  (message "delete-char"))
(defun my/view-undo ()
  "Undo in view mode."
  (interactive)
  (view-mode 0)
  (undo)
  (save-buffer)
  (view-mode 1)
  (message "undo and save!"))


;; hydra-view-mode
(defhydra hydra-view-mode (:hint nil :color pink)
  "
_SPC_: next page   _a_: top of line  _u_: view undo      _m_: magit-status  _j_: gg:next-hunk   _s_: swiper
  _b_: prev page   _e_: end of line  _w_: forward word   _B_: magit-blame   _k_: gg:prev-hunk   _d_: dired-jump
  _g_: page top    _l_: goto line    _W_: backward word  _t_: timemachine   _p_: gg:popup-hunk  _i_: view exit
  _G_: page end    _D_: delete line  _[_: forward pair   _v_: vc-diff       _S_: gg:stage-hunk  _q_: view quit
  _;_: top-bottom  _X_: delete char  _]_: backward pair  _h_: github        _r_: gg:revert-hun  _._: close
"
  ;; Move page
  ("SPC" scroll-up-command)
  ("b" scroll-down-command)
  ("g" beginning-of-buffer)
  ("G" end-of-buffer)
  ;; Move line
  ("a" beginning-of-line)
  ("e" end-of-line)
  ("w" my/view-forward-word+1)
  ("W" backward-word)
  ("D" my/view-kill-whole-line)
  ("X" my/view-del-char)
  ("u" my/view-undo)
  ;; Misc
  ("i" View-exit :exit t)
  ("q" View-quit :exit t)
  (":" View-exit :exit t)
  ("[" forward-list)
  ("]" backward-lis)
  ("l" goto-line)
  ;; git
  ("v" vc-diff)
  ("m" magit-status :exit t)
  ("B" magit-blame :exit t)
  ("t" git-timemachine :exit t)
  ("h" my/github)
  ;; gitgutter
  ("j" git-gutter:next-hunk)
  ("k" git-gutter:previous-hunk)
  ("p" git-gutter:popup-hunk)
  ("S" git-gutter:stage-hunk)
  ("r" git-gutter:revert-hunk)
  (";" recenter-top-bottom)
  ;; Others
  ("d" dired-jump :exit t)
  ("_" delete-other-windows :exit t)
  ("s" swiper-for-region-or-swiper)
  ("," hydra-window/body :exit t)
  ("." nil :color blue))

;; Local Variables:
;; byte-compile-warnings: (not free-vars callargs)
;; End:
;;; 40_view-mode.el ends here
```

## Hydra-browse ##

![Alt Text](https://farm8.staticflickr.com/7924/46548095355_6be31c004f_b.jpg) 

何でもかんでもemacsを作業デスクとしてして生活しているので、色んな場所へジプシーするためのお気に入りランチャーです。macアプリもいれました。

``` emacs-lisp
;;; 50_hydra-browse.el --- 50_hydra-browse.el
;;; Commentary:
;;; Code:
;;(setq debug-on-error t)

(bind-key
 [f10]
 (defhydra hydra-browse (:hint nil :exit t)
  "
 ^Shop^           ^SNS^           ^Repos^          ^GH^           ^Favorite^      ^Others^       ^Applications
 ^^^^^^---------------------------------------------------------------------------------------------------
 _a_: Amazon      _t_: Twitter    _g_: github      _h_: HOME      _j_: Jorudan    _D_: Dropbox   _c_: Browse-url
 _r_: Rakuten     _u_: Youtube    _0_: gist        _d_: d.kukai   _n_: News       _x_: Xserver   _l_: ForkLift
 _y_: Yodobashi   _f_: Flickr     _1_: masasam     _m_: m.kukai   _w_: Weather    _q_: Qiita     _@_: Airmail
 _k_: Kakaku      _e_: Evernote   _2_: hydraWiki   _b_: BBS       _B_: SanyoBas   _p_: Photo(G)  _i_: iTerm
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
   ("_" (browse-url "https://github.com/minorugh/emacs.d/"))
   ("1" (browse-url "https://github.com/masasam/dotfiles/tree/master/.emacs.d"))
   ("2" (browse-url "https://github.com/abo-abo/hydra/wiki"))
   ("h" (browse-url "http://gospel-haiku.com/"))
   ("d" (browse-url "http://gospel-haiku.com/d_kukai/"))
   ("m" (browse-url "http://gospel-haiku.com/m_kukai/"))
   ("b" (browse-url "http://gospel-haiku.com/danwa/"))
   ("j" (browse-url "https://www.jorudan.co.jp/"))
   ("n" (browse-url "https://news.yahoo.co.jp/"))
   ("w" (browse-url "https://tenki.jp/week/6/31/"))
   ("B" (browse-url "http://www.sanyo-bus.co.jp/pdf/180913.pdf"))
   ("x" (browse-url "https://www.xserver.ne.jp/login_server.php"))
   ("D" (browse-url "https://www.dropbox.com/home/documents"))
   ("q" (browse-url "https://qiita.com/minoruGH"))
   ("v" (browse-url "https://drive.google.com/drive/u/0/my-drive"))
   ("p" (browse-url "https://photos.google.com/?pageId=none"))
   ("/" kill-other-buffers)
   ("," hydra-window/body)
   ("." hydra-work1/body)
   ("c" browse-url-at-point)
   ("s" my:skitch-app)
   ("l" my:forklift-app)
   ("@" my:amail-app)
   ("i" my:iterm-app)
   ("<f10>" nil)))

;; Launch for Mac applications
(defun my:calendar-app ()
  "Launch for calendar.app."
  (interactive)
  (shell-command "open -a calendar.app"))
(defun my:forklift-app ()
  "Launch for forklift.app."
  (interactive)
  (shell-command "open -a forklift.app"))
(defun my:amail-app ()
  "Launch for airmail3.app."
  (interactive)
  (shell-command "open -a 'airmail 3.app'"))
(defun my:iterm-app ()
  "Launch for iterm.app."
  (interactive)
  (shell-command "open -a iterm.app"))
(defun my:skitch-app ()
  "Launch for skitch.app."
  (interactive)
  (shell-command "open -a skitch.app"))


;; Local Variables:
;; byte-compile-warnings: (not free-vars callargs)
;; End:
;;; 50_hydra-browse.el ends here
```

## Hydra-compile ##

![Alt Text](https://farm8.staticflickr.com/7869/46740221534_ba6d113363_b.jpg) 

> 同じことは２度しない。必ずしなければ習い作業 → 自動化できないか考える。

これは私が原点としている心がけですが、尊敬する故・石井 勝氏による下記記事の受け売りです。

  * [自動化のためのGNU Make入門講座](bjectclub.jp/community/memorial/homepage3.nifty.com/masarl/article/gnu-make.html)

emacsには、`compile` というコマンドがあり、デフォルトで `make -k` を実行してくれます。

それ以外のmakeコマンドを実行させたいときは、deleteで "-k" の部分を消して書き換えるのですが、そうすると次回 `compile` コマンドを実行したときに `make -k` にならないで書き換えたものに置き換えられてしまいますう。それらを解決するためにhydraで工夫してみました。

`make -k` 以外のときは、`shell-command` を使います。

``` emacs-lisp
;;; 50_hydra-compile.el --- 50_hydra-compile.el
;;; Commentary:
;;; Code:
;;(setq debug-on-error t)

(bind-key
 [f2]
 (defhydra hydra-make (:color red :hint nil)
  "
Command: _RET_:compile  _u_pftp  _m_ove  _b_klog  _g_it  _c_lean  _r_estore:compile-command"
   ("RET" compile)
   ("u" my:make-upftp :exit t)
   ("m" my:make-move :exit t)
   ("g" my:make-git :exit t)
   ("b" my:make-bklog :exit t)
   ("c" my:make-clean)
   ("r" my:restore-command)))

(defun my:make-upftp ()
  "Make command for upftp."
  (interactive)
  (shell-command "make up"))

(defun my:make-move ()
  "Make command for move."
  (interactive)
  (shell-command "make mv"))

(defun my:make-bklog ()
  "Make command for bklog."
  (interactive)
  (shell-command "make bk"))

(defun my:make-git ()
  "Make command for git."
  (interactive)
  (shell-command "make git"))

(defun my:make-clean ()
  "Make command for clean."
  (interactive)
  (shell-command "make clean"))

(defun my:restore-command ()
  "Restore for compile command."
  (interactive)
  (setq compile-command "make -k")
  (message "Restored!"))

;; Local Variables:
;; byte-compile-warnings: (not free-vars callargs)
;; End:
;;; 50_hydra-compile ends here
```

## Hydra-window ##

![Alt Text](https://farm8.staticflickr.com/7839/46740221244_142d9b2292_b.jpg) 

ネット上でも最近良く見かけるemacsのwindow制御です。

正直、"C-x .." を叩く癖が身についているのでそれほど便利な気はしませんが、swap,toggle-divosionのほか、frameにも対応させています。

hydraとは別に `window移動：other-window-or-split` を "C-q" にも割り当てています。私の場合、二分割を超えて使うことは先ずないので、
慣れ親しんだこちらのほうが使いやすいです。

``` emacs-lisp
;;; 50_hydra-window.el --- 50_hydra-window.el
;;; Commentary:
;;; Code:
;;(setq debug-on-error t)

(bind-key
 [f7]
 (defhydra hydra-window (:color red :hint nil)
   "
Window: _v_sprit  _h_sprit  _o_ther  _s_wap  e_x_change  del_0_:_1_  |  Frame: _n_ew  _m_ove  _d_el"
  ("v" split-window-right)
  ("h" split-window-below)
  ("o" other-window-or-split)
  ("s" window-swap-states)
  ("x" window-toggle-division)
  ("0" delete-window :exit t)
  ("1" delete-other-windows :exit t)
  ;; Common setting with hydra-work
  ("_" delete-other-windows :exit t)
  ("n" new-frame)
  ("m" other-frame)
  ("d" delete-frame :exit t)))


(defun other-window-or-split ()
  "If there is one window, open split window.
If there are two or more windows, it will go to another window."
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(bind-key "C-q" 'other-window-or-split)


(defun window-toggle-division ()
  "Replace vertical <-> horizontal when divided into two."
  (interactive)
  (unless (= (count-windows 1) 2)
    (error "Not divided into two!"))
  (let ((before-height)
        (other-buf (window-buffer (next-window))))
    (setq before-height (window-height))
    (delete-other-windows)
    (if (= (window-height) before-height)
        (split-window-vertically)
      (split-window-horizontally))
    (other-window 1)
    (switch-to-buffer other-buf)
    (other-window -1)))



;; Local Variables:
;; byte-compile-warnings: (not free-vars callargs)
;; End:
;;; 50_hydra-window.el ends here
```

## Hydra-howm ##

![Alt Text](https://farm8.staticflickr.com/7917/46548095175_ccce1b85d3_b.jpg) 

私はemacsでタスク管理はしないのでorg-modeには余り興味がなくシンプルなhowmを愛用しています。

そのhowmも本来の使い方ではなく "メモデーターの保存と管理" 目的で使っています。メモの種類は以下のとおりです。

  * ブログ記事などの下書き: howm-create で書く
  * ちょこっとしたアイデアメモ: howm-remember で書く
  * 俳句創作手帳: elispコマンドでtemplateを挿入
  * open-junk-file: いわゆる自作のちょこっとしたジュンクコード

上記それぞれ保存するデレクトリは分けて別々に一覧表示させるのですが、全てをhowmディレクトリ下に配置することで全ファイルを串刺し検索できるようにもしています。長くなるので詳細は別記事にしたいと思います。

``` emacs-lisp
;;; 60_howm.el --- 60_howm.el
;;; Commentary:
;;; Code:
;;(setq debug-on-error t)

;; Set the title-hearder chara first and then reqire howm.
(setq howm-view-title-header "#")
(use-package howm)

;; List buffer and content buffer side by side
(setq howm-view-split-horizontally t)
;; Clear list buffer when opening file with RET
(setq howm-view-summary-persistent nil)
;; List in descending order
(setq howm-list-normalizer 'howm-view-sort-by-reverse-date)

;; hydra
(defhydra hydra-howm (:hint nil :exit t)
   "
HOWM: _c_reate  _m_emo  _f_ile  //  LIST: mem_o_  _h_aiku  _j_unk  all_@_"
   ("c" my:howm-create)
   ("m" my:howm-memo-create)
   ("f" open-junk-file)
   ("o" my:howm-memo-list-all)
   ("h" my:howm-haiku-list-all)
   ("j" my:howm-junk-list-all)
   ("@" my:howm-list-all))

;; open-junk-file
(setq open-junk-file-format "~/Dropbox/howm/junk/%Y/junk.%Y%m%d.")

;; Set howm directory and file name format
(defun my:howm-create ()
  "My howm memo create."
  (interactive)
  (setq howm-directory "~/Dropbox/howm")
  (setq howm-file-name-format "%Y/%m/%Y%m%d%H%M.md")
  (howm-create))
(defun my:howm-memo-create ()
  "My howm memo create."
  (interactive)
  (setq howm-directory "~/Dropbox/howm/memo")
  (setq howm-file-name-format "%Y/%m/memo-%Y%m%d.md")
  (howm-remember))

;; Set howm directory for list display
(defun my:howm-memo-list-all ()
  "My howm memo list all."
  (interactive)
  (setq howm-directory "~/Dropbox/howm/memo")
  (howm-list-all))
(defun my:howm-list-all ()
  "My howm list all."
  (interactive)
  (setq howm-directory "~/Dropbox/howm")
  (howm-list-all))
(defun my:howm-haiku-list-all ()
  "My howm list all."
  (interactive)
  (setq howm-directory "~/Dropbox/howm/haiku")
  (howm-list-all))
(defun my:howm-junk-list-all ()
  "My howm list all."
  (interactive)
  (setq howm-directory "~/Dropbox/howm/junk")
  (howm-list-all))

;; Local Variables:
;; byte-compile-warnings: (not free-vars callargs)
;; End:
;;; 60_howm.el ends here
```

## Hydra-search-web ##

![Alt Text](https://farm8.staticflickr.com/7825/40497399043_4d85414338_b.jpg) 

必須のgoogle-translateは別設定です。

googole-this とかを使うTipsも多いですが、私は昔から、[search-web](https://github.com/nomaddo/search-web.el) というパッケージを愛用しています。簡単な設定で自由にカスタマイズすることができて様々な検索サイトを利用することが出来るからです。

郵便番号から住所を検索したりgoogle−mapやgoogle-earthを検索すると圧巻です。

``` emacs-lisp
;;; 50_hydra-search-web.el --- 50_hydra-search-web.el
;;; Commentary:
;;; Code:
;;(setq debug-on-error t)

(use-package search-web)
(add-to-list 'search-web-engines '("weblio" "http://weblio.jp/content/%s" nil))
(add-to-list 'search-web-engines '("kobun" "http://kobun.weblio.jp/content/%s" nil))
(add-to-list 'search-web-engines '("ruigo" "http://thesaurus.weblio.jp/content/%s" nil))
(add-to-list 'search-web-engines '("github" "https://github.com/search?utf8=✓&q=%s&ref=simplesearch" nil))
(add-to-list 'search-web-engines '("qitta" "https://qiita.com/search?q=%s" nil))
(add-to-list 'search-web-engines '("post" "https://postcode.goo.ne.jp/search/q/%s/" nil))
(add-to-list 'search-web-engines '("earth" "https://earth.google.com/web/search/%s/" nil))


(bind-key
 "s-s"
 (defhydra hydra-search-web (:color red :hint nil)
   "
Search:  _a_mazon  _g_oogle  _t_ranslate  _e_ijiro  _m_aps  git_h_ub  _q_itta  _w_eblio  _p_〒  _k_古語  _r_類語 _._close"
  ("a" (search-web-dwim "amazon jp"))
  ("e" (search-web-dwim "eijiro"))
  ("g" (search-web-dwim "google"))
  ("m" (search-web-dwim "google maps"))
  ("h" (search-web-dwim "github"))
  ("q" (search-web-dwim "qitta"))
  ("w" (search-web-dwim "weblio"))
  ("p" (search-web-dwim "post"))
  ("k" (search-web-dwim "kobun"))
  ("r" (search-web-dwim "ruigo"))
  ("t" google-translate-auto)
  ("q" nil)
  ("." nil :color blue)))

;; Local Variables:
;; byte-compile-warnings: (not free-vars callargs)
;; End:
;;; 50_hydra-search-web.el ends here
```

## まとめ ##

![Alt Text](https://farm8.staticflickr.com/7891/47410497172_5f912d55c9_b.jpg) 

とりいそぎざっくりと紹介しました。自作のコードはほとんどなく大半はパッチワークです。

Emacsの設定ファイルはGitHubに公開していますので興味ある方は覗いてみてください。

  * [https://github.com/minorugh/emacs.d](https://github.com/minorugh/emacs.d)


