+++
date = "2019-08-08T12:18:18+09:00"
categories = ["emacs"]
tags = ["mac"]
title = "Hide .DS_Store file from Emacs"
+++
`.DS_Store`ファイルをMacから削除する…というTipsは山ほどあるのですが、いづれもいつの間にかまた復活してしまいます。なので私の場合は、Emacsの各シーンで非表示になるように設定しています。

## counsel
```emacs-lisp
(setq counsel-find-file-ignore-regexp (regexp-opt '(".DS_Store")))
```

## dired
```emacs-lisp
;; omit .DS_Store
(require 'dired-x)
(setq-default dired-omit-files-p t)
(setq dired-omit-files "^\\.DS_Store")
```

## neotree
```emacs-lisp
;; Patched to allow everything but .DS_Store
;; Tips from https://github.com/syl20bnr/spacemacs/issues/2751
(with-eval-after-load 'neotree
  (defun neo-util--walk-dir (path)
    "Return the subdirectories and subfiles of the PATH."
    (let* ((full-path (neo-path--file-truename path)))
      (condition-case nil
	  (directory-files
	   path 'full "^\\([^.]\\|\\.[^D.][^S]\\).*")
	('file-error
	 (message "Walk directory %S failed." path)
	 nil)))))
```
