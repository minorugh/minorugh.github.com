+++
date = "2021-11-25T12:01:10+09:00"
categories = ["tech"]
tags = ["gh"]
title = "Linuxbrewで Github CLI（gh）を install"
+++

先の Linuxbrewさえうまくインストールできれば、brewで ghを installするのは実に簡単。

## install

```shell
% brew install gh
```
とするだけ。

コマンドラインで使えるように `.zshrc` に下記の設定を書く。

```zshrc
# GitHub CLI
eval "$(gh completion -s zsh)"
```
bashなら `.bashrc`に書く。

```shell
eval "$(gh completion -s bash)"
```


## auth login

次のように `gh auth login` と入力しあとは terminal画面の指示通りに進めて完了させれば、ghコマンドが使えるようになります。

```shell
% gh auth login
? What account do you want to log into? GitHub.com
- Logging into github.com
? How would you like to authenticate? Login with a web browser
! First copy your one-time code: ****-****
#👇Enterキーを押すとブラウザから認証作業ができます(下の図参照)。
- Press Enter to open github.com in your browser...
#👇ブラウザでの作業完了後に表示されます。
✓ Authentication complete. Press Enter to continue...
? Choose default git protocol HTTPS
- gh config set -h github.com git_protocol https
✓ Configured git protocol
✓ Logged in as ***********
```

認証後正常にログインできるようになっていると、ステータスが正常に跳ね返ってきます。

```shell
% gh auth status                                                                                                        [~][master]
github.com
  ✓ Logged in to github.com as minorugh (/home/minoru/.config/gh/hosts.yml)
  ✓ Git operations for github.com configured to use https protocol.
  ✓ Token: *******************
```

## Write automation commands in zshrc

現状、ghコマンドで複雑なことをするつもりはないので、新規リポジトリ作成と cloneだけを.zshrcに設定した。

```zshrc
# Create a new repository and first commit
function gh-new() {
    echo "Type repository name: " && read name;
	echo "# ${name}" >> README.md
    git init && git add README.md && git commit -m "First commit"
    gh repo create ${name}
	git branch -M main
	git remote rm origin
    git remote add origin git@github.com:minorugh/${name}.git
    git push -u origin main;
}

# Cloning a repository
function gh-clone() {
    echo "Type repository name: " && read name;
    gh repo clone ${name}
```

ローカルリポジトリ用のデレクトリを作成し、そのデレクトリをカレントとして `gh-new` コマンドを打てばあとは全自動でローカルレポジトリとの同期まで完了する。

cloneも同様に、作成したローカルリポジトリ用のデレクトリで `gh-clone` を打つだけ。
