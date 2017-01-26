export LANG=ja_JP.UTF-8
if echo $OSTYPE | fgrep -q darwin; then
  export PATH=$HOME/.anyenv/bin:/bin:/sbin:/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin
  export ANDROID_HOME=/usr/local/opt/android-sdk
  export ECLIPSE_HOME=/Applications/eclipse/
  alias vim='env LANG=ja_JP.UTF-8 /Applications/MacVim.app/Contents/MacOS/mvim -v "$@"'
  alias ll='ls -l'
  eval "$(anyenv init - zsh)"
  # Go
  export GOROOT=/usr/local/opt/go/libexec
  export GOPATH=$HOME/Works/go
  export PATH=$GOPATH/bin:$GOROOT/bin:$PATH
fi
if echo $OSTYPE | fgrep -q linux; then
  alias pbcopy='xsel -i -b'
  alias pbpaste='xsel -o -b'
  alias ll='ls -l'
  alias vim='nvim'
  # alias tsc='$(npm bin)/tsc'
  # Go
  export GOROOT=/usr/lib/go
  export GOPATH=/usr/share/go
  export PATH=$GOPATH/bin:$GOROOT/bin:$PATH
fi
export GIT_EDITOR="/usr/local/bin/vim"

# シャープを含むコマンドライン引数を変に解釈しない
setopt nonomatch

## 履歴の保存先
HISTFILE=$HOME/.zsh-history
## メモリに展開する履歴の数
HISTSIZE=100000
## 保存する履歴の数
SAVEHIST=100000

## 補完機能の強化
autoload -U compinit
compinit -u

## コアダンプサイズを制限
limit coredumpsize 102400
## 出力の文字列末尾に改行コードが無い場合でも表示
unsetopt promptcr
## Emacsライクキーバインド設定
# bindkey -v
# bindkey "^P" up-line-or-history
# bindkey "^N" down-line-or-history
# #
# # Set vi mode status bar
# #
#
# #
# # Reads until the given character has been entered.
# #
# readuntil () {
#     typeset a
#     while [ "$a" != "$1" ]
#     do
#         read -E -k 1 a
#     done
# }
#
# #
# # If the $SHOWMODE variable is set, displays the vi mode, specified by
# # the $VIMODE variable, under the current command line.
# #
# # Arguments:
# #
# #   1 (optional): Beyond normal calculations, the number of additional
# #   lines to move down before printing the mode.  Defaults to zero.
# #
# showmode() {
#     typeset movedown
#     typeset row
#
#     # Get number of lines down to print mode
#     movedown=$(($(echo "$RBUFFER" | wc -l) + ${1:-0}))
#
#     # Get current row position
#     echo -n "\e[6n"
#     row="${${$(readuntil R)#*\[}%;*}"
#
#     # Are we at the bottom of the terminal?
#     if [ $((row+movedown)) -gt "$LINES" ]
#     then
#         # Scroll terminal up one line
#         echo -n "\e[1S"
#
#         # Move cursor up one line
#         echo -n "\e[1A"
#     fi
#
#     # Save cursor position
#     echo -n "\e[s"
#
#     # Move cursor to start of line $movedown lines down
#     echo -n "\e[$movedown;E"
#
#     # Change font attributes
#     echo -n "\e[1m"
#
#     # Has a mode been set?
#     if [ -n "$VIMODE" ]
#     then
#         # Print mode line
#         echo -n "-- $VIMODE -- "
#     else
#         # Clear mode line
#         echo -n "\e[0K"
#     fi
#
#     # Restore font
#     echo -n "\e[0m"
#
#     # Restore cursor position
#     echo -n "\e[u"
# }
#
# clearmode() {
#     VIMODE= showmode
# }
#
# #
# # Temporary function to extend built-in widgets to display mode.
# #
# #   1: The name of the widget.
# #
# #   2: The mode string.
# #
# #   3 (optional): Beyond normal calculations, the number of additional
# #   lines to move down before printing the mode.  Defaults to zero.
# #
# makemodal () {
#     # Create new function
#     eval "$1() { zle .'$1'; ${2:+VIMODE='$2'}; showmode $3 }"
#
#     # Create new widget
#     zle -N "$1"
# }
#
# # Extend widgets
# makemodal vi-add-eol           INSERT
# makemodal vi-add-next          INSERT
# makemodal vi-change            INSERT
# makemodal vi-change-eol        INSERT
# makemodal vi-change-whole-line INSERT
# makemodal vi-insert            INSERT
# makemodal vi-insert-bol        INSERT
# makemodal vi-open-line-above   INSERT
# makemodal vi-substitute        INSERT
# makemodal vi-open-line-below   INSERT 1
# makemodal vi-replace           REPLACE
# makemodal vi-cmd-mode          NORMAL
#
# unfunction makemodal

## 色を使う
setopt prompt_subst
## ビープを鳴らさない
setopt nobeep
## 内部コマンド jobs の出力をデフォルトで jobs -l にする
setopt long_list_jobs
## 補完候補一覧でファイルの種別をマーク表示
setopt list_types
## サスペンド中のプロセスと同じコマンド名を実行した場合はリジューム
setopt auto_resume
## 補完候補を一覧表示
setopt auto_list
## 直前と同じコマンドをヒストリに追加しない
setopt hist_ignore_dups
## cd 時に自動で push
setopt auto_pushd
## 同じディレクトリを pushd しない
setopt pushd_ignore_dups
## ファイル名で #, ~, ^ の 3 文字を正規表現として扱う
setopt extended_glob
## TAB で順に補完候補を切り替える
setopt auto_menu
## zsh の開始, 終了時刻をヒストリファイルに書き込む
setopt extended_history
## =command を command のパス名に展開する
setopt equals
## --prefix=/usr などの = 以降も補完
setopt magic_equal_subst
## ヒストリを呼び出してから実行する間に一旦編集
setopt hist_verify
## ファイル名の展開で辞書順ではなく数値的にソート
setopt numeric_glob_sort
## 出力時8ビットを通す
setopt print_eight_bit
## ヒストリを共有
setopt share_history
## 補完候補のカーソル選択を有効に
zstyle ':completion:*:default' menu select=1
## 補完候補の色づけ
#eval `dircolors`
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
export ZLS_COLORS=$LS_COLORS
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
## ディレクトリ名だけで cd
setopt auto_cd
## カッコの対応などを自動的に補完
setopt auto_param_keys
## ディレクトリ名の補完で末尾の / を自動的に付加し、次の補完に備える
setopt auto_param_slash
## スペルチェック
setopt correct
## {a-c} を a b c に展開する機能を使えるようにする
setopt brace_ccl
## Ctrl+S/Ctrl+Q によるフロー制御を使わないようにする
setopt NO_flow_control
## コマンドラインの先頭がスペースで始まる場合ヒストリに追加しない
setopt hist_ignore_space
## コマンドラインでも # 以降をコメントと見なす
setopt interactive_comments
## ファイル名の展開でディレクトリにマッチした場合末尾に / を付加する
setopt mark_dirs
## history (fc -l) コマンドをヒストリリストから取り除く。
setopt hist_no_store
## 補完候補を詰めて表示
setopt list_packed
## 最後のスラッシュを自動的に削除しない
setopt noautoremoveslash

alias ls='ls -lhGF'
autoload colors
colors

PROMPT=" %{${fg[yellow]}%}%~%{${reset_color}%}
[%n]$ "
PROMPT2='[%n]> '

#######################################
# peco hitory
#######################################
function peco-select-history() {
  BUFFER=`history -n 1 | tac | awk '!a[$0]++' | peco`
  CURSOR=$#BUFFER
  zle reset-prompt
}
zle -N peco-select-history
bindkey '^r' peco-select-history
# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

export PATH="$HOME/.cargo/bin:$PATH"
