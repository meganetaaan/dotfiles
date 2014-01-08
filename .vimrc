" Must have options
set nocompatible               " be iMproved
set backspace=indent,eol,start
set nonumber
filetype off
set notitle
set smarttab
set expandtab
set ts=4 sw=4 sts=0

" KeyRemap
command! Ev edit $MYVIMRC
command! Rv source $MYVIMRC

nmap <space> zz
nmap n nzz
nmap N Nzz
autocmd BufNewFile log*.md 0r $HOME/.vim/template/log_md.txt
" noremap <CR> O<ESC>

nnoremap <ESC><ESC> :nohlsearch<CR>

" insert modeでの移動
inoremap <C-e> <END>
inoremap <C-a> <HOME>

inoremap <C-h> <Left>
inoremap <C-l> <Right>

" Display
set ambiwidth=double

" StatulLine
"ステータス行を表示
set laststatus=2

"ステータス行の指定
set statusline=%<%f\ %m%r%h%w
set statusline+=%{'['.(&fenc!=''?&fenc:&enc).']['.&fileformat.']'}
set statusline+=%=%l/%L,%c%V%8P

" Backup
set backupdir=./.vimbackup,~/.vim/backup,.
" Persistent Undo
if has('persistent_undo')
    set undodir=./.vimundo,~/.vim/undo,.
    "augroup vimrc-undofile
    "    autocmd!
    "    autocmd BufReadPre /work/ishikawa/* setlocal undofile
    "augroup END
    set undofile
endif

" neocomplcache & neosnippet
let g:neocomplcache_enable_at_startup = 1
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)

" VimShell
" ,is: シェルを起動
nnoremap <silent> ,is :VimShell<CR>
" ,irb: irbを非同期で起動
nnoremap <silent> ,irb :VimShellInteractive irb<CR>
" ,ss: 非同期で開いたインタプリタに現在の行を評価させる
nnoremap <silent> ,ghci :VimShellInteractive ghci<CR>
nnoremap <silent> ,ss <S-v>:VimShellSendString<CR>
" 選択中に,ss: 非同期で開いたインタプリタに選択行を評価させる
vmap <silent> ,ss :VimShellSendString<CR>

" TwitVim
" let twitvim_login_b64 ="bWVnYW5ldGFhYW46c3VraXlha2lkb24K" 
" let twitvim_count = 100

" ウィンドウを閉じずにバッファを閉じる
command! Ebd call EBufdelete()
function! EBufdelete()
  let l:currentBufNum = bufnr("%")
  let l:alternateBufNum = bufnr("#")

  if buflisted(l:alternateBufNum)
    buffer #
  else
    bnext
  endif

  if buflisted(l:currentBufNum)
    execute "silent bwipeout".l:currentBufNum
    " bwipeoutに失敗した場合はウインドウ上のバッファを復元
    if bufloaded(l:currentBufNum) != 0
      execute "buffer " . l:currentBufNum
    endif
  endif
endfunction

" ディレクトリが存在しない場合は確認・作成する
augroup vimrc-auto-mkdir  " {{{
	autocmd!
	autocmd BufWritePre * call s:auto_mkdir(expand('<afile>:p:h'), v:cmdbang)
	function! s:auto_mkdir(dir, force)  " {{{
		if !isdirectory(a:dir) && (a:force ||
					\    input(printf('"%s" does not exist. Create? [y/N]', a:dir)) =~? '^y\%[es]$')
			call mkdir(iconv(a:dir, &encoding, &termencoding), 'p')
		endif
	endfunction  " }}}
augroup END  " }}}

if has('vim_starting')
	set runtimepath+=~/.vim/bundle/neobundle.vim
	call neobundle#rc(expand('~/.vim/bundle/'))
endif
" originalrepos on github
"NeoBundle 'Shougo/neobundle.vim'
"NeoBundle 'Shougo/vimproc'
NeoBundle 'VimClojure'
NeoBundle 'Shougo/vimshell'
"NeoBundle 'Shougo/unite.vim'
"NeoBundle 'Shougo/neocomplcache'
"NeoBundle 'Shougo/neosnippet'
NeoBundle 'jpalardy/vim-slime'
NeoBundle 'scrooloose/syntastic'
NeoBundle 'scrooloose/nerdtree'
NeoBundle 'tpope/vim-markdown'
NeoBundle 'surround.vim'

" OpenBrowser
NeoBundle 'https://github.com/tyru/open-browser.vim.git'
nmap gW <Plug>(openbrowser-open)

" Ctags
NeoBundle 'majutsushi/tagbar'
nmap <Leader> tt :TagbarToggle<CR>

" colorscheme

" solarized カラースキーム
NeoBundle 'altercation/vim-colors-solarized'
" mustang カラースキーム
NeoBundle 'croaker/mustang-vim'
" wombat カラースキーム
NeoBundle 'jeffreyiacono/vim-colors-wombat'
" jellybeans カラースキーム
NeoBundle 'nanotech/jellybeans.vim'
" lucius カラースキーム
NeoBundle 'vim-scripts/Lucius'
" zenburn カラースキーム
NeoBundle 'vim-scripts/Zenburn'
" mrkn256 カラースキーム
NeoBundle 'mrkn/mrkn256.vim'
" railscasts カラースキーム
NeoBundle 'jpo/vim-railscasts-theme'
" pyte カラースキーム
NeoBundle 'therubymug/vim-pyte'
" molokai カラースキーム
NeoBundle 'tomasr/molokai'

" カラースキーム一覧表示に Unite.vim を使う
NeoBundle 'Shougo/unite.vim'
NeoBundle 'ujihisa/unite-colorscheme'
""NeoBundle 'https://bitbucket.org/kovisoft/slimv'

" ColorScheme
colorscheme molokai

" unite-outline
NeoBundle 'https://github.com/h1mesuke/unite-outline.git'
nnoremap ,uo :<C-u>Unite -vertical -winwidth=30 -no-quit outline<CR>

"Twitter for Vim
NeoBundle 'https://github.com/basyura/bitly.vim.git'
NeoBundle 'https://github.com/basyura/TweetVim.git'
NeoBundle 'https://github.com/basyura/twibill.vim.git'
"NeoBundle 'https://github.com/h1mesuke/unite-outline.git'
"NeoBundle 'https://github.com/mattn/webapi-vim.git'
"NeoBundle 'https://github.com/tyru/open-browser.vim.git'
NeoBundle 'https://github.com/yomi322/neco-tweetvim.git'
NeoBundle 'https://github.com/yomi322/unite-tweetvim.git'
let g:tweetvim_tweet_per_page = 50
nnoremap ,uv :<C-u>Unite tweetvim<CR>
nnoremap ,th :<C-u>TweetVimHomeTimeline<CR>
nnoremap ,tu :<C-u>TweetVimUserStream<CR>
nnoremap ,ts :<C-u>TweetVimSay<CR>

" 文字数をカウントする
" NeoBundle 'anekos/char-counter-vim'
" set statusline=%{b:charCounterCount}

" はてなブログに投稿
NeoBundle 'toyamarinyon/hatenablog-vim'

" QuickRun
NeoBundle 'thinca/vim-quickrun'
" 閉じるコマンド
nnoremap <expr><silent> <C-c> quickrun#is_running() ? quickrun#sweep_sessions() : "\<C-c>"
" 非同期実行
let g:quickrun_config = {
\       "_" : {
\               "runner" : "vimproc",
\               "runner/vimproc/updatetime" : 60,
\               "outputter" : "error",
\               "outputter/buffer/split" : ":botright",
\               "outputter/buffer/close_on_empty" : 1,
\               "outputter/error/success" : "buffer",
\               "outputter/error" : "quickfix",
\       },
\}

" vim-latex
NeoBundle 'git://vim-latex.git.sourceforge.net/gitroot/vim-latex/vim-latex'
filetype plugin on
let tex_flavor = 'platex'
" let tex_flavor = 'latex'
set grepprg=grep\ -nH\ $*
set shellslash
let g:Tex_DefaultTargetFormat = 'pdf'
let g:Imap_UsePlaceHolders = 1
let g:Imap_DeleteEmptyPlaceHolders = 1
let g:Imap_StickyPlaceHolders = 0
let g:Tex_CompileRule_dvi = 'platex --interaction=nonstopmode $*'
" let g:Tex_CompileRule_dvi = 'latex --interaction=nonstopmode $*'
let g:Tex_CompileRule_pdf = 'dvipdfmx $*.dvi'
let g:Tex_FormatDependency_ps = 'dvi,ps'
let g:Tex_FormatDependency_pdf = 'dvi,pdf'
let g:Tex_BibtexFlavor = 'jbibtex'
" let g:Tex_BibtexFlavor = 'bibtex'
let g:Tex_ViewRule_dvi = 'pxdvi'
let g:Tex_ViewRule_pdf = '/usr/bin/open -a preview'

" latex-suiteカスタマイズ
augroup MyIMAPs
  au!
" EALの入力で
" \begin{align}
" <++>
" \label{eq:<++>}
" \end{align}<++>
" を出力する。
  au VimEnter *.tex call IMAP('EAL','\begin{align}<++>
\label{eq:<++>}
\end{align}<++>','tex')
" `pの入力で^/primeを書き込む，などなど
  au VimEnter *.tex call IMAP('`p', '^\prime', 'tex') 
  au VimEnter *.tex call IMAP('`P', '\prime', 'tex') 
  au VimEnter *.tex call IMAP('`o', '\omega', 'tex') 
augroup END

filetype plugin indent on     " required!
filetype indent on
syntax on

" Git
NeoBundle 'git://github.com/tpope/vim-fugitive.git'

" Haskell
NeoBundle 'eagletmt/ghcmod-vim'
NeoBundle 'kana/vim-filetype-haskell'
nmap <Bslash>t :GhcModType

NeoBundle 'ujihisa/neco-ghc'

" neobundle"{{{
" コマンドを伴うやつの遅延読み込み
"bundle"{{{
" その他 {{{
NeoBundle 'Shougo/vimproc', {
			\ 'build' : {
			\     'mac' : 'make -f make_mac.mak',
			\     'unix' : 'make -f make_unix.mak',
			\    },
			\ }
NeoBundleLazy 'taichouchou2/vim-endwise.git', {
			\ 'autoload' : {
			\   'insert' : 1,
			\ } }
" }}}

" 補完 {{{
NeoBundleLazy 'Shougo/neocomplcache', {
			\ 'autoload' : {
			\   'insert' : 1,
			\ }}
NeoBundleLazy 'Shougo/neosnippet', {
			\ 'autoload' : {
			\   'insert' : 1,
			\ }}

NeoBundle 'Shougo/neocomplcache-rsense', {
			\ 'depends': 'Shougo/neocomplcache',
			\ 'autoload': { 'filetypes': 'ruby' }}
NeoBundleLazy 'taichouchou2/rsense-0.3', {
			\ 'build' : {
			\    'mac': 'ruby etc/config.rb > ~/.rsense',
			\    'unix': 'ruby etc/config.rb > ~/.rsense',
			\ } }
" }}}

" 便利 {{{
" 範囲指定のコマンドが使えないので、tcommentのLazy化はNeoBundleのアップデートを待ちましょう...
" NeoBundle 'tomtom/tcomment_vim'
" NeoBundleLazy 'tpope/vim-surround', {
" 			\ 'autoload' : {
" 			\   'mappings' : [
" 			\     ['nx', '<Plug>Dsurround'], ['nx', '<Plug>Csurround'],
" 			\     ['nx', '<Plug>Ysurround'], ['nx', '<Plug>YSurround'],
" 			\     ['nx', '<Plug>Yssurround'], ['nx', '<Plug>YSsurround'],
" 			\     ['nx', '<Plug>YSsurround'], ['vx', '<Plug>VgSurround'],
" 			\     ['vx', '<Plug>VSurround']
" 			\ ]}}
" }}}
" Excittranslate
NeoBundle 'git://github.com/mattn/webapi-vim.git'
NeoBundle 'mattn/excitetranslate-vim'

" ruby / railsサポート {{{
NeoBundle 'tpope/vim-rails'
NeoBundleLazy 'ujihisa/unite-rake', {
			\ 'depends' : 'Shougo/unite.vim' }
NeoBundleLazy 'basyura/unite-rails', {
			\ 'depends' : 'Shjkougo/unite.vim' }
NeoBundleLazy 'taichouchou2/unite-rails_best_practices', {
			\ 'depends' : 'Shougo/unite.vim',
			\ 'build' : {
			\    'mac': 'gem install rails_best_practices',
			\    'unix': 'gem install rails_best_practices',
			\   }
			\ }
NeoBundleLazy 'taichouchou2/unite-reek', {
			\ 'build' : {
			\    'mac': 'gem install reek',
			\    'unix': 'gem install reek',
			\ },
			\ 'autoload': { 'filetypes': ['ruby', 'eruby', 'haml'] },
			\ 'depends' : 'Shougo/unite.vim' }
"NeoBundleLazy 'taichouchou2/alpaca_complete', {
"			\ 'depends' : 'tpope/vim-rails',
"			\ 'build' : {
"			\    'mac':  'gem install alpaca_complete',
"			\    'unix': 'gem install alpaca_complete',
"			\   }
"			\ }

let s:bundle_rails = 'unite-rails unite-rails_best_practices unite-rake alpaca_complete'

function! s:bundleLoadDepends(bundle_names) "{{{
	" bundleの読み込み
	execute 'NeoBundleSource '.a:bundle_names
	au! RailsLazyPlugins
endfunction"}}}
aug RailsLazyPlugins
	au User Rails call <SID>bundleLoadDepends(s:bundle_rails)
aug END

" reference環境
NeoBundleLazy 'vim-ruby/vim-ruby', {
			\ 'autoload' : { 'filetypes': ['ruby', 'eruby', 'haml'] } }
NeoBundleLazy 'taka84u9/vim-ref-ri', {
			\ 'depends': ['Shougo/unite.vim', 'thinca/vim-ref'],
			\ 'autoload': { 'filetypes': ['ruby', 'eruby', 'haml'] } }
NeoBundleLazy 'ujihisa/ref-hoogle', {
			\ 'depends': ['thinca/vim-ref'],
			\ 'autoload': { 'filetypes': ['haskell'] } }
NeoBundleLazy 'skwp/vim-rspec', {
			\ 'autoload': { 'filetypes': ['ruby', 'eruby', 'haml'] } }
NeoBundleLazy 'ruby-matchit', {
			\ 'autoload' : { 'filetypes': ['ruby', 'eruby', 'haml'] } }
" }}}

" }}}
"}}}


" Load Plugins on other sites
