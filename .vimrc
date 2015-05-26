" Must have options
set nocompatible               " be iMproved
set backspace=indent,eol,start
set number
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
noremap <CR> i<CR><ESC>

nnoremap <ESC><ESC> :nohlsearch<CR>

imap jk <Esc>
imap ｊｋ <Esc>


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
	call neobundle#begin(expand('~/.vim/bundle/'))
endif

" originalrepos on github
" NeoBundle 'surround.vim'

" molokai カラースキーム
NeoBundle 'tomasr/molokai'

" unite-outline
NeoBundle 'https://github.com/h1mesuke/unite-outline.git'
nnoremap ,uo :<C-u>Unite -vertical -winwidth=30 -no-quit outline<CR>

" はてなブログに投稿
" NeoBundle 'toyamarinyon/hatenablog-vim'

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

" Git
NeoBundle 'git://github.com/tpope/vim-fugitive.git'

" Haskell
NeoBundle 'eagletmt/ghcmod-vim'
NeoBundle 'kana/vim-filetype-haskell'
nmap <Bslash>t :GhcModType
nnoremap ,h :<C-u>Unite hoogle<CR>
NeoBundle 'ujihisa/neco-ghc'

" scala
NeoBundle 'scala.vim'
NeoBundle 'derekwyatt/vim-scala'
" ファイルタイプの追加
augroup filetypedetect
    autocmd! BufNewFile,BufRead *.scala setfiletype scala
    autocmd! BufNewFile,BufRead *.sbt setfiletype scala
augroup END

" neobundle"{{{
" コマンドを伴うやつの遅延読み込み
"bundle"{{{
" その他 {{{
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

NeoBundle 'mattn/emmet-vim'
 let g:user_emmet_mode = 'iv'
  let g:user_emmet_leader_key = '<C-Y>'
  let g:use_emmet_complete_tag = 1
  let g:user_emmet_settings = {
        \ 'lang' : 'ja',
        \ 'html' : {
        \   'filters' : 'html',
        \ },
        \ 'css' : {
        \   'filters' : 'fc',
        \ },
        \ 'php' : {
        \   'extends' : 'html',
        \   'filters' : 'html',
        \ },
        \}
  augroup EmmitVim
    autocmd!
    autocmd FileType * let g:user_emmet_settings.indentation = '               '[:&tabstop]
  augroup END

NeoBundle 'tell-k/vim-browsereload-mac'
NeoBundle 'hail2u/vim-css3-syntax'
" NeoBundle 'taichouchou2/html5.vim'
NeoBundle 'pangloss/vim-javascript'
NeoBundle 'moll/vim-node'
NeoBundle 'kchmck/vim-coffee-script'
call neobundle#end()
NeoBundleCheck
nmap gW <Plug>(openbrowser-open)
" ColorScheme
colorscheme molokai

filetype plugin indent on     " required!
filetype indent on
syntax on

