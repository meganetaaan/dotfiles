"Python3 support
" let g:python3_host_prog = expand('$HOME') . '/.anyenv/envs/pyenv/shims/python'
let g:python3_host_prog = '/usr/bin/python3'

"""" dein settings {{{
if &compatible
  set nocompatible
endif
" dein.vimのディレクトリ
let s:dein_dir = expand('~/.cache/dein')
let s:dein_repo_dir = s:dein_dir . '/repos/github.com/Shougo/dein.vim'

" なければgit clone
if !isdirectory(s:dein_repo_dir)
  execute '!git clone https://github.com/Shougo/dein.vim' s:dein_repo_dir
endif
execute 'set runtimepath^=' . s:dein_repo_dir

if dein#load_state(s:dein_dir)
  call dein#begin(s:dein_dir)
  " 管理するプラグインを記述したファイル
  let s:toml = '~/.config/nvim/dein.toml'
  let s:lazy_toml = '~/.config/nvim/dein_lazy.toml'
  call dein#load_toml(s:toml, {'lazy': 0})
  call dein#load_toml(s:lazy_toml, {'lazy': 1})
  call dein#end()
  call dein#save_state()
endif

" vimprocだけは最初にインストールしてほしい
if dein#check_install(['vimproc'])
  call dein#install(['vimproc'])
endif
" その他インストールしていないものはこちらに入れる
if dein#check_install()
  call dein#install()
endif
" }}}

" Neomake setting
let g:neomake_javascript_enabled_makers = ['standard']
let g:neomake_open_list = 2
let g:neomake_verbose = 3

""" Syntastic
" let g:syntastic_check_on_open=0 "ファイルを開いたときはチェックしない
" let g:syntastic_check_on_save=1 "保存時にはチェック
" let g:syntastic_check_on_wq = 0 " wqではチェックしない
" let g:syntastic_auto_loc_list=1 "エラーがあったら自動でロケーションリストを開く
" let g:syntastic_loc_list_height=6 "エラー表示ウィンドウの高さ
" set statusline+=%#warningmsg# "エラーメッセージの書式
" set statusline+=%{SyntasticStatuslineFlag()}
" set statusline+=%*
" let g:syntastic_typescript_checkers = ['']
" let g:syntastic_javascript_checkers = ['eslint'] "ESLintを使う
" let g:syntastic_mode_map = {
"       \ 'mode': 'active',
"       \ 'active_filetypes': ['javascript'],
"       \ 'passive_filetypes': []
"       \ }

let g:quickrun_config={'*': {'split': ''}}
filetype plugin indent on

""" Autoformat
au BufWrite * :Autoformat

""" HTML
vmap <silent> ;h :s?^\(\s*\)+ '\([^']\+\)',*/s$?\1\2?g<CR>
vmap <silent> ;q :s?^\(\s*\)\(.*\)\s*$? \1 + '\2'?<CR>

""" CSS

""" JavaScript

""" Jade
autocmd BufNewFile,BufRead *.jade setlocal filetype=jade

""" TypeScript
autocmd BufNewFile,BufRead *.ts setlocal filetype=typescript

" Display
set notitle
syntax enable
set number

" Indent & Tab
set smarttab
set expandtab
set autoindent
set ts=2 sw=2 sts=0

" Key
" nmap <space> zz
nmap n nzz
nmap N Nzz
nnoremap <ESC><ESC> :nohlsearch<CR>
imap jk <Esc>
imap ｊｋ <Esc>

" StatulLine
" "ステータス行を表示
" set laststatus=2
" "ステータス行の指定
" set statusline=%<%f\ %m%r%h%w
" set statusline+=%{'['.(&fenc!=''?&fenc:&enc).']['.&fileformat.']'}
" set statusline+=%=%l/%L,%c%V%8P

" vim airline
set laststatus=2
set showtabline=2
set t_Co=256
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#buffer_idx_mode = 1
let g:airline_theme = 'molokai'

" Backup
set backupdir=~/.vim/backup,.

" Persistent Undo
if has('persistent_undo')
  set undodir=./.vimundo,~/.vim/undo,.
  set undofile
endif

"" Color scheme
colorscheme molokai
let g:molokai_original = 1
let g:rehash256 = 1
set background=dark

