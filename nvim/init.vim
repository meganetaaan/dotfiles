"Python3 support
let g:python3_host_prog = expand('$HOME') . '/.anyenv/envs/pyenv/shims/python'

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

"" HTML
vmap <silent> ;h :s?^\(\s*\)+ '\([^']\+\)',*/s$?\1\2?g<CR>
vmap <silent> ;q :s?^\(\s*\)\(.*\)\s*$? \1 + '\2'?<CR>

" StatulLine
"ステータス行を表示
set laststatus=2
"ステータス行の指定
set statusline=%<%f\ %m%r%h%w
set statusline+=%{'['.(&fenc!=''?&fenc:&enc).']['.&fileformat.']'}
set statusline+=%=%l/%L,%c%V%8P

" Backup
set backupdir=~/.vim/backup,.

" Persistent Undo
if has('persistent_undo')
    set undodir=./.vimundo,~/.vim/undo,.
    set undofile
endif

" NeoBundle
if has('vim_starting')
set runtimepath+=~/.config/nvim/bundle/neobundle.vim/
endif

call neobundle#begin(expand('~/.nvim/bundle/'))
NeoBundleFetch 'Shougo/neobundle.vim'
NeoBundle 'Shougo/deoplete.nvim'
NeoBundle 'tpope/vim-surround'
NeoBundle 'tomasr/molokai'

"" HTML
NeoBundle 'mattn/emmet-vim'

"" CSS
NeoBundle 'hail2u/vim-css3-syntax'

"" JavaScript
NeoBundleLazy 'heavenshell/vim-jsdoc' , {'autoload': {'filetypes': ['javascript']}}
NeoBundle 'moll/vim-node'
NeoBundle 'pangloss/vim-javascript'
NeoBundle 'Townk/vim-autoclose'
NeoBundle 'scrooloose/syntastic'
let g:syntastic_check_on_open=0 "ファイルを開いたときはチェックしない
let g:syntastic_check_on_save=1 "保存時にはチェック
let g:syntastic_check_on_wq = 0 " wqではチェックしない
let g:syntastic_auto_loc_list=1 "エラーがあったら自動でロケーションリストを開く
let g:syntastic_loc_list_height=6 "エラー表示ウィンドウの高さ
set statusline+=%#warningmsg# "エラーメッセージの書式
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_javascript_checkers = ['eslint'] "ESLintを使う
let g:syntastic_mode_map = {
      \ 'mode': 'active',
      \ 'active_filetypes': ['javascript'],
      \ 'passive_filetypes': []
      \ }
NeoBundle 'thinca/vim-quickrun'
""" 水平に分割する
let g:quickrun_config={'*': {'split': ''}}

call neobundle#end()

filetype plugin indent on
NeoBundleCheck

"" Color scheme
colorscheme molokai
let g:molokai_original = 1
let g:rehash256 = 1
set background=dark

let g:deoplete#enable_at_startup = 1 " :DeopleteEnable実行でもよい
