"dein Scripts-----------------------------
if &compatible
  set nocompatible               " Be iMproved
endif

" Required:
set runtimepath+=~/.vim/dein/repos/github.com/Shougo/dein.vim

" Required:
if dein#load_state('~/.vim/dein')
  call dein#begin('~/.vim/dein')

  " Let dein manage dein
  " Required:

  " Add or remove your plugins here:
  call dein#add('Shougo/neocomplete.vim')
  call dein#add('Shougo/neosnippet.vim')
  call dein#add('Shougo/neosnippet-snippets')
  call dein#add('tpope/vim-fugitive')
  call dein#add('vim-airline/vim-airline')
  call dein#add('vim-airline/vim-airline-themes')
  call dein#add('altercation/vim-colors-solarized')
  call dein#add('morhetz/gruvbox')
  call dein#add('scrooloose/nerdtree')
  call dein#add('jistr/vim-nerdtree-tabs')
  call dein#add('vim-scripts/surround.vim')
  call dein#add('cohama/lexima.vim')
  call dein#add('simeji/winresizer')
  call dein#add('majutsushi/tagbar')
  call dein#add('alvan/vim-closetag')
  call dein#add('bronson/vim-trailing-whitespace')
  call dein#add('dag/vim-fish')
  call dein#add('purescript-contrib/purescript-vim')
  call dein#add('FrigoEU/psc-ide-vim')
  call dein#add('stephpy/vim-yaml')
  call dein#add('vim-ruby/vim-ruby')
  call dein#add('dag/vim2hs')
  call dein#add('thinca/vim-quickrun')
  call dein#add('pangloss/vim-javascript')
  call dein#add('MaxMEllon/vim-jsx-pretty')
  call dein#add('fleischie/vim-styled-components')

  " You can specify revision/branch/tag.
  " call dein#add('Shougo/deol.nvim', { 'rev': 'a1b5108fd' })
  call dein#add('Shougo/deol.nvim')

  " Required:
  call dein#end()
  call dein#save_state()
endif

" Required:
filetype plugin indent on
syntax enable

" If you want to install not installed plugins on startup.
if dein#check_install()
  call dein#install()
endif

"End dein Scripts-------------------------

"=======================Settings of colorscheme======================
" toggle theme dark or light
set background=dark

"solarized
"let g:solarized_termtrans=1
"colorscheme solarized
"let g:airline_theme='solarized'

"gruvbox
colorscheme gruvbox
let g:gruvbox_contrast_dark='soft'
let g:airline_theme='gruvbox'
hi Normal guibg=NONE ctermbg=NONE
"====================================================================

"=======================Settings of Powerline========================
let g:airline#extensions#tabline#enabled=1
let g:airline#extensions#fugitiveline#enabled=1
let g:airline_powerline_fonts=1
"====================================================================

"==========================Settings of NERDTree======================
" Launch NERDTree when vim stert with vim-nerdtree-tabs
" instead of "autocmd vimenter * NERDTree".
let g:nerdtree_tabs_focus_on_files=1

" Show hidden files in NERDTree explorer window.
let NERDTreeShowHidden=1
"====================================================================

"================Settings of neocomplete, neosnippet=================

let g:neocomplete#enable_at_startup = 1
let g:neocomplete#enable_smart_case = 1
let g:neocomplete#min_keyword_length = 3
let g:neocomplete#enable_auto_delimiter = 1
let g:neocomplete#auto_completion_start_length = 1
inoremap <expr><BS> neocomplete#smart_close_popup()."<C-h>"

imap <expr><CR> neosnippet#expandable() ? "<Plug>(neosnippet_expand_or_jump)" : pumvisible() ? "<C-y>" : "<CR>"
imap <expr><TAB> pumvisible() ? "<C-n>" : neosnippet#jumpable() ? "<Plug>(neosnippet_expand_or_jump)" : "<TAB>"

" Plugin key-mappings.
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)

"====================================================================

"======================Settings of lexima.vim========================
let g:lexima_enable_basic_rules = 1
"====================================================================

"=======================Settings of tagbar===========================
let g:tagbar_autofocus = 1
"autocmd BufEnter * nested :call tagbar#autoopen(0)
"====================================================================

"======================Settings of purescript========================
let purescript_indent_if = 3
let purescript_indent_case = 5
let purescript_indent_let = 4
let purescript_indent_where = 6
let purescript_indent_do = 3
let purescript_indent_in = 1
let purescript_indent_dot = v:true
"====================================================================

"============================close tag===============================
let g:closetag_filenames = "*.html,*.xhtml,*.phtml,*.jsx,*.js"
let g:closetag_xhtml_filenames = '*.xhtml,*.jsx,*.js'
let g:closetag_emptyTags_caseSensitive = 1
let g:closetag_shortcut = '>'
let g:closetag_close_shortcut = '<leader>>'
"====================================================================

"=============================Remap keys=============================
" Disable following keys in order bad habits breaking.
noremap  <Up>        <nop>
noremap  <Down>      <nop>
noremap  <Left>      <nop>
noremap  <Right>     <nop>
noremap! <Up>        <nop>
noremap! <Down>      <nop>
noremap! <Left>      <nop>
noremap! <Right>     <nop>
noremap  <BackSpace> <nop>
noremap  <Delete>    <nop>
noremap! <BackSpace> <nop>
noremap! <Delete>    <nop>

noremap  <C-h> <C-w>h
noremap  <C-l> <C-w>l
noremap  <C-j> <C-w>j
noremap  <C-k> <C-w>k

" like emacs
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>
cnoremap <C-b> <Left>
cnoremap <C-f> <Right>
cnoremap <C-a> <Home>
cnoremap <C-e> <End>
cnoremap <C-d> <Del>

" Disable ex mode when press Q.
nnoremap Q <nop>

" Open and edit and reload vimrc anywhere.
nnoremap <F6> :<C-u>vsplit $MYVIMRC<CR>
nnoremap <F7> :<C-u>source $MYVIMRC<CR>
nnoremap <C-n> :<C-u>NERDTreeTabsToggle<CR>
nnoremap <C-m> :<C-u>TagbarToggle<CR>

" Press <Esc> key 2 times, to :noh
nnoremap <silent><Esc><Esc> :noh<CR><Esc>

" incremental and decremental
nnoremap + <C-a>
nnoremap - <C-x>

" run the program
nnoremap <F5> :w<Enter><C-w>w clear<Enter>pulp run<Enter><C-w>w
"====================================================================

" Highlight the cursorline
set cursorline

" show the number of line and offset
set relativenumber number

set showcmd

" Disable mouse and scroll
set mouse=
set hlsearch         " highlight words which are searched
set ai            " set auto-indenting on for programming
set showmatch        " automatically show matching brackets. works like it does in bbedit.
set ruler         " show the cursor position all the time
set tabstop=3        " set tab stop
set shiftwidth=3     " set tab stop of autoindent, for override settings
set cindent
set expandtab       " replace tab with space
set clipboard=unnamed,autoselect    " set clipboard to unnamed to access the system clipboard under windows.

" set incremetnal search
set incsearch

set wildmenu
set wildmode=longest,full

" Change shape of cursor by mode switching.
let &t_SI .= "\e[6 q"
let &t_EI .= "\e[2 q"
let &t_SR .= "\e[4 q"

" Turn off beep sound and screen blinking.
set visualbell t_vb=
set noerrorbells

" window will be displayed to the end without omitiing
set display+=lastline

set scrolloff=2

set undodir=~/.vim/.tmp/undo/
set backupdir=~/.vim/.tmp/backup/
set directory=~/.vim/.tmp/swp/

" Set filetype as another filetype depend on extention
au BufRead,BufNewFile *.ipynb set filetype=json

set encoding=utf-8
set fileencodings=utf-8,euc-jp,sjis,cp932,iso-2022-jp
