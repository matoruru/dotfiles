"dein Scripts-----------------------------
if &compatible
  set nocompatible               " Be iMproved
endif

" Required:
set runtimepath+=/root/.nvim/dein/repos/github.com/Shougo/dein.vim

" Required:
if dein#load_state('/root/.nvim/dein')
  call dein#begin('/root/.nvim/dein')

  " Let dein manage dein
  " Required:

  " Add or remove your plugins here:
  call dein#add('scrooloose/nerdtree')
  call dein#add('jistr/vim-nerdtree-tabs')
  call dein#add('bronson/vim-trailing-whitespace')
  call dein#add('dag/vim-fish')

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
"====================================================================

"==========================Settings of NERDTree======================
" Launch NERDTree when vim stert with vim-nerdtree-tabs
" instead of "autocmd vimenter * NERDTree".
let g:nerdtree_tabs_focus_on_files=1

" Show hidden files in NERDTree explorer window.
let NERDTreeShowHidden=1
"====================================================================

"=======================Settings of tagbar===========================
let g:tagbar_autofocus = 1
"autocmd BufEnter * nested :call tagbar#autoopen(0)
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

" set incremetnal search
set incsearch

" Turn off beep sound and screen blinking.
set visualbell t_vb=
set noerrorbells

" window will be displayed to the end without omitiing
set display=lastline

set noswapfile
set noundofile
set nobackup

tnoremap <ESC> <C-\><C-n>

set hidden
