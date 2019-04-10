"dein Scripts-----------------------------
if &compatible
  set nocompatible               " Be iMproved
endif

" Required:
set runtimepath+=~/.nvim/dein/repos/github.com/Shougo/dein.vim

" Required:
if dein#load_state('~/.nvim/dein')
  call dein#begin('~/.nvim/dein')

  " Let dein manage dein
  " Required:

  " Add or remove your plugins here:
  call dein#add('Shougo/neosnippet.vim')
  call dein#add('Shougo/neosnippet-snippets')
  call dein#add('tpope/vim-fugitive')
  call dein#add('vim-airline/vim-airline')
  call dein#add('vim-airline/vim-airline-themes')
  call dein#add('altercation/vim-colors-solarized')
  call dein#add('morhetz/gruvbox')
  call dein#add('vim-scripts/surround.vim')
  call dein#add('cohama/lexima.vim')
  call dein#add('simeji/winresizer')
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
  call dein#add('dhruvasagar/vim-table-mode')
  call dein#add('iamcco/markdown-preview.nvim', {'on_ft': ['markdown', 'pandoc.markdown', 'rmd'],
					\ 'build': 'cd app & yarn install' })

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

"======================Settings of lexima.vim========================
let g:lexima_enable_basic_rules = 1
"====================================================================

"======================Settings of purescript========================
let purescript_indent_if = 3
let purescript_indent_case = 3
let purescript_indent_let = 3
let purescript_indent_where = 3
let purescript_indent_do = 3
let purescript_indent_in = 3
let purescript_indent_dot = v:true
"====================================================================

"============================close tag===============================
let g:closetag_filenames = "*.html,*.xhtml,*.phtml,*.jsx,*.js"
let g:closetag_xhtml_filenames = '*.xhtml,*.jsx,*.js'
let g:closetag_emptyTags_caseSensitive = 1
let g:closetag_shortcut = '>'
let g:closetag_close_shortcut = '<leader>>'
"====================================================================

"============================Markdown settings=======================
let g:table_mode_corner='|'
let g:mkdp_auto_start = 1
"===================================================================

"=============================Remap keys=============================
set backspace=0
noremap  <BackSpace> <nop>
noremap! <BackSpace> <nop>
inoremap <BackSpace> <nop>
inoremap <BackSpace> <nop>
noremap  <Delete>    <nop>
noremap! <Delete>    <nop>

noremap  <C-h> <C-w>h
noremap  <C-l> <C-w>l
noremap  <C-j> <C-w>j
noremap  <C-k> <C-w>k

" Disable ex mode when press Q.
nnoremap Q <nop>

noremap : q:i

nnoremap <C-n> :Ex<CR>

" Open and edit and reload vimrc anywhere.
nnoremap <F6> :<C-u>tabnew $MYVIMRC<CR>
nnoremap <F7> :<C-u>source $MYVIMRC<CR>

" Press <Esc> to :noh
nnoremap <silent> <Esc> :noh<CR><Esc>

" incremental and decremental
nnoremap + <C-a>
nnoremap - <C-x>

tnoremap <ESC> <C-\><C-n>

nnoremap ]b :bn<CR>
nnoremap [b :bp<CR>
nnoremap ]B :bl<CR>
nnoremap [B :bf<CR>

nnoremap ]a :next<CR>
nnoremap [a :prev<CR>
nnoremap ]A :last<CR>
nnoremap [A :first<CR>

nnoremap ]t gt
nnoremap [t gT
nnoremap ]T :tabl<CR>
nnoremap [T :tabfir<CR>
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
set expandtab       " replace tab with space

" set incremetnal search
set incsearch

" Change shape of cursor by mode switching.
let &t_SI .= "\e[6 q"
let &t_EI .= "\e[2 q"
let &t_SR .= "\e[4 q"

set undodir=~/.nvim/.tmp/undo/
set backupdir=~/.nvim/.tmp/backup/
set directory=~/.nvim/.tmp/swp/

set visualbell t_vb=
set novisualbell
set noerrorbells

" Set filetype as another filetype depend on extention
augroup open-ipynb-asjson
  autocmd!
  autocmd BufRead,BufNewFile *.ipynb set filetype=json
augroup END

set encoding=utf-8
set fileencodings=utf-8,euc-jp,sjis,cp932,iso-2022-jp

" It makes buffers useful
set hidden

" It road the file automatically when it was updated
augroup nvimrc-checktime
  autocmd!
  autocmd CursorMoved * if !bufexists("[Command Line]") | checktime | endif
augroup END


augroup terminsert
  autocmd!
  autocmd TermOpen * startinsert
augroup END

let g:netrw_banner=0
