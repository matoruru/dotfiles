" --- Plugins
call plug#begin('~/.vim/plugged')

" --- --- Theme
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'altercation/vim-colors-solarized'

" --- --- Input
Plug 'cohama/lexima.vim'

" --- --- IDE
Plug 'vim-scripts/vim-auto-save'
Plug 'w0rp/ale'

" --- --- PureScript
Plug 'purescript-contrib/purescript-vim'

" --- --- Utility
Plug 'simeji/winresizer'
Plug 'bronson/vim-trailing-whitespace'

call plug#end()


" --- Theme
set background=dark
colorscheme solarized
let g:airline_theme='solarized'
let g:airline#extensions#tabline#enabled=1
let g:airline#extensions#fugitiveline#enabled=1
let g:airline_powerline_fonts=1


" --- Lexima.vim
let g:lexima_enable_basic_rules = 1


" --- Auto save
let g:auto_save = 1
let g:auto_save_silent = 1


" --- ALE
let g:ale_completion_enabled = 1
let g:ale_set_loclist = 0
let g:ale_set_quickfix = 1
let g:ale_open_list = 1
set omnifunc=ale#completion#OmniFunc


" --- PureScript
let purescript_indent_if = 2
let purescript_indent_case = 2
let purescript_indent_let = 2
let purescript_indent_where = 2
let purescript_indent_do = 2
let purescript_indent_in = 2
let purescript_indent_dot = v:true


" --- Remap keys
set backspace=0
noremap   <BackSpace> <nop>
noremap!  <BackSpace> <nop>
inoremap  <BackSpace> <nop>
noremap   <Delete>    <nop>
noremap!  <Delete>    <nop>

noremap  <C-h> <C-w>h
noremap  <C-l> <C-w>l
noremap  <C-j> <C-w>j
noremap  <C-k> <C-w>k

nnoremap Q <nop>

noremap : q:i
noremap / q/i

nnoremap <C-n> :Ex<CR>

nnoremap <F6> :<C-u>tabnew ~/.nvimrc<CR>
nnoremap <F7> :<C-u>source ~/.nvimrc<CR>
nnoremap <F8> :set relativenumber! number!<CR>

nnoremap <silent> <Esc> :noh<CR><Esc>

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

nnoremap ]c :cnext<CR>
nnoremap [c :cprev<CR>
nnoremap ]C :clast<CR>
nnoremap [C :cfirst<CR>

nnoremap ]t gt
nnoremap [t gT
nnoremap ]T :tabl<CR>
nnoremap [T :tabfir<CR>


" --- Basic settings
set cursorline
set relativenumber number

set mouse=
set hlsearch
set ai
set showmatch
set ruler
set tabstop=2
set shiftwidth=2
set expandtab

set incsearch

let &t_SI .= "\e[6 q"
let &t_EI .= "\e[2 q"
let &t_SR .= "\e[4 q"

set nobackup
set noundofile
set noswapfile

set visualbell t_vb=
set novisualbell
set noerrorbells

set hidden

autocmd TermOpen * startinsert
