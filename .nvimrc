" Plugins
call plug#begin('~/.vim/plugged')

"  Theme
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'altercation/vim-colors-solarized'

"  Move
Plug 'unblevable/quick-scope'

"  Input
Plug 'cohama/lexima.vim'
Plug 'gyim/vim-boxdraw'

"  IDE
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" HTML
Plug 'mattn/emmet-vim'

" JavaScript
Plug 'pangloss/vim-javascript'

"  Haskell
Plug 'neovimhaskell/haskell-vim'

"  PureScript
Plug 'purescript-contrib/purescript-vim'

"  Fish
Plug 'dag/vim-fish'

"  Utility
Plug 'simeji/winresizer'
Plug 'bronson/vim-trailing-whitespace'
Plug 'moll/vim-bbye'
Plug 'machakann/vim-highlightedyank'
Plug 'sbdchd/neoformat'
Plug 'vim-scripts/vim-auto-save'

"  Git
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'airblade/vim-gitgutter'

"  File explorer
Plug 'preservim/nerdtree'

call plug#end()


" Set leader key
let mapleader = "\<Space>"


" Theme
let g:airline_theme='solarized'
let g:airline#extensions#tabline#enabled=1
let g:airline#extensions#fugitiveline#enabled=1
let g:airline_powerline_fonts=1
let base16colorspace=256

syntax enable
set background=dark
colorscheme solarized


" Lexima.vim
let g:lexima_enable_basic_rules = 1


" quick-scope
let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']


" vim-auto-save
let g:auto_save = 1
let g:auto_save_silent = 1
let g:auto_save_in_insert_mode = 0


" coc
nnoremap <leader>sd :call <SID>show_documentation()<CR>
nnoremap <leader>ca :CocAction<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction


" neoformat
let g:neoformat_enabled_haskell = ['ormolu']

" see: https://github.com/sbdchd/neoformat/issues/134#issuecomment-347180213
augroup fmt
  autocmd!
  au BufWritePre * try | undojoin | Neoformat | catch /^Vim\%((\a\+)\)\=:E790/ | finally | silent Neoformat | endtry
augroup END


" Haskell
let g:haskell_enable_quantification = 1
let g:haskell_enable_recursivedo = 1
let g:haskell_enable_arrowsyntax = 1
let g:haskell_enable_pattern_synonyms = 1
let g:haskell_enable_typeroles = 1
let g:haskell_enable_static_pointers = 1
let g:haskell_backpack = 1

let g:haskell_indent_if = 2
let g:haskell_indent_case = 2
let g:haskell_indent_let = 2
let g:haskell_indent_where = 2
let g:haskell_indent_before_where = 2
let g:haskell_indent_after_bare_where = 2
let g:haskell_indent_do = 2
let g:haskell_indent_in = 2
let g:haskell_indent_guard = 2


" PureScript
let purescript_indent_if = 2
let purescript_indent_case = 2
let purescript_indent_let = 2
let purescript_indent_where = 2
let purescript_indent_do = 2


" --- NERDTree
function IsInNERDTreeBuf()
  return exists('t:NERDTreeBufName') && bufname("%") == t:NERDTreeBufName
endfunction


" --- Nt
command! Nt :tabnew|term


" --- Bbye
noremap <silent> <leader><C-w> :Bdelete<CR>


" --- vim-highlightedyank
let g:highlightedyank_highlight_duration = 500

" Remap keys
set backspace=0
noremap  <BackSpace> <nop>
noremap! <BackSpace> <nop>
inoremap <BackSpace> <nop>
noremap  <Delete>    <nop>
noremap! <Delete>    <nop>

noremap  <Up>    <nop>
noremap! <Up>    <nop>
noremap  <Down>    <nop>
noremap! <Down>    <nop>
noremap  <Left>    <nop>
noremap! <Left>    <nop>
noremap  <Right>    <nop>
noremap! <Right>    <nop>

noremap <silent> <C-h> <C-w>h
noremap <silent> <C-l> <C-w>l
noremap <silent> <C-j> <C-w>j
noremap <silent> <C-k> <C-w>k

nnoremap Q <nop>

map <C-n> :NERDTreeToggle<CR>

nnoremap <F6> :<C-u>tabnew ~/.nvimrc<CR>
nnoremap <F7> :<C-u>source ~/.nvimrc<CR>
nnoremap <F8> :set relativenumber! number!<CR>

nnoremap <F10> :!code %<CR>

nnoremap <silent> <Esc> :noh<CR><Esc>

nnoremap cp "+p
nnoremap cy "+y
vnoremap cy "+y

nnoremap + <C-a>
nnoremap - <C-x>

tnoremap <ESC> <C-\><C-n>

" Use the pattern to serach the history
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>

" Use the command-line mode with emacs keybindings
cnoremap <C-b> <Left>
cnoremap <C-f> <Right>
cnoremap <C-e> <End>
cnoremap <C-a> <Home>

" Open the command line window
cnoremap <C-g> <C-f>

" Highlight without jumping
noremap * *``
noremap # #``

nnoremap <silent> <expr> ]b IsInNERDTreeBuf() ? '<C-w>l:bn<CR>' : ':bn<CR>'
nnoremap <silent> <expr> [b IsInNERDTreeBuf() ? '<C-w>l:bp<CR>' : ':bp<CR>'
nnoremap <silent> <expr> ]B IsInNERDTreeBuf() ? '<C-w>l:bl<CR>' : ':bl<CR>'
nnoremap <silent> <expr> [B IsInNERDTreeBuf() ? '<C-w>l:bf<CR>' : ':bf<CR>'

nnoremap <silent> ]a :next<CR>
nnoremap <silent> [a :prev<CR>
nnoremap <silent> ]A :last<CR>
nnoremap <silent> [A :first<CR>

nnoremap <silent> ]c :cnext<CR>
nnoremap <silent> [c :cprev<CR>
nnoremap <silent> ]C :clast<CR>
nnoremap <silent> [C :cfirst<CR>

nnoremap <silent> ]e :call CocAction('diagnosticNext')<CR>
nnoremap <silent> [e :call CocAction('diagnosticPrevious')<CR>
nnoremap <silent> ]E :<CR>
nnoremap <silent> [E :<CR>

nnoremap <silent> ]t gt
nnoremap <silent> [t gT
nnoremap <silent> ]T :tabl<CR>
nnoremap <silent> [T :tabfir<CR>

cnoremap <silent> <C-t>h :vs\|term<CR>
cnoremap <silent> <C-t>j :sp<CR><C-w>j:term<CR>
cnoremap <silent> <C-t>k :sp\|term<CR>
cnoremap <silent> <C-t>l :vs<CR><C-w>l:term<CR>


" Basic settings
set cursorline
set relativenumber number

highlight Comment cterm=italic gui=italic

set mouse=
set nowrapscan
set hlsearch
set ai
set showmatch
set ruler
set tabstop=2
set shiftwidth=2
set expandtab

set incsearch
set inccommand=split

let &t_SI .= "\e[6 q"
let &t_EI .= "\e[2 q"
let &t_SR .= "\e[4 q"

set nobackup
set nowritebackup
set noundofile
set noswapfile

set hidden

augroup termopen-startinsert
  autocmd!
  autocmd TermOpen * startinsert
augroup END
