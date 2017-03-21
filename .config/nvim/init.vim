" Specify a directory for plugins
call plug#begin('~/.local/share/nvim/plugged')

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" completitions
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'davidhalter/jedi'
Plug 'zchee/deoplete-jedi'

" syntax
Plug 'neomake/neomake'

" display vertical lines for indent
Plug 'Yggdroot/indentLine'

" colorschemes
Plug 'morhetz/gruvbox'
Plug 'jnurmine/Zenburn'
Plug 'frankier/neovim-colors-solarized-truecolor-only'
Plug 'tomasr/molokai'

" dir tree
Plug 'scrooloose/nerdtree'

" code commentary
Plug 'tpope/vim-commentary'


" Initialize plugin system
call plug#end()


" omnifuncs
augroup omnifuncs
  autocmd!
  autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
  autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
  autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
  autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
  autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
augroup end


" deoplete
let g:deoplete#enable_at_startup = 1
let g:deoplete#sources#jedi#python_path = '/usr/bin/python'

" tab for walking through completition
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
" To close preview window of deoplete automagically
autocmd CompleteDone * pclose
set completeopt-=preview


" neomake
let g:neomake_python_enabled_makers = ['flake8', 'pep8']
let g:neomake_open_list = 0
let g:neomake_echo_current_error = 1

augroup neomake
  autocmd!
  autocmd BufEnter,BufReadPost,BufWritePost *.py Neomake
augroup end


" nerdtree
let NERDTreeIgnore=['\.pyc$', '\~$'] "ignore files in NERDTree


" molokai
" let g:molokai_original = 1
" let g:rehash256 = 1


" highlight python 80 column
" autocmd BufEnter *.py setlocal colorcolumn=80
highlight ColorColumn ctermbg=0 guibg=lightgrey

" autoreload init.vim
" augroup reload_vimrc
"     autocmd!
"     autocmd BufWritePost $MYVIMRC source $MYVIMRC
" augroup END
augroup vimrc     " Source vim configuration upon save
    autocmd! BufWritePost $MYVIMRC source % | echom "Reloaded " . $MYVIMRC | redraw
augroup END

if (has("termguicolors"))
 set termguicolors
endif

syntax enable
filetype plugin indent on
set t_Co=256
set t_ut=
" colorscheme solarized
set background=light
set smartindent
set tabstop=4
set shiftwidth=4
set expandtab
set sessionoptions+=tabpages,globals
" set nu
set numberwidth=3
" switch between buffers without saving them
set hidden
set laststatus=2
set nosmd
set noru
" this causes lag
" set cursorline
set lazyredraw
set autoread
set hlsearch
set ignorecase
set smartcase
set fileencodings=utf-8,cp1251,koi8-r,cp866
set encoding=utf-8
" next command causes blinking on ESC pressing
"set noerrorbells visualbell t_vb=
set nofoldenable
set listchars=tab:»·,trail:·
set list
set clipboard=unnamed
set clipboard=unnamedplus
" pair brackets color
highlight MatchParen cterm=bold ctermfg=cyan


" hotkeys
let mapleader = "\<Space>"
" buffers
nmap <F6> :bp<cr>
vmap <F6> <esc>:bp<cr>i
imap <F6> <esc>:bp<cr>i
nmap <F7> :bn<cr>
vmap <F7> <esc>:bn<cr>i
imap <F7> <esc>:bn<cr>i

nnoremap <CR> :noh<CR><CR>

" fzf
nnoremap <silent> <leader><space> :Files<CR>
nnoremap <silent> <leader>a :Buffers<CR>
nnoremap <silent> <leader>A :Windows<CR>
nnoremap <silent> <leader>? :History<CR>

" nerdtree
map <silent> <F5> :NERDTreeToggle<CR>


" statusline
set statusline=
set statusline +=%1*\ %n\ %*            "buffer number
set statusline +=%5*%{&ff}%*            "file format
set statusline +=%3*%y%*                "file type
set statusline +=%4*\ %<%F%*            "full path
set statusline +=%2*%m%*                "modified flag
set statusline +=%1*%=%5l%*             "current line
set statusline +=%2*/%L%*               "total lines
set statusline +=%1*%4v\ %*             "virtual column number
set statusline +=%2*0x%04B\ %*          "character under cursorline
set statusline +=%5*%{strftime(\"%H:%M\")}%*
hi User1 guifg=magenta guibg=black
hi User2 guifg=red guibg=black
hi User3 guifg=cyan guibg=black
hi User4 guifg=green guibg=black
hi User5 guifg=yellow guibg=black



set keymap=russian-jcukenwin
set iminsert=0
set imsearch=0
highlight lCursor guifg=NONE guibg=Cyan

" this remove flickering, but breaks theme background
"hi Normal guibg=NONE
