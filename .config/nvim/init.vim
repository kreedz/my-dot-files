" Specify a directory for plugins
call plug#begin('~/.local/share/nvim/plugged')

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

Plug 'davidhalter/jedi'

Plug 'zchee/deoplete-jedi'

Plug 'neomake/neomake'

Plug 'Yggdroot/indentLine'

Plug 'morhetz/gruvbox'

Plug 'scrooloose/nerdtree'

Plug 'tpope/vim-commentary'

" Initialize plugin system
call plug#end()


" deoplete.vim
let g:deoplete#enable_at_startup = 1
let g:deoplete#sources#jedi#python_path = '/usr/bin/python'

autocmd CompleteDone * pclose " To close preview window of deoplete automagically


" neomake
let g:neomake_python_enabled_makers = ['flake8', 'pep8']
let g:neomake_open_list = 0
let g:neomake_echo_current_error = 1

autocmd! BufEnter,BufReadPost,BufWritePost *.py Neomake


" this breaks my colors
if (has("termguicolors"))
 "set termguicolors
endif


syntax enable
filetype plugin indent on
set t_Co=256
colorscheme gruvbox
set smartindent
set tabstop=4
set shiftwidth=4
set expandtab
set sessionoptions+=tabpages,globals
set nu
set numberwidth=3
" switch between buffers without saving them
set hidden
set laststatus=2
set nosmd
set noru
" this causes lag
" set cursorline
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
" buffers
nmap <F6> :bp<cr>
vmap <F6> <esc>:bp<cr>i
imap <F6> <esc>:bp<cr>i
nmap <F7> :bn<cr>
vmap <F7> <esc>:bn<cr>i
imap <F7> <esc>:bn<cr>i

nnoremap <CR> :noh<CR><CR>


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
hi User1 ctermfg=magenta ctermbg=black
hi User2 ctermfg=red ctermbg=black
hi User3 ctermfg=cyan ctermbg=black
hi User4 ctermfg=green ctermbg=black
hi User5 ctermfg=yellow ctermbg=black

set keymap=russian-jcukenwin
set iminsert=0
set imsearch=0
highlight lCursor ctermfg=NONE ctermbg=Cyan
