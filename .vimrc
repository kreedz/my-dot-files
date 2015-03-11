syntax on
execute pathogen#infect()
filetype plugin indent on
set smartindent
set tabstop=4
set shiftwidth=4
set expandtab
set sessionoptions+=tabpages,globals
set nu
set numberwidth=3
"set relativenumber
set clipboard=unnamed
set clipboard=unnamedplus
set laststatus=2
set nosmd
set noru
set cursorline
set autoread
set hlsearch
set ignorecase
set smartcase
set fileencodings=utf-8,cp1251,koi8-r,cp866
set encoding=utf8
set noerrorbells visualbell t_vb=
"set fdm=indent
set nofoldenable

set t_Co=256
"let g:solarized_termcolors=256
"let g:solarized_termtrans=1
set background=dark
"let g:zenburn_transparent=1
"let g:zenburn_high_Contrast=1
let g:zenburn_old_Visual=1
let g:zenburn_alternate_Visual=1
colorscheme zenburn

" statusline
"set statusline=
"set statusline +=%1*\ %n\ %*            "buffer number
"set statusline +=%5*%{&ff}%*            "file format
"set statusline +=%3*%y%*                "file type
"set statusline +=%4*\ %<%F%*            "full path
"set statusline +=%2*%m%*                "modified flag
"set statusline +=%1*%=%5l%*             "current line
"set statusline +=%2*/%L%*               "total lines
"set statusline +=%1*%4v\ %*             "virtual column number
"set statusline +=%2*0x%04B\ %*          "character under cursorline
"set statusline +=%5*%{strftime(\"%H:%M\")}%*
"hi User1 ctermfg=magenta ctermbg=black
"hi User2 ctermfg=red ctermbg=black
"hi User3 ctermfg=cyan ctermbg=black
"hi User4 ctermfg=green ctermbg=black
"hi User5 ctermfg=yellow ctermbg=black

" plugin indent
let g:indent_guides_auto_colors = 0
let g:indent_guides_enable_on_vim_startup = 1
let g:indent_guides_exclude_filetypes = ['help', 'nerdtree', 'text']
let g:indent_guides_guide_size = 1
autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  guibg=black   ctermbg=black
autocmd VimEnter,Colorscheme * :hi IndentGuidesEven guibg=darkgrey ctermbg=darkgrey

set listchars=tab:»·,trail:·
set list

" pair brackets color
highlight MatchParen cterm=bold ctermfg=cyan

"hi SpecialKey ctermbg=red ctermfg=red guibg=red guifg=red

"if &term =~ '^screen'
    "http://superuser.com/questions/401926/how-to-get-shiftarrows-and-ctrlarrows-working-in-vim-in-tmux
    " tmux will send xterm-style keys when its xterm-keys option is on
    "execute "set <xUp>=\e[1;*A"
    "execute "set <xDown>=\e[1;*B"
    "execute "set <xRight>=\e[1;*C"
    "execute "set <xLeft>=\e[1;*D"
"endif

" maps
"" save file
noremap  <C-S> :update<CR>
vnoremap <C-S> <C-C>:update<CR>
inoremap <C-S> <C-O>:update<CR>

" editing
vnoremap r "_dP
map <Leader>p iimport ipdb; ipdb.set_trace()<ESC>

" buffers
nmap <F5> <esc>:ToggleBufExplorer<cr>
vmap <F5> <esc>:ToggleBufExplorer<cr>
imap <F5> <esc>:ToggleBufExplorer<cr>
nmap <F6> :bp<cr>
vmap <F6> <esc>:bp<cr>i
imap <F6> <esc>:bp<cr>i
nmap <F7> :bn<cr>
vmap <F7> <esc>:bn<cr>i
imap <F7> <esc>:bn<cr>i

nnoremap <CR> :noh<CR><CR>
map <C-q> :bd<CR>

" NERDTree
let g:NERDTreeWinSize=20
map <C-n> :NERDTreeToggle<CR>

" TagBar
nmap <F8> :TagbarToggle<CR>

" AutoComplPop
"let g:acp_enableAtStartup=0

" jedi (python complete)
let g:jedi#popup_select_first = 0
let g:jedi#use_tabs_not_buffers = 0
"let g:jedi#use_splits_not_buffers = "left"
let g:jedi#show_call_signatures = "2"
"let g:jedi#show_function_definition = 0

" airline
set guifont=Liberation\ Mono\ for\ Powerline\ 13
let g:airline_powerline_fonts = 1
let g:airline_theme='zenburn'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tagbar#enabled = 1
let g:airline#extensions#syntastic#enabled = 1
function! AirlineInit()
    let spc = g:airline_symbols.space
    let g:airline_section_z = airline#section#create(['windowswap', '%3p%%'.spc, 'linenr', ':%3v ', '%{strftime("%H:%M")}'])
endfunction
autocmd VimEnter * call AirlineInit()

" bufferline
let g:bufferline_echo = 0
let g:loaded_bufferline = 1

" bufferexplorer
let g:bufexplorer_version = 'disabled'

" syntastic
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
