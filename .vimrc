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
"path of the current file
set laststatus=2
set statusline+=%F
set cursorline
set autoread
set hlsearch
set ignorecase
set smartcase
set fileencodings=utf-8,cp1251,koi8-r,cp866
set encoding=utf8

set t_Co=256
"let g:solarized_termcolors=256
"let g:solarized_termtrans=1
set background=dark
"let g:zenburn_transparent=1
"let g:zenburn_high_Contrast=1
let g:zenburn_old_Visual=1
let g:zenburn_alternate_Visual=1
"colorscheme xoria256
colorscheme zenburn

" plugin indent
let g:indent_guides_auto_colors = 0
let g:indent_guides_enable_on_vim_startup = 1
let g:indent_guides_exclude_filetypes = ['help', 'nerdtree']
let g:indent_guides_guide_size = 1
autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  guibg=black   ctermbg=black
autocmd VimEnter,Colorscheme * :hi IndentGuidesEven guibg=darkgrey ctermbg=darkgrey

set listchars=tab:»·,trail:·
set list

set hidden
set switchbuf=usetab,newtab

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
"" tabs
"nnoremap <C-S-Tab> :tabprevious<CR>
"nnoremap <C-Tab>   :tabnext<CR>
nnoremap <C-t>     :tabnew<CR>
"nnoremap <C-w>     :tabclose<CR>
nnoremap <S-h> gT
nnoremap <S-l> gt
nnoremap th  :tabfirst<CR>
nnoremap tj  :tabnext<CR>
nnoremap tk  :tabprev<CR>
nnoremap tl  :tablast<CR>
nnoremap tt  :tabedit<Space>
nnoremap tn  :tabnext<Space>
nnoremap tm  :tabm<Space>
nnoremap td  :tabclose<CR>

"" save file
noremap  <C-S>          :update<CR>
vnoremap <C-S>         <C-C>:update<CR>
inoremap <C-S>         <C-O>:update<CR>

" editing
vnoremap r "_dP

" buffers
"nmap <F5> :ls<CR>
"nmap <F7> :bn<CR>
"nmap <F8> :bp<CR>
nmap <F5> <Esc>:ToggleBufExplorer<cr>
vmap <F5> <esc>:ToggleBufExplorer<cr>
imap <F5> <esc>:ToggleBufExplorer<cr>
" F6 - предыдущий буфер
nmap <F6> :bp<cr>
vmap <F6> <esc>:bp<cr>i
imap <F6> <esc>:bp<cr>i
" F7 - следующий буфер
nmap <F7> :bn<cr>
vmap <F7> <esc>:bn<cr>i
imap <F7> <esc>:bn<cr>i

" NERDTree
let g:NERDTreeWinSize=20
map <C-n> :NERDTreeToggle<CR>

" TagBar
nmap <F8> :TagbarToggle<CR>
