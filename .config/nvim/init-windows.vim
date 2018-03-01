"Specify a directory for plugins
let mapleader = "\<Space>"

call plug#begin('~/.local/share/nvim/plugged')

set rtp+=~\.fzf
Plug 'junegunn/fzf.vim'

" completitions
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
" Plug 'Shougo/denite.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'davidhalter/jedi-vim'
Plug 'davidhalter/jedi'
Plug 'zchee/deoplete-jedi'

" syntax
Plug 'neomake/neomake'

" display vertical lines for indent
Plug 'Yggdroot/indentLine'

" manage sessions
Plug 'mhinz/vim-startify'

" display λ for lambda, etc.
" Plug 'ehamberg/vim-cute-python'

" colorschemes
Plug 'morhetz/gruvbox'
Plug 'jnurmine/Zenburn'
Plug 'frankier/neovim-colors-solarized-truecolor-only'
Plug 'tomasr/molokai'
Plug 'junegunn/seoul256.vim'

" dir tree
Plug 'scrooloose/nerdtree'

" code commentary
Plug 'tpope/vim-commentary'

" syntax highlighting for vue
" Plug 'posva/vim-vue'

" match tags in html
" Plug 'Valloric/MatchTagAlways'

" match brackets
" Plug 'itchyny/vim-parenmatch'

" Plug 'Shougo/vimproc.vim', {'do' : 'make'}

" ts completition
Plug 'mhartington/nvim-typescript'
" Plug 'Quramy/tsuquyomi'

" syntax hi for html5, js, jsx, ts, tsx
Plug 'othree/html5.vim'
Plug 'othree/yajs.vim'
Plug 'othree/javascript-libraries-syntax.vim'
Plug 'HerringtonDarkholme/yats.vim'
" Plug 'peitalin/vim-jsx-typescript'
" Plug 'pangloss/vim-javascript'
" Plug 'mxw/vim-jsx'
" Plug 'leafgarland/typescript-vim'

" syntax hi for python
Plug 'vim-python/python-syntax'

" refactoring and autoimport
" Plug 'python-rope/ropevim'

" Initialize plugin system
call plug#end()


aug omnifuncs
    au!
    au FileType css setlocal omnifunc=csscomplete#CompleteCSS
    au FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
    au FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
    au FileType python setlocal omnifunc=pythoncomplete#Complete
    au FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
aug end

" set filetypes as typescript.jsx
au BufNewFile,BufRead *.tsx set filetype=typescript.jsx
au FileType javascript,typescript,typescript.jsx,css setl sw=4 sts=4 et

aug filetype_odd_vue
    au!
    " au BufNewFile,BufRead *.vue setlocal filetype=html.javascript.css
    au BufNewFile,BufRead *.vue set ft=html
aug end

" aug vue_ft_sync
"     au!
"     au FileType vue syntax sync fromstart
" aug end


" deoplete
let g:deoplete#enable_at_startup = 1
let g:deoplete#sources#jedi#python_path = 'C:/Python36-32/python3.exe'
let g:python_host_prog = 'C:/Python27/python.exe'
let g:python3_host_prog = 'C:/Python36-32/python3.exe'
let g:jedi#force_py_version = 3

" tab for walking through completition
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
" To close preview window of deoplete automagically
autocmd CompleteDone * pclose
" turn off preview window
set completeopt-=preview


" neomake
let g:neomake_python_enabled_makers = ['flake8']
let g:neomake_typescript_enabled_makers = ['tsc', 'tslint']
let g:neomake_python_pep8_exe = 'pep8'
let g:neomake_open_list = 0
let g:neomake_echo_current_error = 1

let neomake_blacklisted_files = ['settings.py']
aug neomake_autostart
  au!
  au BufRead,BufReadPost,BufWritePost *.py if index(neomake_blacklisted_files, expand('%:t')) < 0 | Neomake
  au BufRead,BufReadPost,BufWritePost *.ts,*.tsx Neomake
  " autocmd BufEnter,BufReadPost,BufWritePost *.py echom "New buffer!"
aug end


" nerdtree
let NERDTreeIgnore=['\.pyc$', '\~$', 'node_modules', '.git']
map <silent> <F4> :NERDTreeFind<CR>
map <silent> <F5> :NERDTreeToggle<CR>
let NERDTreeShowHidden=1


" javascript-libraries-syntax.vim
" let g:used_javascript_libs = 'react'


" MatchTagAlways
let g:mta_filetypes = {'typescript.jsx': 0, 'html' : 1, 'xhtml' : 1, 'xml' : 1}
let g:mta_use_matchparen_group = 0
let g:mta_set_default_matchtag_color = 0
" hi MatchTag guifg=black guibg=lightgreen


" startify
let g:startify_list_order = [
        \ ['   Sessions:'],
        \ 'sessions',
        \ ['   MRU:'],
        \ 'files',
        \ ['   MRU current dir:'],
        \ 'dir',
        \ ['   Bookmarks:'],
        \ 'bookmarks',
        \ ['   Commands:'],
        \ 'commands',
        \ ]
let g:startify_disable_at_vimenter = 1
let g:startify_skiplist = [
        \ 'NERD_tree_*',
        \ ]
function! Ssave()
    :NERDTreeClose
    :SSave
endfunction
command! Ssave call Ssave()

nnoremap <silent> <leader>s :Startify<CR>
" aug nerdtree_start_session
"     au!
"     au VimEnter *
"         \   if !argc()
"         \ |   Startify
"         \ |   NERDTree
"         \ |   wincmd w
"         \ | endif
" aug end


" fzf
nnoremap <silent> <leader><space> :Files<CR>
nnoremap <silent> <leader>a :Buffers<CR>
nnoremap <silent> <leader>A :Windows<CR>
nnoremap <silent> <leader>? :History<CR>
imap <c-x><c-j> <plug>(fzf-complete-file-ag)


" vim jsx
let g:jsx_ext_required = 0


" python-syntax
let g:python_highlight_all = 1
" let g:python_highlight_class_vars=1


" disable match paren
" let g:loaded_matchparen=1


" vim-jsx-typescript
" set jsx-tag colors
" hi xmlTagName ctermfg=45
" hi xmlTag ctermfg=20


" ropevim
" let g:ropevim_autoimport_modules = ["os", "django"]


" molokai
" let g:molokai_original = 1
" let g:rehash256 = 1


" highlight python,js,ts 80 column
augroup py_js_ts_80column
    autocmd!
    autocmd BufRead *.py,*.js,*.ts,*.jsx,*.tsx setlocal colorcolumn=80
augroup end
highlight ColorColumn ctermbg=0 guibg=lightgrey

" autoreload init.vim
augroup vimrc
    autocmd! BufWritePost $MYVIMRC source % | echom "Reloaded " . $MYVIMRC | redraw
augroup END


if (has("termguicolors"))
    set termguicolors
endif

syntax enable
filetype plugin indent on
language messages C
let $LANG = 'en'
set t_Co=256
set t_ut=
colorscheme solarized
set background=light
set smartindent
set autoindent
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
set timeoutlen=200
" pair brackets color
highlight MatchParen cterm=bold guifg=orange guibg=grey


" hotkeys
" buffers
nmap <F6> :bp<cr>
vmap <F6> <esc>:bp<cr>i
imap <F6> <esc>:bp<cr>i
nmap <F7> :bn<cr>
vmap <F7> <esc>:bn<cr>i
imap <F7> <esc>:bn<cr>i

"" save file
noremap  <C-S> :update<CR>
vnoremap <C-S> <C-C>:update<CR>
inoremap <C-S> <C-O>:update<CR>

" editing
vnoremap r "_dP
map <Leader>p iimport ipdb; ipdb.set_trace()<ESC>
" stop hi
nnoremap <CR> :noh<CR>
" insert new line without leaving normal mode
nmap oo o<Esc>
" forward-delete in insert-mode 
inoremap <C-d> <Del>


" statusline
set statusline=
set statusline +=%6*\ %n\ %*                    " buffer number
set statusline +=%5*%{&ff}%*                    " file format
set statusline +=%3*%y%*                        " file type
set statusline +=%4*\ %<%F%*                    " full path
set statusline +=%2*%m%*                        " modified flag
" set statusline +=%1*%=%5l%*                   " current line
set statusline +=%1*%=%1*%L%*                   " total lines
set statusline +=%5*\ %P%*                      " percentage current line
set statusline +=%1*%4v\ %*                     " virtual column number
" set statusline +=%2*0x%04B\ %*                " character under cursorline
set statusline +=%5*%{strftime(\"%H:%M\")}%*    " current time
hi User1 guifg=grey guibg=black
hi User2 guifg=white guibg=black
hi User3 guifg=grey guibg=black
hi User4 guifg=grey guibg=black
hi User5 guifg=grey guibg=black
hi User6 guifg=grey guibg=black

" indicate insert mode
augroup indicate_insert
    autocmd!
    au InsertEnter * hi User6 guibg=orange
    au InsertLeave * hi User6 guibg=black
augroup end


set keymap=russian-jcukenwin
set iminsert=0
set imsearch=0
highlight lCursor guifg=NONE guibg=Cyan
inoremap <C-l> <C-^>
