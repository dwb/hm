let s:this_dir=expand('<sfile>:p:h')

augroup Vimrc
autocmd!

if $SHELL =~ 'bin/fish'
  set shell=/bin/sh
endif

let mapleader = " "
let maplocalleader = "\\"

set autoread

set noswapfile

set lazyredraw

if has("termguicolors")
  set termguicolors
endif

" Wrap too long lines
set wrap
set lbr " wrap by word
set breakat=\ 
set display=lastline " show partial lines
if exists("&breakindent")
  set breakindent
  set breakindentopt=shift:2
endif

" Tabs are 2 characters
set tabstop=2

" (Auto)indent uses 2 characters
set shiftwidth=2

" spaces instead of tabs
set expandtab

" guess indentation
set autoindent
set copyindent
set preserveindent

" Expand the command line using tab
set wildchar=<Tab>

" show line numbers
set number
set relativenumber

set ignorecase
set smartcase

" visual bell
set vb

" Fold using markers {{{
" like this
" }}}
set foldmethod=marker

" enable all features
set nocompatible

" powerful backspaces
set backspace=indent,eol,start

" highlight the searchterms
set hlsearch

" jump to the matches while typing
set incsearch

" don't wrap words
set textwidth=0

" history
set history=50

" 1000 undo levels
set undolevels=1000

" status line
set statusline=%t\ %m\ %y
if exists("*SyntasticStatusLineFlag")
  set statusline+=\ %#warningmsg#%{SyntasticStatuslineFlag()}%*
endif
if exists("*fugitive#statusline")
  set statusline+=\ %{fugitive#statusline()}
endif
" set statusline+=\ %{Tlist_Get_Tag_Prototype_By_Line()}
set statusline+=%=[\%03.3b\ 0x%02.2B]\ [%l/%L,%03v][%p%%]
set laststatus=2
set noshowmode

if has('nvim-0.4')
  set wildoptions=pum
  set pumblend=20
endif

" show partial commands
set showcmd

" show matching braces
set showmatch

" don't leave backup files lying around after vim has closed
set nobackup
" workaround for `crontab -e` not working
set backupskip=/tmp/*,/private/tmp/*

" hide buffers rather than closing them
set hidden

" how hidden characters show on screen
set listchars=tab:⇥\ ,trail:·

" don't insert comment leader when using 'o'/'O'
set formatoptions-=o

" don't further indent in new lines with unclosed parentheses
set cinoptions="(0J1j1"

set updatetime=750

" Prevent Vim from clobbering the scrollback buffer. See
" http://www.shallowsky.com/linux/noaltscreen.html
set t_ti= t_te=

set wildignore+=.git/**,.svn/**,.vagrant/**,.ropeproject/**,*.pyc,*.o,*.beam,*.pyc,*.pyo
set wildignore+=*.png,*.mng,*.jpg,*.jpeg,*.gif,*.bmp,*.pdf,*.class,*.hi,**/tmp/**,**/node_modules/**,**/public/system/**,**/vendor/**,*.egg-info/**
if filereadable("setup.py")
  set wildignore+=**/build/**,**/dist/**
endif

set sessionoptions=blank,buffers,curdir,folds,globals,localoptions,slash,tabpages,winsize

if v:version >= 703
  set undofile
  set undodir=~/.cache/vimundo
endif

filetype off
filetype plugin indent on

syntax on

if has("gui_running")
  set columns=85
  set lines=55
endif


set t_Co=256
set bg=dark
set cursorline

color morning
set colorcolumn=80

" GVIM stuff
set guioptions-=T " no toolbar
set guioptions-=m " no menu
" on separate lines due to MacVim bug:
set guioptions-=r " no right scrollbar
set guioptions-=l " no left scrollbar
set guioptions-=L " also no left scrollbar
set guioptions-=b " no bottom scrollbar
set guifont=PragmataPro:h12

" enable mouse support
set mouse=a

" Always show the menu, insert longest match
set completeopt=menuone,longest


if has('nvim')
  " live-updating search/replace
  set inccommand=split

  function! s:MaybeEnableShada()
    if filereadable('Session.vim')
      set shada+=n.nvim.shada
    endif
  endfunction

  autocmd BufEnter,VimLeavePre * call s:MaybeEnableShada()
endif


""" Little behaviour fixes """

" When editing a file, always jump to the last known cursor position.
" Don't do it when the position is invalid or when inside an event handler
" (happens when dropping a file on gvim).
autocmd BufReadPost *
  \ if &ft != 'gitcommit' && line("'\"") > 0 && line("'\"") <= line("$") |
  \   exe "normal g`\"" |
  \ endif

""" File type specific """

autocmd FileType ruby setlocal iskeyword+=!,?
autocmd FileType eruby setlocal iskeyword+=!,?

""" Maps """

autocmd BufNewFile,BufReadPost,BufWritePost *
  \ if &buftype == '' |
  \   nnoremap <buffer> <Enter> :|
  \ endif

vnoremap s "_s
nnoremap S "_S
nnoremap x "_x
vnoremap x "_x
nnoremap X "_X
nnoremap Y y$
nnoremap Q @@
nnoremap gz '[V']
nnoremap gy g$r<CR>
nnoremap gA g$a
nnoremap <silent> { :keepjumps normal! {<CR>
nnoremap <silent> } :keepjumps normal! }<CR>

" change word under cursor, repeatable
nnoremap c* *``cgn
nnoremap c# #``cgN

" erases and edits the current line, adding two blank lines around it
nnoremap gO O<Esc>jo<Esc>k"_S
imap <C-_> <Esc>gO

nnoremap <C-J> 5<C-E>
nnoremap <C-K> 5<C-Y>

" Move lines up and down...
nnoremap <silent> <Up> :move -2<CR>==
nnoremap <silent> <Down> :move +1<CR>==

nnoremap - "_
vnoremap - "_

vnoremap <Tab> >gv
vnoremap <S-Tab> <gv

augroup end " Vimrc
