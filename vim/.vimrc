"  Default ~/.vimrc for vagrant user on Arch Linux.

" Vundle
" {{{
set nocompatible              " be iMproved, required
filetype off                  " required
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" Let Vundle manage itself.
Plugin 'gmarik/Vundle.vim'
Plugin 'vim-airline/vim-airline'
" Color schemes
Plugin 'tomasr/molokai'
Plugin 'chriskempson/base16-vim'
Plugin 'dracula/vim', { 'name': 'dracula' }

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" }}}

" Airline
" {{{
let g:airline_powerline_fonts = 1
set laststatus=2
" }}}

" Colors
" {{{
set t_Co=256
colorscheme dracula
syntax enable                   "syntax highlighting
" }}}

" UI
" {{{
set ruler                       "show the cursor position all the time
set showcmd                     "display incomplete commands
set nu                          "show line numbers
set cmdheight=1                 "The commandbar height
set splitbelow                  " Horizontal splits open below current file
set splitright                  " Vertical splits open to the right of the current file
set wildmode=longest,list       " Pressing <Tab> shows command suggestions similar to pressing <Tab> in bash 
" }}}

" Moving
" {{{
set backspace=indent,eol,start  "allow backspacing over everything in insert mode
" }}}

" Search
" {{{
set incsearch                   "do incremental searching
set hlsearch                    "highlight search terms
set ic                          "Ignore Case during searches
set showmatch                   "Show matching bracets when text indicator is over them
" }}}

" Whitespaces
" {{{
set expandtab                   "use spaces instead of tabs
set tabstop=4                   "insert 4 spaces whenever the tab key is pressed
set shiftwidth=4                "set indentation to 4 spaces
set autoindent                  "start new line at the same indentation level
" }}}


" Misc
" {{{
set history=50                  "keep 50 lines of command line history
set nobackup                    " do not keep backup files, it's 70's style cluttering
set noswapfile                  " do not write annoying intermediate swap files,
set ttimeoutlen=50              "Solves: there is a pause when leaving insert mode
" }}}
