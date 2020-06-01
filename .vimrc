set nocompatible
filetype plugin indent on
runtime macros/matchit.vim
let mapleader=" "

" {{{ Plugins
" Load vim-plug
if empty(glob("~/.vim/autoload/plug.vim"))
    execute '!curl -fLo ~/.vim/autoload/plug.vim https://raw.github.com/junegunn/vim-plug/master/plug.vim'
endif
" {{{ vim-plug set up
call plug#begin('~/.vim/bundle')
" }}}
" {{{ Custom text objects
Plug 'kana/vim-textobj-entire'
Plug 'kana/vim-textobj-indent'
Plug 'kana/vim-textobj-line'
Plug 'kana/vim-textobj-user'
Plug 'nelstrom/vim-textobj-rubyblock'
Plug 'whatyouhide/vim-textobj-erb'
" }}}
" {{{ Custom commands
Plug 'tpope/vim-repeat'
Plug 'christoomey/vim-sort-motion'
Plug 'tommcdo/vim-exchange'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'vim-scripts/ReplaceWithRegister'
Plug 'christoomey/vim-titlecase'
Plug 'ecomba/vim-ruby-refactoring', { 'for': 'ruby' }
" }}}
" {{{ Testing
Plug 'thoughtbot/vim-rspec', { 'for': 'ruby' }
" }}}
" {{{ Navigation
Plug 'ctrlpvim/ctrlp.vim'
Plug 'easymotion/vim-easymotion'
Plug 'tpope/vim-unimpaired'
" }}}
" {{{ Tmux
Plug 'christoomey/vim-tmux-navigator'
" }}}
" {{{ Man/Info Pages
Plug 'vim-utils/vim-man'
Plug 'alx741/vinfo'
" }}}
" {{{ git + vim
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'
" }}}
" {{{ Rails / Ruby Navigation
Plug 'tpope/vim-rails'
Plug 'tpope/vim-rake'
Plug 'tpope/vim-bundler'
" }}}
" {{{ Languages
Plug 'HerringtonDarkholme/yats.vim', { 'for': 'typescript' }
Plug 'leafgarland/typescript-vim', { 'for': 'typescript' }
Plug 'Quramy/vim-js-pretty-template', { 'for': 'typescript' }
Plug 'mhartington/vim-typings', { 'for': 'typescript' }
Plug 'jason0x43/vim-js-indent'
Plug 'vim-ruby/vim-ruby', { 'for': 'ruby' }
Plug 'tpope/vim-markdown', { 'for': 'markdown' }
Plug 'jtratner/vim-flavored-markdown', { 'for': 'markdown' }
Plug 'chrisbra/Colorizer'
Plug 'othree/yajs.vim', { 'for': 'javascript' }
Plug 'othree/javascript-libraries-syntax.vim', { 'for': 'javascript' }
Plug 'briancollins/vim-jst', { 'for': 'jst' }
Plug 'lambdatoast/elm.vim', { 'for': 'elm' }
Plug 'bumaociyuan/vim-swift', { 'for': 'swift' }
Plug 'elixir-lang/vim-elixir', { 'for': 'elixir' }
Plug 'slashmili/alchemist.vim', { 'for': 'elixir' }
Plug 'mattreduce/vim-mix', { 'for': 'elixir' }
" }}}
" {{{ language-specific tagging
Plug 'ramitos/jsctags', { 'for': 'javascript' }
" }}}
" {{{ Typing aids
Plug 'ambirdsall/emmet-vim'
Plug 'christoomey/vim-system-copy'
Plug 'ntpeters/vim-better-whitespace'
Plug 'scrooloose/syntastic'
Plug 'tpope/vim-endwise'
Plug 'alvan/vim-closetag'
" }}}
" {{{ Configuration
Plug 'tpope/vim-projectionist'
" }}}
" {{{ Searching
Plug 'mileszs/ack.vim'
Plug 'rking/ag.vim'
" }}}
" {{{ Colors
Plug 'altercation/vim-colors-solarized'
" }}}
" {{{ Sonic Pi
Plug 'dermusikman/sonicpi.vim', { 'for': 'ruby' }
" }}}
" {{{ Org Mode
Plug 'jceb/vim-orgmode'
" }}}
call plug#end()
" }}}
" {{{ Plugin customization
" {{{ Ack.vim
" fold together all hits from same file in ack results when not under cursor.
let g:ack_autofold_results = 1
" }}}
" {{{ Colorizer
let g:colorizer_auto_filetype='css,scss,sass,html'
" }}}
" {{{ Ctrl-p
let g:ctrlp_custom_ignore = 'node_modules'
" }}}
" {{{ Solarized
let g:solarized_termtrans = 1
" }}}
" {{{ Syntastic
let g:syntastic_check_on_open=0
" let g:syntastic_javascript_checkers = ['~/job/rio/node_modules/.bin/eslint']
let g:syntastic_mode_map = { "mode": "active", "active_filetypes": ["ruby"],
                                            \ "passive_filetypes": ["javascript"] }
" }}}
" {{{ typescript
let g:tsuquyomi_disable_quickfix = 1
let g:syntastic_typescript_checkers = ['tsuquyomi']
if !exists("g:ycm_semantic_triggers")
 let g:ycm_semantic_triggers = {}
endif
let g:ycm_semantic_triggers['typescript'] = ['.']
" }}}
" {{{ Titlecase
" vim-titlecase's default mapping is `gt`, but I use tabs
let g:titlecase_map_keys = 0
nmap tit <Plug>Titlecase
vmap tit <Plug>Titlecase
nmap tiT <Plug>TitlecaseLine
" }}}
" {{{ VimTmuxNavigator
if has('nvim')
  nmap <bs> :<c-u>TmuxNavigateLeft<cr>
endif
"}}}
" }}}
" {{{ Folding
set foldopen=
set foldclose=
set foldlevel=0
" }}}
" {{{ Shell and file settings
set shell=zsh
set noswapfile " that's git's job
" it is okay to change files without saving every damn thing
set hidden
set encoding=utf-8
" }}}
" {{{ UI settings (typing and mouse)
" just because you shouldn't use the mouse doesn't mean you shouldn't be able to
set mouse=a
" ditto deleting shit in insert mode
set backspace=indent,eol,start
" 350ms is enough to finish typing combos even on a bad day, but not toooo long.
set timeoutlen=350
set omnifunc=syntaxcomplete#Complete
" }}}
" {{{ Sessions
nnoremap <leader>ms :mksession!<cr>
nnoremap <leader>sms :source %:p:h/Session.vim<cr>
" }}}
" {{{ Search
set incsearch
set hlsearch
" <leader>h to clear highlighting when it gets annoying
nnoremap <leader>h :noh<cr>
" }}}
" {{{ (js)?Ctags
set tags+=./.git/jstags
" }}}
" {{{ Navigation
" `gf` should work with js require/import directives
set suffixesadd+=.js,.ts
" }}}
" {{{ The Land Of Autocommand
if has("autocmd")
  augroup CursorLine
    au!
    " cursorline messes with hex color highlighting
    au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
    au VimEnter,WinEnter,BufWinEnter * setlocal cursorcolumn
    au WinLeave * setlocal nocursorline
    au WinLeave * setlocal nocursorcolumn
  augroup END

  " automatically rebalance splits on resize
  autocmd VimResized * :wincmd =

  " cron jobs, tho
  autocmd filetype crontab setlocal nobackup nowritebackup

  autocmd filetype gitcommit setlocal textwidth=72

  " [Plug] github-flavored markdown highlighting for all markdown files
  augroup markdown
    au!
    " nice line formatting for free in markdown
    autocmd bufreadpre *.md setlocal textwidth=80
    autocmd bufreadpre *.markdown setlocal textwidth=80
    " au BufNewFile,BufRead *.md,*.markdown setlocal filetype=ghmarkdown
  augroup END

  " Middleman navigation with slim templating. Useful settings for reference,
  " though the selector is obsolete since I'm not at Flock anymore.
  autocmd BufReadPre,BufNewFile */flock-landing/* set sua+=.html.slim
  autocmd BufReadPre,BufNewFile */flock-landing/* set inex=substitute(v:fname,'partials/','partials/_','')

  " automatically save files on focus lost. Theoretically.
  au FocusLost * silent! wa

  " stop that autocomment noise
  autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

  " Automatically source vimrc when saving changes to it.
  autocmd! BufWritePost vimrc source $MYVIMRC

  " When editing a file, always jump to the last known cursor position.
  " Don't do it for commit messages, when the position is invalid, or when
  " inside an event handler (happens when dropping a file on gvim).
  autocmd BufReadPost *
        \ if &ft != 'gitcommit' && line("'\"") > 0 && line("'\"") <= line("$") |
        \   exe "normal g`\"" |
        \ endif
endif
" }}}
" {{{ Functions
function! Git_branch()
  if !exists('b:git_dir')
    return ''
  endif

  return " ".fugitive#head()
endfunction

function! g:ToggleColorColumn()
  if &colorcolumn != ''
    setlocal colorcolumn&
  else
    setlocal colorcolumn=81
  endif
endfunction
nnoremap <silent> <leader>cc :call g:ToggleColorColumn()<cr>

function! s:VSetSearch()
  let temp = @s
  norm! gv"sy
  let @/ = '\V' . substitute(escape(@s, '/|'), '|n', '||n', 'g')
  let @s = temp
endfunction

function! g:AlignAssignments()
  " locate assignment operators
  let ASSIGN_OP   = '[-+*/%|&]\?=\@<!=[=~]\@!'
  let ASSIGN_LINE = '^\(.\{-}\)\s*\(' . ASSIGN_OP . '\)'

  " locate block of code to be considered (same indentation, no blanks)
  " TODO: also match a previous line that is indented one less level if
  "   1) the current line starts with ','
  "   2) said previous line starts with 'var', 'const', or 'let'
  let indent_pat = '^' . matchstr(getline('.'), '^\s*') . '\S'
  let firstline  = search('^\%(' . indent_pat . '\)\@!', 'bnW') + 1
  let lastline   = search('^\%(' . indent_pat . '\)\@!',  'nW') - 1
  if lastline < 0 " i.e. EOF reached before non-matching line
    let lastline = line('$')
  endif

  " find the column at which the operators should be aligned
  let max_align_col = 0
  let max_op_width  = 0
  for linetext in getline(firstline, lastline)
    " is there an assignment?
    let left_width = match(linetext, '\s*' . ASSIGN_OP)

    " if so, track the max assignment column and operator width
    if left_width >= 0
      let max_align_col = max([max_align_col, left_width])

      let op_width      = strlen(matchstr(linetext, ASSIGN_OP))
      let max_op_width  = max([max_op_width, op_width + 1])
    endif
  endfor

  " git 'er done
  let FORMATTER = '\=printf("%-*s%*s", max_align_col, submatch(1),
                                     \ max_op_width,  submatch(2))'

  for linenum in range(firstline, lastline)
    let oldline = getline(linenum)
    let newline = substitute(oldline, ASSIGN_LINE, FORMATTER, "")
    call setline(linenum, newline)
  endfor
endfunction
nnoremap <silent> <leader>= :call g:AlignAssignments()<cr>

" function! g:TagJavascript()
"   execute '! find . -type f -iregex ".*\.js$" -not -path "./node_modules/*" -exec jsctags {} -f \; | sed "/^$/d" | sort > tags'
" endfunction

" insert evaluated code output into buffer
" http://blog.joncairns.com/2014/10/evaluate-ruby-or-any-command-and-insert-into-vim-buffers/
function! InsertCommand(command)
  redir => output
  silent execute a:command
  redir END
  call feedkeys('i'.substitute(output, '^[\n]*\(.\{-}\)[\n]*$', '\1', 'gm'))
endfunction

command! -nargs=+ I call InsertCommand(<q-args>)
" }}}
" {{{ Commands
" Hideous and shitty hack, burn with fire ASAP
" But it brings back colors
command! Wtf source $MYVIMRC <bar> source $MYVIMRC
command! Q mksession! <bar> quitall
" }}}
" {{{ Variables
" Treat <li> and <p> tags like the block tags they are.
let g:html_indent_tags = 'li\|p'
let g:netrw_banner=0
let g:netrw_bufsettings = 'noma nomod nu nobl nowrap ro'
" }}}
" {{{ non-leader mappings
" {{{ Scrolling
" scroll browser-style
noremap <up> <c-y>
noremap <down> <c-e>
" Actually using <c-e> and <c-y> is a sped-up version
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>
nnoremap <left> zh
nnoremap <right> zl
" }}}
" {{{ Yanking
" `Y` yanks from current cursor position to EOL instead of acting like `yy`.
nnoremap Y y$
" }}}
" {{{ Smash Escape
"                         CURSOR POSITION AFTER:
"                       |F| | |J| | <= backwards like esc <=
inoremap jf <esc>
inoremap fj <esc>
inoremap fd <esc>
vnoremap jf <esc>
vnoremap fj <esc>
vnoremap fd <esc>
"                       | | | |J|K| => doesn't move =>
inoremap jk <esc>l
inoremap kj <esc>l
" }}}
" {{{ Insert mode
" {{{ Insert new lines
" quickly insert new line above...
inoremap jj <c-o>o
" ...or below the current one in insert mode.
inoremap uu <c-o>O
" }}}
" }}}
" {{{ Visual mode
" use `*` or `#` in visual mode to search for the selection
xnoremap * :<c-u>call <SID>VSetSearch()<cr>/<c-r>=@/<cr><cr>
xnoremap # :<c-u>call <SID>VSetSearch()<cr>?<c-r>=@/<cr><cr>
" }}}
" {{{ Command mode
" Allow saving of files as sudo when I forgot to start vim using sudo.
cmap w!! w !sudo tee > /dev/null %
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'
" }}}
" }}}
" {{{ <leader> mappings
" {{{ Moving around
nnoremap <leader><leader><leader> <c-^>
nnoremap <leader>f ^
nnoremap <leader>b :buffer
nnoremap <leader>ev :tabe ~/.dotfiles/vimrc<cr>
nnoremap <leader>gf <c-w>f<bar><c-w>L
" quickly jump to inside an empty matched pair (e.g. '()', '""')
nnoremap <leader>in ?\%<c-r>=line('.')<Return>l\({}\\|\[]\\|<>\\|><\\|()\\|""\\|''\\|``\\|><lt>\)?s+1<Return>
" }}}
" {{{ Inserting stuff
nnoremap <leader>O O<esc>
nnoremap <leader>o o<esc>
nnoremap <leader>P "*P
nnoremap <leader>p "*p
" }}}
" {{{ Formatting & Saving
nnoremap <leader>gq gq}
nnoremap <leader>lint :!node_modules/.bin/eslint -c .eslintrc.js %<cr>
nnoremap <leader>r :set wrap!<cr>
nnoremap <leader>tts :%s/\t/  /g<cr>
nnoremap <leader>w :w<cr>
nnoremap <leader>fs :w<cr>
" }}}
" {{{ Folding
" toggle fold under cursor
nnoremap <leader>z za
" }}}
" {{{ Plugins
" {{{ ReplaceWithRegister
nnoremap <leader>gr "*gr
" }}}
" {{{ Vim-fugitive
nnoremap <leader>s :Gstatus<cr>
nnoremap <leader>bl :Gblame<cr>
nnoremap <leader>di :Gdiff
" }}}
" {{{ Vim-rspec
nnoremap <leader>t :call RunCurrentSpecFile()<cr>
nnoremap <leader>n :call RunNearestSpec()<cr>
nnoremap <leader>l :call RunLastSpec()<cr>
" nnoremap <leader>a :call RunAllSpecs()<cr>
" }}}
" {{{ Ag.vim
" The terminal whitespace is on purpose here:
nnoremap <leader>ag :Ag! 
nnoremap <leader>ack :Ack! 
" Don't automatically jump to the first match.
cnoreabbrev Ack Ack!
" }}}
" {{{ AutoClose
nnoremap <leader>ct :AutoCloseToggle<cr>
" }}}
" {{{ Vim-commentary
nmap <leader>c gcc
" }}}
" {{{ Ctags
nnoremap <leader>. :TagbarToggle<cr>
" }}}
" {{{ vim-better-whitespace
nnoremap <leader>sw :StripWhitespace<cr>
" }}}
" {{{ easymotion
nmap <leader>a <Plug>(easymotion-s2)
" }}}
" {{{ typescript
nnoremap <leader>ts :TsuquyomiStartServer<cr>
" }}}
" }}}
" }}}
" " {{{ Statusline
set statusline=%#DiffChange#
set statusline+=\ 
set statusline+=%{Git_branch()}
set statusline+=\ 
set statusline+=\ 
set statusline+=%#DiffAdd#
set statusline+=%f
set statusline+=%m
set statusline+=\ 
set statusline+=%#SignColumn#

set statusline+=%=

set statusline+=%010y
set statusline+=\ 
set statusline+=%l
set statusline+=:
set statusline+=%#DiffText#
set statusline+=%c
set statusline+=%#SignColumn#
set statusline+=\ 
set statusline+=%P
set statusline+=\ 
" " }}}
" {{{ Visual settings: highlighting, linenumbers, indents, splits
syntax enable
set background=dark
colorscheme solarized
" hybrid linenumbers
set number
set relativenumber
set numberwidth=5 " gutter column bigger for readability
set ruler " redundant with powerline installed; left in b/c Wu-Tang is for the
set laststatus=2 " always display status line, even in single buffer
set lazyredraw " don't redraw during macros "
set showcmd
set wildmenu
" 008 comes after 007, not 010.
set nrformats-=octal
set list
set listchars=eol:␤,tab:→·
highlight Search       cterm=reverse ctermfg=66 ctermbg=143
highlight VertSplit    ctermfg=11 ctermbg=232 gui=reverse
highlight StatusLine   cterm=NONE ctermbg=232
highlight StatusLineNC cterm=NONE ctermbg=232
highlight ColorColumn  ctermbg=0 guifg=Cyan guibg=Grey
highlight CursorLine   ctermbg=0 guifg=Cyan guibg=Grey
highlight CursorColumn ctermbg=0 guifg=Cyan guibg=Grey
highlight SignColumn   ctermfg=12 ctermbg=0 guifg=Cyan guibg=Grey " the gutter behind the numbers; where gitgutter signs are missing
highlight LineNr       ctermfg=12 ctermbg=0 guifg=Cyan guibg=Grey     " the line numbers themselves
highlight NonText      ctermfg=237
highlight SpecialKey   ctermfg=237
highlight Folded       ctermbg=NONE
" 2-space indents; <</>> shifting goes to nearest multiple of 2, even from odds.
set tabstop=2
set shiftwidth=2
" split panes spawn below the current pane, b/c Principle of Least Surprise.
set splitbelow
set nowrap " <leader>r toggles wrap for when it's needed
set diffopt+=vertical " always compare diffs with vertical splits.
" }}}

" vim:foldmethod=marker:foldlevel=0
