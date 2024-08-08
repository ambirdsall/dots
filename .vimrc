" Defaults not covered by sensible.vim (cf ~/.vim/bin/init)
set nohlsearch
set noswapfile
set mouse=a

" Set leader key to space
let mapleader = "\<Space>"

" Save current buffer
nnoremap <leader>fs :w<CR>

" Navigate window splits
nnoremap <leader>wh <C-w>h
nnoremap <leader>wj <C-w>j
nnoremap <leader>wk <C-w>k
nnoremap <leader>wl <C-w>l

" Edit parent directory of current buffer
nnoremap <leader>. :e %:h<CR>

" toggle between buffers
nnoremap <leader>` :b#<CR>

" Plugin config
map <C-;> <Plug>(easymotion-sn)
