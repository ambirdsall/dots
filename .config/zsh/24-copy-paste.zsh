# it's just a better clipboard API, tbh; plus muscle memory
if [ $(uname) != 'Darwin' ]; then
  alias pbcopy='clipboard-copy'
  alias pbpaste='clipboard-paste'
fi
