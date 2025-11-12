# it's just a better clipboard API, tbh; plus muscle memory
if [ $OS_TYPE != "macos" ]; then
  alias pbcopy='clipboard-copy'
  alias pbpaste='clipboard-paste'
fi
