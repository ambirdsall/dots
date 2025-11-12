if [ -d ~/.deno ]; then
 export DENO_INSTALL="~/.deno"
 export PATH="$DENO_INSTALL/bin:$PATH"
fi
