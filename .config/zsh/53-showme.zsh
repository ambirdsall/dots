if dots/at_hand bat; then
  showme () {
    bat $(which $1)
  }
else
  showme () {
    cat $(which $1)
  }
fi
