cdc () {
  if [[ $# -gt 0 ]]; then
    cdd ~/c/${@}
  else
    cdd ~/c
  fi
}
compdef '_files -W ~/c' cdc
tnc () {
  cdd ~/c/${@} && tn
}
compdef '_files -W ~/c' tnc


cd. () {
  if [[ $# -gt 0 ]]; then
    cdd ~/.config/${@}
  else
    cdd ~/.config
  fi
}
compdef '_files -W ~/.config' cd.

e. () {
  if [[ $# -gt 0 ]]; then
    e ~/.config/${@}
  else
    e ~/.config
  fi
}
compdef '_files -W ~/.config' e.
