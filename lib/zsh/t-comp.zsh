# zsh completion for `t` (~/bin/t). Auto-sourced by ~/.config/zsh/03-completion.zsh;
# elsewhere, `source <(t --zsh)`.

# `t` draws candidates from four different places, so keep them in separate,
# labelled groups in the order they are added below instead of one pooled,
# alphabetised list. Scoped to `t`, so global completion styles are untouched.
zstyle ':completion:*:*:t:*' group-name ''
zstyle ':completion:*:*:t:*:descriptions' format '-- %d --'

# --- The two dynamic sources of session names -------------------------------

_t_session_names() {
  # Empty when no server is running; tmux chirps to stderr, so silence it.
  command tmux list-sessions -F '#S' 2>/dev/null
}

_t_layout_names() {
  local dir=${T_SESSIONS_DIR:-$HOME/.tmux/sessions}
  print -l -- $dir/*.janet(N:t:r)
}

_t_complete_sessions() {
  local -a sessions=( ${(f)"$(_t_session_names)"} )
  (( $#sessions )) && _describe -t sessions 'active session' sessions
}

_t_complete_layouts() {
  local -a layouts=( ${(f)"$(_t_layout_names)"} )
  local -a sessions=( ${(f)"$(_t_session_names)"} )
  # A layout that is already up belongs under 'active session', not here.
  layouts=( ${layouts:|sessions} )
  (( $#layouts )) && _describe -t layouts 'session layout' layouts
}

# --- Static and delegated command lists -------------------------------------

_t_complete_subcommands() {
  local -a subcommands=(
    'up:start the autostart layouts, then attach'
    'new:create a session (from its layout, if any) and attach'
    'kill:kill a running session'
    'ls:list running sessions'
    'list-sessions:list running sessions'
  )
  _describe -t subcommands 't subcommand' subcommands
}

typeset -ga _t_tmux_command_cache

_t_complete_tmux_commands() {
  if (( ! $#_t_tmux_command_cache )); then
    local out=$(command tmux list-commands \
                  -F '#{command_list_name} #{command_list_alias}' 2>/dev/null)
    local -a tokens=( ${=out} )
    # Keep only command-shaped tokens: tmux older than 3.2 ignores -F here and
    # prints usage lines, which would otherwise land in the completion list.
    _t_tmux_command_cache=( ${(M)tokens:#[a-z][a-z0-9-]#} )
  fi
  (( $#_t_tmux_command_cache )) &&
    _describe -t tmux-commands 'tmux command' _t_tmux_command_cache
}

_t_delegate_tmux() {
  (( $+_comps[tmux] )) || return 1
  # Re-frame the line as `tmux ...` and hand it to zsh's stock tmux completion,
  # so pass-through subcommands get version-accurate flags and targets.
  local -a rewritten=( tmux "${words[@]}" )
  local -i pos=$(( CURRENT + 1 ))
  local -a words=( "${rewritten[@]}" )
  local -i CURRENT=$pos
  "$_comps[tmux]"
}

# --- Per-position completion ------------------------------------------------

_t_complete_first() {
  _t_complete_sessions
  _t_complete_layouts
  _t_complete_subcommands
  _t_complete_tmux_commands
}

_t_complete_up() {
  # `t up [-d | <session>]`: -d starts the layouts without attaching, otherwise
  # the argument names the session to attach to once they are up.
  local -a flags=( '-d:start the autostart layouts without attaching' )
  _describe -t options 'option' flags
  _t_complete_layouts
  _t_complete_sessions
}

_t() {
  local curcontext=$curcontext state line
  typeset -A opt_args

  _arguments -C \
    '(- *)--zsh[print this completion script]' \
    '1:command:_t_complete_first' \
    '*::arg:->args'

  [[ $state == args ]] || return

  case $words[1] in
    up)   (( CURRENT == 2 )) && _t_complete_up ;;
    new)  (( CURRENT == 2 )) && { _t_complete_layouts; _t_complete_sessions } ;;
    kill) (( CURRENT == 2 )) && _t_complete_sessions ;;
    # `t ls` is an alias t defines itself; everything else is passed to tmux.
    ls)   words[1]=list-sessions; _t_delegate_tmux ;;
    *)    _t_delegate_tmux ;;
  esac
}

compdef _t t
