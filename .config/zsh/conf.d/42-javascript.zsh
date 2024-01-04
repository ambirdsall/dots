check_node_version() {
	# When a .nvmrc file in the current directory exists ..
	if [[ "$IGNORE_NODE_VERSION" != "1" && -f ".nvmrc" ]]; then
		nvmrc=`cat .nvmrc`
		# .. check whether the current node version is the required one
		if [ ! -x "$(command -v node)" ] || [ `node -v` != `nvm version $nvmrc` ]; then
			# If not use nvm to set the version to the one from .node-version
			echo -e "$fg_bold[yellow][.nvmrc] Switching to $nvmrc$reset_color"
			nvm ls "$nvmrc" &> /dev/null
			if [ $? -eq 0 ]; then
				nvm use "$nvmrc"
			else
				nvm install "$nvmrc"
			fi
			if [ $? -eq 0 ]; then
				echo -e "$fg_bold[green]Done!$reset_color"
			else
				echo -e "$fg_bold[red]Failed!$reset_color"
			fi
		fi
	fi
}

if [ -f "/usr/share/nvm/init-nvm.sh" ]; then
	source /usr/share/nvm/init-nvm.sh
	[ -f /usr/share/nvm/bash_completion ] && source /usr/share/nvm/init-nvm.sh
	add-zsh-hook precmd check_node_version
elif [ -f "$HOME/.nvm/nvm.sh" ]; then
	source "$HOME/.nvm/nvm.sh"

	if [ -f "$HOME/.nvm/bash_completion" ]; then
	 source "$HOME/.nvm/bash_completion"
	elif [ -f "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ]; then
		source "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"
	fi
	add-zsh-hook precmd check_node_version
fi

alias scripts='jq .scripts < package.json'
alias s=scripts
run-npm-script () {
  local run=$(if test -f pnpm-lock.yaml; then echo pnpm; else echo npm run; fi)

  # using separate sed invocations instead of something simpler like `tr -d` to
  # avoid mangling script keys containing colons
  local chosen_script=$(scripts | grep -Eo '".+": ' | sed 's/"//' | sed 's/": //' | fzf)

  test -n $chosen_script && $run $chosen_script
}
alias S=run-npm-script
