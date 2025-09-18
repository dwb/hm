### AUTO SCREEN ###

if (( $SHLVL == 1 )) && (( $+commands[screen] )) \
  && [[ -z $STY && -r ~/.config/cool.dani/autoscreen ]]; then
  exec screen -DR
fi

### ENV CUSTOMISATION ###

export VISUAL=$EDITOR
export PAGER="less -r"
export READNULLCMD=less
export CLICOLOR=1
export LS_COLORS=''
export RSYNC_RSH=ssh
export MOST_SWITCHES="-w"

### END ENV CUSTOMISATION ###
#
### MAYBE AUTOEXEC NUSHELL ###

if (( $+commands[nu] )) && \
   [[ -o login ]] && \
   [[ -o interactive ]] && \
   [[ ! -o shinstdin ]] && \
   [[ -z $ZSH_EXECUTION_STRING ]] && \
   [[ -z $VSCODE_RESOLVING_ENVIRONMENT ]] && \
   [[ -z $INTELLIJ_ENVIRONMENT_READER ]] && \
   [[ ! -f ~/.inhibit-nushell-autoexec ]]; then

  () {
    if [[ -n $ZSH_PROFILE_STARTUP ]]; then zprof; fi

    local args=()
    if [[ $INSIDE_EMACS == vterm ]]; then
      args+=( --config ~/.config/nushell/emacs-vterm-config.nu )
    fi
    exec nu -il "${args[@]}"
  }
fi
