### AUTO SCREEN ###

if (( $SHLVL == 1 )) && (( $+commands[screen] )) \
  && [[ -z $STY && -r ~/.config/cool.dani/autoscreen ]]; then
  exec screen -DR
fi

### ENV CUSTOMISATION ###

e() {
  "$EDITOR" "$@"
}

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
   [[ ! -f ~/.inhibit-nushell-autoexec ]]; then

  () {
    local args=()
    if [[ $INSIDE_EMACS == vterm ]]; then
      args+=( --config ~/.config/nushell/emacs-vterm-config.nu )
    fi
    exec nu -il "${args[@]}"
  }
fi

### OK THEN ###

PS1="%1F%n@%m%f %2F%1d%f %B%#%b "

ZSH_AUTOSUGGEST_USE_ASYNC=1

autoload smart-insert-last-word
zle -N insert-last-word smart-insert-last-word

bindkey -v
# this breaks emacs vterm, dunno why:
# bindkey "^J" push-line-or-edit
bindkey "^K" insert-last-word
bindkey "\e[A" up-line-or-search
bindkey "\e[B" down-line-or-search
bindkey "^R" history-incremental-search-backward
bindkey '^[[1;5C' forward-word

# beta zsh-vi-mode engine
# https://github.com/jeffreytse/zsh-vi-mode#readkey-engine
ZVM_READKEY_ENGINE=nex

autoload -z edit-command-line
zle -N edit-command-line
bindkey "^X^E" edit-command-line

autoload zmv

setopt extended_glob

# Ignore commands that start with a space character when adding them to the
# command history.
setopt histignorespace

# history related stuff.
#
if [[ -n $__INTELLIJ_COMMAND_HISTFILE__ ]]; then
  HISTFILE="$__INTELLIJ_COMMAND_HISTFILE__"
elif [[ -n $ITERM_SESSION_ID ]]; then
  mkdir -p ${ZDOTDIR:-$HOME}/.cache/iterm
  HISTFILE=${ZDOTDIR:-$HOME}/.cache/iterm/zsh-history-${ITERM_SESSION_ID/:/_}
else
  HISTFILE=
fi

# clear old history logs
() {
  local files=( ${ZDOTDIR:-$HOME}/.cache/iterm/zsh-history-*(.NmM+1) )
  if [[ ${#files} -gt 0 ]]; then
    echo "Deleting ${#files} historical iTerm history files..."
    rm "${files[@]}"
  fi
}


# Define the maximum number of commands to store in memory
HISTSIZE=5000

# Set the maximum number of commands to save in the history file
SAVEHIST=1000

# Append each command to the history file as soon as it is executed, instead of
# waiting until the shell exits
setopt INC_APPEND_HISTORY

# Remove oldest duplicate command in history when saving a new command
setopt HIST_EXPIRE_DUPS_FIRST

# Prevent consecutive duplicate commands from being stored in the history
setopt HIST_IGNORE_DUPS

# Remove extra spaces from the command before saving it in the history
setopt HIST_REDUCE_BLANKS

# Don't save commands in the history file if they are duplicates of existing
# commands
setopt HIST_SAVE_NO_DUPS

# When searching history using `history` builtin, don't show duplicates
setopt HIST_FIND_NO_DUPS

# *Do* save a command that is already in the history, regardless of position
setopt NO_HIST_IGNORE_ALL_DUPS

# *Don't* record timestamp and duration of each command in the history file
setopt NO_EXTENDED_HISTORY

# Show the expanded line for verification before execution after using history
# expansion (e.g., `!`)
setopt HIST_VERIFY

# Don't share history instantly between sessions.
setopt nosharehistory

REPORTTIME=5

setopt pushd_silent # No more annoying pushd messages...
setopt pushd_to_home
unsetopt re_match_pcre # not available on OS X
unsetopt bash_rematch

# function {
#   local dir="$HOME/.config/zsh/functions"

#   if [[ -d $dir ]]; then
#     fpath=($dir $fpath)
#     autoload -U "$dir"/*(:t)
#   fi
# }

function title() {
  print -Pn "\e]0;$1\a"
}

function precmd() {
  title "%n@%m:%55<...<%~"
}

autoload -U colors
colors
setopt prompt_subst

TERM_IDE_EMBEDDED=""
if [[ $TERMINAL_EMULATOR = "JetBrains-JediTerm" ]] || [[ $TERM_PROGRAM = vscode ]] || [[ $INSIDE_EMACS = 'vterm' ]]; then
  TERM_IDE_EMBEDDED=true
fi


_git_prompt_info() {
  if whence -w git_super_status >/dev/null; then
    git_super_status
  else
    local ref
    ref=$(git symbolic-ref HEAD 2> /dev/null) || return
    echo -n " %{$fg_bold[yellow]%}(${ref#refs/heads/})%{$reset_color%}"
  fi
  # if git log -1 --format=%f 2> /dev/null | grep -qiw 'danwip'; then
    # echo " %{$fg_bold[red]%}WIP%{$reset_color%}"
  # fi
}

function _my_rprompt() {
  if [[ -z $TERM_IDE_EMBEDDED ]]; then
    if [[ $(git rev-parse --show-toplevel 2>/dev/null) == $HOME ]]; then
      printf '%s' '🏠 '
    fi
    _git_prompt_info
  fi
}

RPROMPT='$(_my_rprompt)'

ZSH_THEME_GIT_PROMPT_CHANGED="%{$fg[blue]%}+"

# enable starship prompt if installed
if (( $+commands[starship] )); then
  eval "$(starship init zsh)"
fi


refresh() {
  exec "$SHELL"
}

alias ll="ls -l "
alias la="ls -la "
alias lf="less +F "
alias pd="pushd"
alias pod="popd"
alias clf="clear; tail -n 0 -f "
alias svi="svn --ignore-externals "
alias ssh='ssh -o "LocalCommand echo -ne \"\033]0;%h\007\"" '
alias tarnox="COPYFILE_DISABLE=true tar "
# projects etc
alias g="git"
alias p="pijul"
alias kc="kubectl"
alias rp="rg"
alias rip="rg"
(( $+commands[hub] )) && eval "$(hub alias -s)"
alias -r be="bundle exec "
alias bi="bundle install --quiet"
alias bu="bundle update --quiet"
alias ne='PATH=$(npm bin):$PATH'
alias ye='yarn exec'
alias xrargs='printf "%s\n"'
alias rr="ruby -I~/.config/zsh/ruby -roneliner_helpers"
[ -z "${TMUX}" ] || alias clear="clear; tmux clear-history"
alias clpt="clear; pt"
alias akcurl="curl -IXGET -H \"Pragma: akamai-x-cache-on, akamai-x-cache-remote-on, akamai-x-check-cacheable, akamai-x-get-cache-key, akamai-x-get-extracted-values, akamai-x-get-nonces, akamai-x-get-ssl-client-session-id, akamai-x-get-true-cache-key, akamai-x-serial-no\""
alias pubip="dig @ns1-1.akamaitech.net ANY whoami.akamai.net +short"
# alias psql="DYLD_LIBRARY_PATH=/Applications/Postgres.app/Contents/Versions/latest/lib:/usr/local/opt/readline/lib:/usr/lib psql"
alias ltail="less -r +F"
alias casscols="sed -e '1,/^---/d;s/^ //;s/ | /	/g'"
alias stripansi="sed -E 's/'$'\E''\[([0-9]{1,3}((;[0-9]{1,3})*)?)?[m|K]//g'"
alias ec=emacsclient
alias ecc='emacsclient -c'

# babashka
bb() {
  command bb --init ~/.config/babashka/init.clj "$@"
}

alias alf='alfred-file'
alias alt='alfred-text'
alias alu='alfred-url'

archivebox() {
  (cd ~/Dump/ArchiveBox && docker-compose run --service-ports archivebox "$@")
}
