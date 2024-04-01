### AUTO SCREEN ###

if (( $SHLVL == 1 )) && (( $+commands[screen] )) \
  && [[ -z $STY && -r ~/.config/cool.dani/autoscreen ]]; then
  exec screen -DR
fi

### HELPERS ###

sourceifpresent() {
  for fn in "$@"; do
    [[ -r $fn ]] && source "$fn"
  done
}

### PATH CUSTOMISATION ###

if ! (( $+commands[nix-env] )); then
  sourceifpresent /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
fi

my-nix-path() {
  if ! (( $+commands[nix] )); then
    return
  fi
  nix profile list | awk '$2 ~ /\.'"$1"'$/ { print $4 }' | tail -1
}

if (( $+commands[brew] )); then
  eval "$(brew shellenv)"
  typeset -U PATH # de-dupe
  rehash
fi

if (( $+commands[pyenv] )); then
  export PYENV_ROOT="$HOME/.pyenv"
  export PATH="$PYENV_ROOT/bin:$PATH"
  eval "$(pyenv init --path)"
fi

if (( $+commands[git] )); then
  export PATH="$(git --exec-path):$PATH"
fi

if (( $+commands[luarocks] )); then
  eval "$(luarocks path)"
fi

sourceifpresent "$HOME/.config/zsh/minpath.zsh"

### END PATH CUSTOMISATION ###
#
### ENV CUSTOMISATION ###

if (( $+commands[emacsdani] )); then
  export EDITOR="$(whence -p emacsdani)"
elif (( $+commands[nvim] )); then
  export EDITOR="$(whence -p nvim)"
elif (( $+commands[vim] )); then
  export EDITOR="$(whence -p vim)"
fi

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

(( $+commands[java_home] )) && export JAVA_HOME="$(java_home)"

(( $+commands[rbenv] )) && eval "$(rbenv init -)"
(( $+commands[pyenv] )) && eval "$(pyenv init -)"
(( $+commands[pyenv-virtualenv-init] )) && eval "$(pyenv virtualenv-init -)"
if [[ -n $HOMEBREW_PREFIX ]] && [[ -r $HOMEBREW_PREFIX/opt/nvm/nvm.sh ]]; then
  export NVM_DIR="$HOME/.nvm"
  mkdir -p "$NVM_DIR"
  . "$HOMEBREW_PREFIX/opt/nvm/nvm.sh"
fi

sourceifpresent /usr/local/bin/aws_zsh_completer.sh
sourceifpresent /usr/local/bin/virtualenvwrapper.sh

### END ENV CUSTOMISATION ###
#
### MAYBE AUTOEXEC NUSHELL ###

if (( $+commands[nu] )) && \
   [[ -o login ]] && \
   [[ -o interactive ]] && \
   [[ ! -f ~/.inhibit-nushell-autoexec ]]; then

  (
    set -eo pipefail
    cd ~/.config/nushell
    make --silent
  ) || echo -e "\n*** Couldn't update nushell config"
  exec nu -il
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

autoload -U compinit

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

fpath=(/usr/local/opt/hub/share/zsh/site-functions $fpath)

function {
  local dir="$HOME/.config/zsh/functions"

  if [[ -d $dir ]]; then
    fpath=($dir $fpath)
    autoload -U "$dir"/*(:t)
  fi
}

compinit -u
compdef -d rake # rake autocomplete is SO SLOW; disable it


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


[[ -x ~/.nvim/plugged/gruvbox/gruvbox_256palette_osx.sh ]] && \
  ~/.nvim/plugged/gruvbox/gruvbox_256palette_osx.sh

fzf_path="$(my-nix-path fzf)"
if [[ -n $fzf_path ]]; then
  sourceifpresent "$fzf_path"/share/fzf/*.zsh

  if (( $+commands[bffindd] )); then
    export FZF_ALT_C_COMMAND='bffindd'
  else
    echo "$0: install bffindd for faster fzf: nix profile install ~/Developer/bffindd"
  fi

  export FZF_TMUX=0
  FZF_COMPLETION_DIR_COMMANDS="cd pushd rmdir pd"
  export FZF_DEFAULT_OPTIONS='+x'
  export FZF_CTRL_T_COMMAND='git ls-files --cached --others --exclude-standard 2>/dev/null | awk '\''{ print $0; sub(/\/[^\/]*$/, ""); print $0 }'\'' | sort | uniq || rg -l '\'\'' | awk '\''{ print $0; sub(/\/[^\/]*$/, ""); print $0 }'\'' | sort | uniq'
fi

install-brew-env() {
  brew install fzf z rbenv pyenv ruby-build antibody ul/kak-lsp/kak-lsp
}

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
alias -g G="| grep"
alias -g L="| less"
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

arbo() {
  (cd ~/Dump/ArchiveBox && docker-compose run --service-ports archivebox "$@")
}

if [[ -n $TRIAS_PROFILE ]] && [[ -z $TRIAS_LOADED ]]; then
  PS1="[prx $TRIAS_PROFILE] $PS1"
  kubectl() {
    command kubectl --context="$TRIAS_PROFILE" "$@"
  }
  TRIAS_LOADED=yep
fi

sourceifpresent ~/.zshrc-local

sourceifpresent ~/.config/zsh/plugins.zsh

# OPAM configuration
if [[ -r $HOME/.opam/opam-init/init.zsh ]]; then
  . $HOME/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
fi

# Re-set this because the "tumult" plugin sets it back to vim -_-
# export EDITOR="$(whence -p emacsdani)"
# export VISUAL="$EDITOR"

# if (( $+commands[nvim] )); then
#   alias vim="nvim"
#   export MANPAGER="nvim -c 'set ft=man' -"
# fi

for fn in ~/.config/zsh/emacs-vterm.zsh ~/.config/zsh/fzfgit.zsh ~/.config/zsh/iterm2_shell_integration.zsh /usr/local/opt/asdf/libexec/asdf.sh ~/.config/broot/launcher/bash/br ; do
  sourceifpresent "$fn"
done

if (( $+commands[direnv] )); then
  eval "$(direnv hook zsh)"
fi
