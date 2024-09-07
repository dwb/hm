$env.config.show_banner = false
$env.config.edit_mode = 'vi'
$env.config.history = {
    max_size: 100_000 # Session has to be reloaded for this to take effect
    sync_on_enter: true # Enable to share history between multiple sessions, else you have to close the session to write history to file
    file_format: "sqlite" # "sqlite" or "plaintext"
    isolation: true # only available with sqlite file_format. true enables history isolation, false disables it. true will allow the history to be isolated to the current session using up/down arrows. false will allow the history to be shared across all sessions.
}
$env.config.cursor_shape = {
    emacs: 'blink_line' # block, underscore, line, blink_block, blink_underscore, blink_line, inherit to skip setting cursor shape (line is the default)
    vi_insert: 'line' # block, underscore, line, blink_block, blink_underscore, blink_line, inherit to skip setting cursor shape (block is the default)
    vi_normal: 'block' # block, underscore, line, blink_block, blink_underscore, blink_line, inherit to skip setting cursor shape (underscore is the default)
}

alias gg = g
alias g = git
alias priv = nu --no-history
alias fopen = open # clashes with macOS's `open`
alias open = ^open

# doesn't work with alias because
# https://github.com/nushell/nushell/issues/8471
use contrib/custom-completions/git/git-completions.nu *

use contrib/custom-completions/make/make-completions.nu *
use contrib/custom-completions/man/man-completions.nu *
use contrib/custom-completions/nix/nix-completions.nu *
use contrib/custom-completions/pnpm/pnpm-completions.nu *
use contrib/custom-completions/yarn/yarn-v4-completions.nu *

use my/ollama.nu *
use my/my-nix.nu *
use my/new.nu *
use my/nix.nu *
use my/utils.nu *

use new

use std *
use std iter *
