# light theme
$env.config.color_config = {
    # color for nushell primitives
    separator: dark_gray
    leading_trailing_space_bg: { attr: n } # no fg, no bg, attr none effectively turns this off
    header: green_bold
    empty: blue
    # Closures can be used to choose colors for specific values.
    # The value (in this case, a bool) is piped into the closure.
    # eg) {|| if $in { 'dark_cyan' } else { 'dark_gray' } }
    bool: dark_cyan
    int: dark_gray
    filesize: cyan_bold
    duration: dark_gray
    date: purple
    range: dark_gray
    float: dark_gray
    string: dark_gray
    nothing: dark_gray
    binary: dark_gray
    cell-path: dark_gray
    row_index: green_bold
    record: dark_gray
    list: dark_gray
    block: dark_gray
    hints: dark_gray
    search_result: { fg: white bg: red }
    shape_and: purple_bold
    shape_binary: purple_bold
    shape_block: blue_bold
    shape_bool: light_cyan
    shape_closure: green_bold
    shape_custom: green
    shape_datetime: cyan_bold
    shape_directory: cyan
    shape_external: cyan
    shape_externalarg: green_bold
    shape_external_resolved: light_purple_bold
    shape_filepath: cyan
    shape_flag: blue_bold
    shape_float: purple_bold
    # shapes are used to change the cli syntax highlighting
    shape_garbage: { fg: white bg: red attr: b}
    shape_globpattern: cyan_bold
    shape_int: purple_bold
    shape_internalcall: cyan_bold
    shape_keyword: cyan_bold
    shape_list: cyan_bold
    shape_literal: blue
    shape_match_pattern: green
    shape_matching_brackets: { attr: u }
    shape_nothing: light_cyan
    shape_operator: yellow
    shape_or: purple_bold
    shape_pipe: purple_bold
    shape_range: yellow_bold
    shape_record: cyan_bold
    shape_redirection: purple_bold
    shape_signature: green_bold
    shape_string: green
    shape_string_interpolation: cyan_bold
    shape_table: blue_bold
    shape_variable: purple
    shape_vardecl: purple
}

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

$env.config.datetime_format.normal = "%F %T%.f %Z"
$env.config.datetime_format.table = "%F %T%.f %Z"

alias gg = g
alias g = git
alias priv = nu --no-history
alias fopen = open # clashes with macOS's `open`
alias open = ^open

# doesn't work with alias because
# https://github.com/nushell/nushell/issues/8471
source contrib/custom-completions/git/git-completions.nu

source contrib/custom-completions/make/make-completions.nu
source contrib/custom-completions/man/man-completions.nu
source contrib/custom-completions/nix/nix-completions.nu
source contrib/custom-completions/pnpm/pnpm-completions.nu
source contrib/custom-completions/yarn/yarn-v4-completions.nu

use my/archivebox.nu
use my/file-menu.nu
use my/glob-menu.nu
use my/ollama.nu *
use my/my-nix.nu *
use my/new.nu *
use my/nix.nu *
use my/utils.nu *

use new

use std *
use std iter *
