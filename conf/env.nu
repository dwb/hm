def xdg-config-home [for?: string] {
  let default = ($nu.home-path | path join .config)
  mut out = (try { $env.XDG_CONFIG_HOME } catch { $default })
  if $out == "" {
    $out = $default
  }
  if $for != null {
    $out = ($out | path join $for)
  }
  $out
}

def xdg-state-home [for?: string] {
  let default = ($nu.home-path | path join .local state)
  mut out = (try { $env.XDG_STATE_HOME } catch { $default })
  if $out == "" {
    $out = $default
  }
  if $for != null {
    $out = ($out | path join $for)
  }
  $out
}

def create_left_prompt [] {
    mut home = ""
    try {
        if $nu.os-info.name == "windows" {
            $home = $env.USERPROFILE
        } else {
            $home = $env.HOME
        }
    }

    let dir = ([
        ($env.PWD | str substring 0..($home | str length) | str replace $home "~"),
        ($env.PWD | str substring ($home | str length)..)
    ] | str join)

    let path_color = (if (is-admin) { ansi red_bold } else { ansi green_bold })
    let separator_color = (if (is-admin) { ansi light_red_bold } else { ansi light_green_bold })
    let path_segment = ($"($path_color)($dir)" | str replace --all (char path_sep) $"($separator_color)/($path_color)")

    let sys = sys
    let hostname = ($sys.host.hostname | | str replace --regex `\..*` '')
    let user = whoami

    [
      (if $nu.history-enabled { "" } else { "🥷  " })
      (if $user =~ "^(dani?|dwb)$" { "" } else { $"($user)@" })
      (if $hostname == "tanxe" { "" } else { $"($hostname) " })
      $path_segment
    ] | str join
}

def create_right_prompt [] {
    let last_exit_code = if ($env.LAST_EXIT_CODE != 0) {([
        (ansi rb)
        ($env.LAST_EXIT_CODE)
    ] | str join)
    } else { "" }

    ([$last_exit_code] | str join)
}

# Use nushell functions to define your right and left prompt
$env.PROMPT_COMMAND = {|| create_left_prompt }
$env.PROMPT_COMMAND_RIGHT = {|| create_right_prompt }

# The prompt indicators are environmental variables that represent
# the state of the prompt
$env.PROMPT_INDICATOR = {|| " > " }
$env.PROMPT_INDICATOR_VI_INSERT = {|| " : " }
$env.PROMPT_INDICATOR_VI_NORMAL = {|| " ; " }
$env.PROMPT_MULTILINE_INDICATOR = {|| "::: " }

# Specifies how environment variables are:
# - converted from a string to a value on Nushell startup (from_string)
# - converted from a value back to a string when running external commands (to_string)
# Note: The conversions happen *after* config.nu is loaded
$env.ENV_CONVERSIONS = {
    "PATH": {
        from_string: { |s| $s | split row (char esep) | path expand --no-symlink }
        to_string: { |v| $v | path expand --no-symlink | str join (char esep) }
    }
    "Path": {
        from_string: { |s| $s | split row (char esep) | path expand --no-symlink }
        to_string: { |v| $v | path expand --no-symlink | str join (char esep) }
    }
}

# Directories to search for scripts when calling source or use
$env.NU_LIB_DIRS = [
    (xdg-config-home nushell | path join scripts)
]

# Directories to search for plugin binaries when calling register
$env.NU_PLUGIN_DIRS = [
    # ($nu.default-config-dir | path join 'plugins') # add <nushell-config-dir>/plugins
]

