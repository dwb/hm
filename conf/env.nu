def xdg-config-home [for?: string] {
  let default = ($nu.home-dir | path join .config)
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
  let default = ($nu.home-dir | path join .local state)
  mut out = (try { $env.XDG_STATE_HOME } catch { $default })
  if $out == "" {
    $out = $default
  }
  if $for != null {
    $out = ($out | path join $for)
  }
  $out
}

$env.PROMPT_COMMAND_DEFUALT = $env.PROMPT_COMMAND

def my_create_left_prompt [] {
    let hostname = (
      try {
        open --raw ~/.config/hostname-override | str trim
      } catch {
        sys host | get hostname | str replace --regex `\..*` ''
      })
    let user = whoami
    let aws_profile = ($env | get -o AWS_PROFILE)

    [
      (if $nu.history-enabled { "" } else { "🥷  " })
      (if $aws_profile != null { $"[AWS ($aws_profile)] " })
      (if $user =~ "^(dani?|dwb)$" { "" } else { $"($user)@" })
      (if $hostname == "tanxe" { "" } else { $"($hostname) " })
      (do $env.PROMPT_COMMAND_DEFUALT)
    ] | where { $in != null } | str join
}

def my_create_right_prompt [] {
    let last_exit_code = if ($env.LAST_EXIT_CODE != 0) {([
        (ansi rb)
        ($env.LAST_EXIT_CODE)
    ] | str join)
    } else { "" }

    ([$last_exit_code] | str join)
}

$env.PROMPT_COMMAND = { my_create_left_prompt }
$env.PROMPT_COMMAND_RIGHT = { my_create_right_prompt }

# Directories to search for scripts when calling source or use
$env.NU_LIB_DIRS = [
    (xdg-config-home nushell | path join scripts)
]

# Directories to search for plugin binaries when calling register
$env.NU_PLUGIN_DIRS = [
    # ($nu.default-config-dir | path join 'plugins') # add <nushell-config-dir>/plugins
]

