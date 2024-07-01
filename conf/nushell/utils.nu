use std iter

export def e [fn: path] {
  ^$env.EDITOR $fn
}

export def --env gcd [] {
  let dir = (do -c { git rev-parse --show-toplevel })
  cd $dir
}

export def success? [fn: closure] {
  try {
    do -c $fn
    true
  } catch {
    false
  }
}

export def when [cond, fn: closure] {
  let val = $in
  match ($cond | describe) {
    bool => (if $cond { $val | (do $fn) } else { $val })
    closure => (if (do $cond $val) { $val | (do $fn) } else { $val })
    _ => (error make {
            msg: "type error"
            label: { text: "condition must be bool or closure" span: (metadata $cond).span }
          })
  }
}

# Repeatedly calls CMD until it succeeds.
export def until-success [cmd: closure, --initial-wait: duration = 2sec, --wait-factor: float = 1.5, --wait-limit: duration = 5min] {
  mut wait: duration = $initial_wait
  while not (success? $cmd) {
    print -e $"until-success?: trying again in ($wait)"
    sleep $wait
    $wait = ([($wait * $wait_factor) $wait_limit] | math min)
  }
}

export def --env mkcd [dir: path] {
  mkdir $dir
  cd $dir
}

export def eachkey [fn: closure] {
  transpose k v | each $fn | transpose -i -r -d
}

# Converts an int or string UNIX timestamp into a datetime
export def "from timestamp" []: [int -> datetime, string -> datetime] {
  into string | into datetime --format %s --timezone u
}

export def file [...globs: glob] {
  let files = ($globs | iter flat-map { glob ($in | into string) })
  do --capture-errors { ^file --mime --no-buffer --print0 --separator '' ...$files } |
    lines |
    parse -r "(?P<file>[^\u{0}]+)\u{0}\\s*(?P<mimetype>[^;]+)(?:;\\s*charset=(?P<charset>\\S+))?" |
    update file { path expand }
}

# restarts current shell
export def reexec [] {
  let args = [ --interactive ] |
    if $nu.is-login { append '--login' } else { $in }
  exec $nu.current-exe ...$args
}

# gets structured data from a remote host
export def --wrapped sshs [host: string, cmd: string, ...args: string] {
  ssh $host $cmd ...$args | ^jc -- $"--($cmd)" | from json
}
