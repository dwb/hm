export def success? [fn: closure] {
  try {
    do -c $fn
    true
  } catch {
    false
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

export def file [...path: glob] {
  ^file --mime --no-buffer --print0 --separator '' ...$path | lines | parse -r "(?P<fn>[^\u{0}]+)\u{0}\\s*(?P<mimetype>[^;]+)(?:; charset=(?P<charset>\\S+))?"
}
