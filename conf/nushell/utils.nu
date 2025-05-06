use std iter

export def e [fn: path] {
  ^$env.EDITOR $fn
}

export def gcd-subpath-completer [context: string] {
  let root = (do -c { git rev-parse --show-toplevel })
  let dir = ($context | split words | get -i 1)
  glob --no-file ($root | path join $"($dir)*") |
    each { path relative-to $root }
}

# chdir to directory relative to git repository root
export def --env gcd [subpath?: string@gcd-subpath-completer] {
  let root = (do -c { git rev-parse --show-toplevel })
  cd (if $subpath == null {
    $root
  } else {
    $root | path join $subpath
  })
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

# Returns the value of the first column found with a non-null value
export def first-col-with-value [
  ...cols: cell-path # Columns to search, in order, for first non-null value
]: record -> any {
  let r = $in
  for $p in $cols {
    let val = $r | get -i $p
    if $val != null {
      return $val
    }
  }
}

# Iterates through list, calling closure with (prev, current) arguments.
#
# $prev is null for first record. Closure pipeline input is current value.
export def each-with-prev [fn: closure]: list -> list {
  let lst = $in
  $lst | zip ($lst | prepend [null]) | each { |rs|
    $rs.0 | do $fn $rs.1 $rs.0
  }
}

# Adds field indicating running total of given field
export def running-total [
  infield: cell-path,  # Field to calculate running total of
  outfield: cell-path, # Field to add
  initial = 0: any,    # Initial value of total
]: list<record> -> list<record> {
  each-with-prev { |prev, r|
    insert $outfield { ($prev | get -i $infield | default $initial) + ($r | get $infield) }
  }
}
