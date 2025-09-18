def state-file [] {
  $env.HOME | path join ".local/state/dwb-vcd/dirs.toml"
}

def load-state [] {
  let f = (state-file)
  if ($f | path exists) {
    open $f
  } else {
    {}
  }
}

def save-state [state: record] {
  let f = (state-file)
  mkdir ($f | path dirname)
  $state | to toml | save -f $f
}

# Collect immediate child directories of parent dirs into name->path record.
# First entry in the list wins on name collisions.
def parent-children [parents: list<string>]: nothing -> record {
  $parents | reduce --fold {} { |parent, acc|
    if ($parent | path exists) {
      ls $parent | where type == dir | get name | reduce --fold $acc { |child, acc|
        let name = ($child | path basename)
        if $name in $acc { $acc } else { $acc | insert $name $child }
      }
    } else {
      $acc
    }
  }
}

def in-scope [pwd: string, scope: string]: nothing -> bool {
  $pwd == $scope or ($pwd | str starts-with $"($scope)/")
}

# Collect all top-level entry paths (dirs values + parent-dir children paths).
def all-entry-paths [state: record]: nothing -> list<string> {
  let dirs = ($state | get -o dirs | default {})
  let parents = ($state | get -o parent-dirs | default [])
  let children = (parent-children $parents)
  ($dirs | values) | append ($children | values)
}

# Find the scope (closest ancestor entry) for a path that is a proper descendant.
def find-scope [target: string]: nothing -> string {
  let all_paths = (all-entry-paths (load-state))
  let ancestors = ($all_paths | where { |p| $target | str starts-with $"($p)/" })
  if ($ancestors | is-empty) {
    error make { msg: $"'($target)' is not within any known vcd entry" }
  }
  $ancestors | sort-by { $in | str length } | last
}

# Resolve scoped-dirs active for PWD into name->path record.
# Most specific scope wins on name collisions.
def active-scoped-dirs [entries: list, pwd: string]: nothing -> record {
  $entries
  | where { |e| in-scope $pwd $e.scope }
  | sort-by { $in.scope | str length } --reverse
  | reduce --fold {} { |e, acc|
    if $e.name in $acc { $acc } else { $acc | insert $e.name $e.path }
  }
}

# Resolve scoped-parent-dirs active for PWD into name->path record.
# Most specific scope wins on name collisions.
def active-scoped-parent-children [entries: list, pwd: string]: nothing -> record {
  let active_paths = ($entries
    | where { |e| in-scope $pwd $e.scope }
    | sort-by { $in.scope | str length } --reverse
    | each { |e| $e.path })
  parent-children $active_paths
}

# Evaluate scoped-globs active for PWD into name->path record.
# Each rule's pattern is a glob matching scope directories. The path field is
# relative within each matched scope. The children flag controls whether to
# enumerate children of that path or treat it as a direct entry.
def active-scoped-glob-entries [entries: list, pwd: string] {
  $entries | reduce --fold {} { |rule, acc|
    let scopes = (glob --depth 1 --no-file ($rule.pattern | into glob))
    let active = ($scopes | where { |s| in-scope $pwd $s })
    if ($active | is-empty) {
      $acc
    } else {
      let scope = ($active | sort-by { $in | str length } | last)
      let target_path = if $rule.path == "" { $scope } else { $scope | path join $rule.path }
      if $rule.children {
        if ($target_path | path exists) {
          ls $target_path | where type == dir | get name | reduce --fold $acc { |child, acc|
            let n = ($child | path basename)
            if $n in $acc { $acc } else { $acc | insert $n $child }
          }
        } else { $acc }
      } else {
        if ($target_path | path exists) {
          let rule_name = ($rule | get -o name | default "")
          let n = if $rule_name != "" { $rule_name } else { $target_path | path basename }
          if $n in $acc { $acc } else { $acc | insert $n $target_path }
        } else { $acc }
      }
    }
  }
}

# Resolve a name to a path.
# Priority: scoped-dirs > scoped-auto (parent-children, globs) > dirs > parent-children.
def resolve [name: string] {
  let state = (load-state)
  let dirs = ($state | get -o dirs | default {})
  let parents = ($state | get -o parent-dirs | default [])
  let scoped_dirs_list = ($state | get -o scoped-dirs | default [])
  let scoped_parents_list = ($state | get -o scoped-parent-dirs | default [])
  let scoped_globs_list = ($state | get -o scoped-globs | default [])
  let pwd = $env.PWD

  let parent_children = (parent-children $parents)
  let active_scoped = (active-scoped-dirs $scoped_dirs_list $pwd)
  let active_scoped_children = (active-scoped-parent-children $scoped_parents_list $pwd)
  let active_scoped_globs = (active-scoped-glob-entries $scoped_globs_list $pwd)

  if $name in $active_scoped {
    if $name in $active_scoped_children or $name in $active_scoped_globs or $name in $dirs or $name in $parent_children {
      print -e $"vcd: warning: scoped bookmark '($name)' shadows another entry"
    }
    return ($active_scoped | get $name)
  }

  if $name in $active_scoped_children {
    if $name in $dirs or $name in $parent_children {
      print -e $"vcd: warning: scoped parent child '($name)' shadows a global entry"
    }
    return ($active_scoped_children | get $name)
  }

  if $name in $active_scoped_globs {
    if $name in $dirs or $name in $parent_children {
      print -e $"vcd: warning: scoped glob match '($name)' shadows a global entry"
    }
    return ($active_scoped_globs | get $name)
  }

  if $name in $dirs {
    if $name in $parent_children {
      print -e $"vcd: warning: bookmark '($name)' shadows a child of a parent directory"
    }
    return ($dirs | get $name)
  }

  if $name in $parent_children {
    return ($parent_children | get $name)
  }

  error make { msg: $"no bookmark named '($name)'" }
}

# Relative path from base to target, with .. traversals for siblings/ancestors.
def relative-path-from [target: string, base: string]: nothing -> string {
  if ($target | str starts-with $"($base)/") {
    $target | path relative-to $base
  } else if $target == $base {
    "."
  } else {
    let parent = ($base | path dirname)
    if $parent == $base {
      $target # reached root — return absolute as fallback
    } else {
      ".." | path join (relative-path-from $target $parent)
    }
  }
}

def completer [context: string] {
  let state = (load-state)
  let dirs = ($state | get -o dirs | default {})
  let parents = ($state | get -o parent-dirs | default [])
  let scoped_dirs_list = ($state | get -o scoped-dirs | default [])
  let scoped_parents_list = ($state | get -o scoped-parent-dirs | default [])
  let scoped_globs_list = ($state | get -o scoped-globs | default [])
  let pwd = $env.PWD

  let parent_children = (parent-children $parents)
  let active_scoped = (active-scoped-dirs $scoped_dirs_list $pwd)
  let active_scoped_children = (active-scoped-parent-children $scoped_parents_list $pwd)
  let active_scoped_globs = (active-scoped-glob-entries $scoped_globs_list $pwd)

  # Build unified list with priority for dedup (lower number = higher priority)
  let all = (
    ($active_scoped | transpose name path | each { |e| { name: $e.name, path: $e.path, priority: 1 } })
    | append ($active_scoped_children | transpose name path | each { |e| { name: $e.name, path: $e.path, priority: 2 } })
    | append ($active_scoped_globs | transpose name path | each { |e| { name: $e.name, path: $e.path, priority: 2 } })
    | append ($dirs | transpose name path | each { |e| { name: $e.name, path: $e.path, priority: 3 } })
    | append ($parent_children | transpose name path | each { |e| { name: $e.name, path: $e.path, priority: 4 } })
  )

  let completions = ($all
  | sort-by priority
  | uniq-by name
  | where { |e| $e.path != $pwd }
  | each { |e|
    let desc = if $e.priority <= 2 {
      relative-path-from $e.path $pwd
    } else {
      $e.path | str replace $env.HOME "~"
    }
    { value: $e.name, description: $desc }
  })
  { completions: $completions, options: { sort: false } }
}

# Quick cd to a bookmarked directory
export def --env main [
  target?: string@completer # Bookmark name (or directory path with --add)
  --add (-a)                    # Add directory to bookmarks (default: current dir)
  --name (-n): string           # Alternate bookmark name (with --add)
  --children (-c)               # Register as parent dir whose children become candidates
  --scoped (-s)                 # Entry is only active when PWD is within its parent entry
  --glob (-g): string           # Glob pattern matching scope directories (requires --scoped)
] {
  if ($children or $scoped or $glob != null) and not $add {
    error make { msg: "--children, --scoped, and --glob require --add" }
  }
  if $glob != null and not $scoped {
    error make { msg: "--glob requires --scoped" }
  }

  if $add {
    let state = (load-state)
    let dirs = ($state | get -o dirs | default {})
    let parents = ($state | get -o parent-dirs | default [])

    if $glob != null {
      let full_glob = ($glob | path expand)
      let rel_path = ($target | default "")
      let scoped_globs = ($state | get -o scoped-globs | default [])
      let entry_name = ($name | default "")
      let existing = ($scoped_globs | where { |e| $e.pattern == $full_glob and $e.path == $rel_path and $e.children == $children and ($e | get -o name | default "") == $entry_name })
      if not ($existing | is-empty) {
        error make { msg: "this glob rule already exists" }
      }
      save-state ($state | upsert scoped-globs ($scoped_globs | append {
        pattern: $full_glob, path: $rel_path, children: $children, name: $entry_name
      }))
    } else if $scoped {
      let dir = ($target | default $env.PWD | path expand)
      if not ($dir | path exists) {
        error make { msg: $"directory does not exist: ($dir)" }
      }
      let scope = (find-scope $dir)

      if $children {
        let scoped_parents = ($state | get -o scoped-parent-dirs | default [])
        let existing = ($scoped_parents | where { |e| $e.path == $dir and $e.scope == $scope })
        if not ($existing | is-empty) {
          error make { msg: $"'($dir)' is already a scoped parent directory under '($scope)'" }
        }
        let new_children = (parent-children [$dir])
        $new_children | transpose name path | each { |e|
          if $e.name in $dirs {
            print -e $"vcd: warning: child '($e.name)' is shadowed by existing bookmark \(($dirs | get $e.name)\)"
          }
        } | ignore
        save-state ($state | upsert scoped-parent-dirs ($scoped_parents | append { path: $dir, scope: $scope }))
      } else {
        let entry_name = ($name | default ($dir | path basename))
        let scoped_dirs = ($state | get -o scoped-dirs | default [])
        let existing = ($scoped_dirs | where { |e| $e.name == $entry_name and $e.scope == $scope })
        if not ($existing | is-empty) {
          error make { msg: $"duplicate scoped bookmark '($entry_name)' under scope '($scope)'" }
        }
        let parent_children = (parent-children $parents)
        if $entry_name in $parent_children {
          print -e $"vcd: warning: scoped bookmark '($entry_name)' shadows child of parent directory \(($parent_children | get $entry_name)\)"
        }
        save-state ($state | upsert scoped-dirs ($scoped_dirs | append { name: $entry_name, path: $dir, scope: $scope }))
      }
    } else if $children {
      let dir = ($target | default $env.PWD | path expand)
      if not ($dir | path exists) {
        error make { msg: $"directory does not exist: ($dir)" }
      }
      if $dir in $parents {
        error make { msg: $"'($dir)' is already a parent directory" }
      }
      let new_children = (parent-children [$dir])
      $new_children | transpose name path | each { |e|
        if $e.name in $dirs {
          print -e $"vcd: warning: child '($e.name)' is shadowed by existing bookmark \(($dirs | get $e.name)\)"
        }
      } | ignore
      save-state ($state | upsert parent-dirs ($parents | append $dir))
    } else {
      let dir = ($target | default $env.PWD | path expand)
      if not ($dir | path exists) {
        error make { msg: $"directory does not exist: ($dir)" }
      }
      let entry_name = ($name | default ($dir | path basename))
      if $entry_name in $dirs {
        error make {
          msg: $"duplicate name '($entry_name)' \(already points to ($dirs | get $entry_name)\). Use --name to specify an alternate name."
        }
      }
      let parent_children = (parent-children $parents)
      if $entry_name in $parent_children {
        print -e $"vcd: warning: bookmark '($entry_name)' shadows child of parent directory \(($parent_children | get $entry_name)\)"
      }
      save-state ($state | upsert dirs ($dirs | insert $entry_name $dir))
    }
  } else if $target == null {
    let state = (load-state)
    let all_paths = (all-entry-paths $state)
    let ancestors = ($all_paths | where { |p| $env.PWD | str starts-with $"($p)/" })
    if ($ancestors | is-empty) {
      cd $env.HOME
    } else {
      cd ($ancestors | sort-by { $in | str length } | last)
    }
  } else {
    cd (resolve $target)
  }
}
