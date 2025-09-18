def vcd-state-file [] {
  $env.HOME | path join ".local/state/dwb-vcd/dirs.toml"
}

def vcd-load-state [] {
  let f = (vcd-state-file)
  if ($f | path exists) {
    open $f
  } else {
    {}
  }
}

def vcd-save-state [state: record] {
  let f = (vcd-state-file)
  mkdir ($f | path dirname)
  $state | to toml | save -f $f
}

# Collect immediate child directories of parent dirs into name->path record.
# First entry in the list wins on name collisions.
def vcd-parent-children [parents: list<string>]: nothing -> record {
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

def vcd-in-scope [pwd: string, scope: string]: nothing -> bool {
  $pwd == $scope or ($pwd | str starts-with $"($scope)/")
}

# Collect all top-level entry paths (dirs values + parent-dir children paths).
def vcd-all-entry-paths [state: record]: nothing -> list<string> {
  let dirs = ($state | get -o dirs | default {})
  let parents = ($state | get -o parent-dirs | default [])
  let children = (vcd-parent-children $parents)
  ($dirs | values) | append ($children | values)
}

# Find the scope (closest ancestor entry) for a path. Errors if not within any entry.
def vcd-find-scope [target: string]: nothing -> string {
  let all_paths = (vcd-all-entry-paths (vcd-load-state))
  let ancestors = ($all_paths | where { |p| $target | str starts-with $"($p)/" })
  if ($ancestors | is-empty) {
    error make { msg: $"'($target)' is not within any known vcd entry" }
  }
  $ancestors | sort-by { $in | str length } | last
}

# Resolve scoped-dirs active for PWD into name->path record.
# Most specific scope wins on name collisions.
def vcd-active-scoped-dirs [entries: list, pwd: string]: nothing -> record {
  $entries
  | where { |e| vcd-in-scope $pwd $e.scope }
  | sort-by { $in.scope | str length } --reverse
  | reduce --fold {} { |e, acc|
    if $e.name in $acc { $acc } else { $acc | insert $e.name $e.path }
  }
}

# Resolve scoped-parent-dirs active for PWD into name->path record.
# Most specific scope wins on name collisions.
def vcd-active-scoped-parent-children [entries: list, pwd: string]: nothing -> record {
  let active_paths = ($entries
    | where { |e| vcd-in-scope $pwd $e.scope }
    | sort-by { $in.scope | str length } --reverse
    | each { |e| $e.path })
  vcd-parent-children $active_paths
}

# Resolve a name to a path.
# Priority: scoped-dirs > scoped-parent-children > dirs > parent-children.
def vcd-resolve [name: string] {
  let state = (vcd-load-state)
  let dirs = ($state | get -o dirs | default {})
  let parents = ($state | get -o parent-dirs | default [])
  let scoped_dirs_list = ($state | get -o scoped-dirs | default [])
  let scoped_parents_list = ($state | get -o scoped-parent-dirs | default [])
  let pwd = $env.PWD

  let parent_children = (vcd-parent-children $parents)
  let active_scoped = (vcd-active-scoped-dirs $scoped_dirs_list $pwd)
  let active_scoped_children = (vcd-active-scoped-parent-children $scoped_parents_list $pwd)

  if $name in $active_scoped {
    if $name in $active_scoped_children or $name in $dirs or $name in $parent_children {
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

def vcd-completer [context: string] {
  let state = (vcd-load-state)
  let dirs = ($state | get -o dirs | default {})
  let parents = ($state | get -o parent-dirs | default [])
  let scoped_dirs_list = ($state | get -o scoped-dirs | default [])
  let scoped_parents_list = ($state | get -o scoped-parent-dirs | default [])
  let pwd = $env.PWD

  let parent_children = (vcd-parent-children $parents)
  let active_scoped = (vcd-active-scoped-dirs $scoped_dirs_list $pwd)
  let active_scoped_children = (vcd-active-scoped-parent-children $scoped_parents_list $pwd)

  # Build unified list with priority for dedup (lower number = higher priority)
  let all = (
    ($active_scoped | transpose name path | each { |e| { name: $e.name, path: $e.path, priority: 1 } })
    | append ($active_scoped_children | transpose name path | each { |e| { name: $e.name, path: $e.path, priority: 2 } })
    | append ($dirs | transpose name path | each { |e| { name: $e.name, path: $e.path, priority: 3 } })
    | append ($parent_children | transpose name path | each { |e| { name: $e.name, path: $e.path, priority: 4 } })
  )

  $all
  | sort-by priority
  | uniq-by name
  | each { |e|
    { value: $e.name, description: ($e.path | str replace $env.HOME "~") }
  }
}

# Quick cd to a bookmarked directory
export def --env main [
  target?: string@vcd-completer # Bookmark name (or directory path with --add)
  --add (-a)                    # Add directory to bookmarks (default: current dir)
  --name (-n): string           # Alternate bookmark name (with --add)
  --children (-c)               # Register as parent dir whose children become candidates
  --scoped (-s)                 # Entry is only active when PWD is within its parent entry
] {
  if ($children or $scoped) and not $add {
    error make { msg: "--children and --scoped require --add" }
  }

  if $add {
    let dir = ($target | default $env.PWD | path expand)
    if not ($dir | path exists) {
      error make { msg: $"directory does not exist: ($dir)" }
    }
    let state = (vcd-load-state)
    let dirs = ($state | get -o dirs | default {})
    let parents = ($state | get -o parent-dirs | default [])

    if $scoped {
      let scope = (vcd-find-scope $dir)

      if $children {
        let scoped_parents = ($state | get -o scoped-parent-dirs | default [])
        let existing = ($scoped_parents | where { |e| $e.path == $dir and $e.scope == $scope })
        if not ($existing | is-empty) {
          error make { msg: $"'($dir)' is already a scoped parent directory under '($scope)'" }
        }
        let new_children = (vcd-parent-children [$dir])
        $new_children | transpose name path | each { |e|
          if $e.name in $dirs {
            print -e $"vcd: warning: child '($e.name)' is shadowed by existing bookmark \(($dirs | get $e.name)\)"
          }
        } | ignore
        vcd-save-state ($state | upsert scoped-parent-dirs ($scoped_parents | append { path: $dir, scope: $scope }))
      } else {
        let entry_name = ($name | default ($dir | path basename))
        let scoped_dirs = ($state | get -o scoped-dirs | default [])
        let existing = ($scoped_dirs | where { |e| $e.name == $entry_name and $e.scope == $scope })
        if not ($existing | is-empty) {
          error make { msg: $"duplicate scoped bookmark '($entry_name)' under scope '($scope)'" }
        }
        let parent_children = (vcd-parent-children $parents)
        if $entry_name in $parent_children {
          print -e $"vcd: warning: scoped bookmark '($entry_name)' shadows child of parent directory \(($parent_children | get $entry_name)\)"
        }
        vcd-save-state ($state | upsert scoped-dirs ($scoped_dirs | append { name: $entry_name, path: $dir, scope: $scope }))
      }
    } else if $children {
      if $dir in $parents {
        error make { msg: $"'($dir)' is already a parent directory" }
      }
      let new_children = (vcd-parent-children [$dir])
      $new_children | transpose name path | each { |e|
        if $e.name in $dirs {
          print -e $"vcd: warning: child '($e.name)' is shadowed by existing bookmark \(($dirs | get $e.name)\)"
        }
      } | ignore
      vcd-save-state ($state | upsert parent-dirs ($parents | append $dir))
    } else {
      let entry_name = ($name | default ($dir | path basename))
      if $entry_name in $dirs {
        error make {
          msg: $"duplicate name '($entry_name)' \(already points to ($dirs | get $entry_name)\). Use --name to specify an alternate name."
        }
      }
      let parent_children = (vcd-parent-children $parents)
      if $entry_name in $parent_children {
        print -e $"vcd: warning: bookmark '($entry_name)' shadows child of parent directory \(($parent_children | get $entry_name)\)"
      }
      vcd-save-state ($state | upsert dirs ($dirs | insert $entry_name $dir))
    }
  } else if $target == null {
    error make { msg: "usage: vcd NAME | vcd --add [DIR] [--name NAME] [--children] [--scoped]" }
  } else {
    cd (vcd-resolve $target)
  }
}
