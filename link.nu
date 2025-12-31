#!/usr/bin/env nu

def main [] {
  let sections = {
    claude: ".claude"
    jjui: ".config/jjui"
    zed: ".config/zed"
  }
  for section in ($sections | transpose name dest) {
    let sectionPath = pwd | path join conf $section.name
    for fn in (glob --no-dir ($sectionPath)/**) {
      let dest = $env.HOME | path join $section.dest ($fn | path relative-to $sectionPath)
      ($dest | path dirname) | mkdir -v $in
      if ($dest | path type) == 'file' {
        print -e $"($dest): is a regular file, not overwriting"
      } else {
        ln -fsv $fn $dest
      }
    }
  }
}
