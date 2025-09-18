def --wrapped main [...args] {
  let result = (jj log -r 'trunk()..@-' --no-graph -T
    'separate(" ", change_id.short(), description.first_line(), if(bookmarks, "(" ++ bookmarks.join(", ") ++ ")")) ++ "\n"' |
    lines |
    input list --fuzzy)
            
  if $result != null {
    let change = ($result | split words | get 0)
    exec jj squash -kut $change ...$args
  }
}
