def --wrapped main [...args] {
  let change = (jj log -r '@-' --no-graph -T
    'separate(" ", change_id.short(), description.first_line(), if(bookmarks, "(" ++ bookmarks.join(", ") ++ ")")) ++ "\n"' |
    lines |
    gum filter ...$in | split words | get 0)
            
  exec jj squash -kut $change ...$args
}
