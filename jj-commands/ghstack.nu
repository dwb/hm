def main [from: string] {
  let branches = (jj log --reversed -r $"bookmarks\() & \(($from)::@ | trunk\()..($from)) ~ empty\() ~ description\(\"\") ~ private_commits\()" -GT 'separate("\n", bookmarks.map(|b| b.name())) ++ "\n"' |
    lines)
            
  if ($branches | length) > 1 {
    exec gh stack link ...$branches
  } else {
    error make 'at least two bookmarks needed'
  }
}
