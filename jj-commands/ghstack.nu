def main [from: string, stack?: string] {
  let branches = (jj log --reversed -r $"bookmarks\() & \(($from)::@ | trunk\()..($from)) ~ empty\() ~ description\(\"\") ~ private_commits\()" -GT 'separate("\n", bookmarks.map(|b| b.name())) ++ "\n"' |
    lines)

  if ($branches | length) > 1 {
    if $stack != null {
        exec gh stack link $stack ...$branches
    } else {
        exec gh stack link ...$branches
    }
  } else {
    error make 'at least two bookmarks needed'
  }
}
