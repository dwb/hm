# For each conflicted file in a revision, show the changes that cause the
# conflict: the divergent commits that last modified the file on each side.
#
# For a conflicted file, the culprits are the heads of the file-modifying
# commits among the target's ancestors (`heads(::(rev-) & files(path))`).
# heads() drops any commit that is an ancestor of another, leaving exactly the
# divergent tips - two or more when the file is genuinely conflicted.
#
# The target itself is added when it modifies the file (`rev & files(path)`),
# covering the case where a merge both combines its parents and edits the file
# yet stays conflicted. It is unioned in rather than folded into the heads()
# argument so it can't swallow the parent-side tips it descends from. files()
# does not report a plain merge that only carries a conflict, so this adds the
# target only when it made a real edit.
def main [revision: string = "@"] {
  # resolve --list exits non-zero and writes to stderr when there are no
  # conflicts; capture both so the "no conflicts" path stays quiet.
  let conflicts = (
    (jj resolve --list -r $revision | complete).stdout
    | lines
    | parse --regex '^(?<path>.*?)\s+\d+-sided conflict.*$'
  )

  if ($conflicts | is-empty) {
    print $"No conflicts at ($revision)"
    return
  }

  for c in $conflicts {
    let path = $c.path
    let pf = $"files\(($path | to json)\)"
    let revset = $"heads\(::\(($revision)-\) & ($pf)\) | \(($revision) & ($pf)\)"
    print $"($path):"
    (jj log --no-graph -r $revset -T
      '"  " ++ separate(" ", change_id.shortest(8), description.first_line(), if(bookmarks, "(" ++ bookmarks.join(", ") ++ ")")) ++ "\n"')
  }
}
