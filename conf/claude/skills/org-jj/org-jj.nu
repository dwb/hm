#!/usr/bin/env nu

# Update and check jj-diff dynamic blocks in an Org file through emacsclient.
# Calls my/org-jj-diff-file-report in conf/doom.d/lib/my-org-jj.el.

def file-report [file: string, update: bool] {
  let path = ($file | path expand)
  if not ($path | path exists) {
    error make { msg: $"No such file: ($path)" }
  }
  # A JSON string is also a valid Emacs Lisp string.
  let form = $"\(my/org-jj-diff-file-report ($path | to json) (if $update { 't' } else { 'nil' }))"
  let result = (^emacsclient --eval $form | str trim)
  if $result == "nil" {
    print "Every hunk is shown exactly once"
  } else {
    # Emacs prints the report as a Lisp string literal. Decode it as JSON
    # where possible, and otherwise print it as it is.
    print (try { $result | from json } catch { $result })
  }
}

# Update jj-diff blocks and check hunk coverage in Org files.
def main [] {
  help main
}

# Update all dynamic blocks in FILE, save it, and print the coverage report.
# Fails without changing anything if Emacs has FILE open with unsaved changes.
def "main update" [file: string] {
  file-report $file true
}

# Print the coverage report for the jj-diff blocks in FILE without changing it.
def "main check" [file: string] {
  file-report $file false
}
