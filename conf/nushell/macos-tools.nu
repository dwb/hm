# List everything
export def "launchctl list" [] {
  ^launchctl list | from tsv --flexible |
    update PID { |r| if $r.PID == '-' { null } else { $r.PID | into int } } 
}

# List 3rd-party services
export def "launchctl list3ps" [] {
  launchctl list | where Label !~ '^com\.apple\.' | where Label !~ '^application\.'
}
