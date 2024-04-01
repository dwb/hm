def --env gimme [...pkgs: string] {
  let dirs = ($pkgs | each { |p|
    ^nix build --print-out-paths --no-link $'nixpkgs#($p)' | lines
  } | flatten)

  let bindirs = ($dirs | each { path join bin } | filter { path exists })
  $env.PATH = ($env.PATH | prepend $bindirs)

  let mandirs = ($dirs | each { path join share/man } | filter { path exists })
  if ($mandirs | length) > 0 {
    $env.MANPATH = ($mandirs | append "" | str join ":")
  }
  null
}
