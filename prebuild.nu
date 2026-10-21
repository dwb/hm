#!/usr/bin/env nu

def main [pkg: string] {
  print -e $"*** prebuilding ($pkg)..."
  let paths = (nix build --no-link --print-build-logs --print-out-paths .#($pkg) | lines)
  if ($paths | length) > 1 {
    error make { msg: "expected only one out path" }
  }

  let path = ($paths | get 0)
  let out = $"prebuilt/($pkg).tar.xz"

  # Archive the store path's entries rather than `.`: a `./` entry carries the
  # store path's read-only mode, and extracting it makes tar try to chmod the
  # build directory, which fails on builders where the build user doesn't own
  # it (e.g. the Determinate Nix Linux builder on macOS).
  let entries = (ls --all $path | get name | each { path basename })

  print -e $"\n*** archiving ($pkg) to ($out)..."
  tar -cvJf $out --transform=$"s|^|./($pkg)/|" --directory=($path) ...$entries
}
