#!/usr/bin/env nu

def main [pkg: string] {
  print -e $"*** prebuilding ($pkg)..."
  let paths = (nix build --no-link --print-out-paths .#($pkg) | lines)
  if ($paths | length) > 1 {
    error make { msg: "expected only one out path" }
  }

  let out = $"prebuilt/($pkg).tar.xz"

  print -e $"\n*** archiving ($pkg) to ($out)..."
  tar -cvJf $out --transform=$"s|^./|./($pkg)/|" --directory=($paths | get 0) .
}
