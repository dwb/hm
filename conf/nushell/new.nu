# make a new thing

def --env newrepo [
  dest: path,
  orig: record,
  make: closure, # must create dest
] {
  if ($dest | path exists) {
    error make {
        msg: "path exists"
        label: {
            text: "path exists"
            span: $orig.span
        }
    }
  }

  do $make

  cd $dest
  "use flake\n" | save .envrc
  do -c {
    git init
    if not ('.envrc' | path exists) {
      'use flake' | save .envrc
    }
    git add .
    direnv allow
    nix flake lock
    git add flake.lock
    git commit -m 'Initial commit'
  }
}

export module dev {

  export def --env haskell [dest: path] {
    newrepo $dest (metadata $dest) {
        nix flake new -t templates#haskell-flake $dest
    }
  }

}

export def main [] {
  print "make a new thing"
}
