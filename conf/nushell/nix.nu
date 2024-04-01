# nice wrappers for nix commands

#list installed packages
export def --wrapped "nix profile list" [...rest] {
  let $args = ($rest | prepend '--json')
  mut out = (do --capture-errors { ^nix profile list ...$args })
  let err = $out
  try {
    $out = ($out | from json)
    match $out.version {
        2 => ($out | get elements |
                select attrPath originalUrl url outputs priority active storePaths),
        _ => (error make { msg: $"Unrecognised JSON version: ($out.version)" })
    }
  } catch {
    print --stderr $err
  }
}
