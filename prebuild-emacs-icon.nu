#!/usr/bin/env nu

# Compile pkgs/Emacs.icon into the artifacts that pkgs/emacs-app-wrapper.nix
# installs into Emacs.app.
#
# actool ships only inside a full Xcode install, which a Nix build cannot reach,
# so its output is committed under prebuilt/ instead of being built on demand.
# Rerun this (via `make prebuild`) whenever pkgs/Emacs.icon changes.
#
# Assets.car carries a build timestamp, so a rerun produces a diff even when the
# icon itself has not changed.

def main [] {
  let src = ($env.FILE_PWD | path join pkgs Emacs.icon)
  let dst = ($env.FILE_PWD | path join prebuilt emacs-icon)
  # CFBundleIconFile in Emacs' own Info.plist, so the raster fallback actool
  # emits replaces the icns already in the bundle rather than sitting beside it.
  let name = "Emacs"

  print -e $"*** compiling ($src)..."

  rm --recursive --force $dst
  mkdir ($dst | path join Resources)

  (^xcrun actool
    --output-format human-readable-text --notices --warnings
    --platform macosx --target-device mac --minimum-deployment-target 26.0
    --app-icon $name
    --output-partial-info-plist ($dst | path join Info.plist)
    --compile ($dst | path join Resources)
    $src)
}
