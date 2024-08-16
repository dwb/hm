{ lib, pkgs, guiEnabled, doomemacs, ... }:
let
  configDir = ".emacs.d";
in {

  home.sessionVariables = {
    # Otherwise doom will try to create directories in the nix store
    EMACSDIR = "~/${configDir}";
  } // (lib.optionalAttrs guiEnabled {
    EDITOR = "emacsclient";
  });

  home.file."${configDir}" = {
    source = doomemacs;
    recursive = true;
  };

  home.sessionPath = [
    "~/${configDir}/bin"
  ];

  home.activation.linkDoomEmacsConfig = let
    src = builtins.toPath ./conf/doom.d;
  in lib.hm.dag.entryAfter ["writeBoundary"] ''
    checkout=""
    for dir in ~/Developer/hm ~/.config/home-manager; do
      if [[ -d $dir ]]; then
        checkout="$dir/conf/doom.d"
      fi
    done

    RSYNC=${pkgs.rsync}/bin/rsync

    if [[ -n $checkout ]]; then
      ln -sf "$checkout" ~/.doom.d
    else
      run $RSYNC -r --delete $VERBOSE_ARG ${src}/ ~/.doom.d
    fi
  '';

  programs.emacs = {
    enable = true;
    package = if guiEnabled then pkgs.emacs29-pgtk else pkgs.emacs29-nox;
  };

}
