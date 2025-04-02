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

  home.sessionPath = [
    "~/${configDir}/bin"
  ];

  home.activation.linkDoomEmacsConfig = let
    src = /. + ./conf/doom.d;
  in lib.hm.dag.entryAfter ["writeBoundary"] ''
    checkout=""
    for dir in ~/Developer/hm ~/.config/home-manager; do
      if [[ -d $dir ]]; then
        checkout="$dir/conf/doom.d"
      fi
    done

    RSYNC=${pkgs.rsync}/bin/rsync

    run $RSYNC -rlp --delete --chmod=+w --exclude=/.local/ $VERBOSE_ARG ${doomemacs}/ ~/${configDir}/

    if [[ -n $checkout ]]; then
      ln -snf "$checkout" ~/.doom.d
    else
      run $RSYNC -r --delete $VERBOSE_ARG ${src}/ ~/.doom.d
    fi
  '';

  programs.emacs = {
    enable = true;
    package = with pkgs; (emacsPackagesFor
      (if guiEnabled
       then emacs30-pgtk.override { withNativeCompilation = false; }
       else emacs30-nox)).emacsWithPackages (epkgs: with epkgs; [
         treesit-grammars.with-all-grammars
         vterm
       ]);
  };

}
