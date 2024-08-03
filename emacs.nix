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

  programs.emacs = {
    enable = true;
    package = if guiEnabled then pkgs.emacs29-pgtk else pkgs.emacs29-nox;
  };

}
