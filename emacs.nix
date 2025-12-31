{
  lib,
  pkgs,
  pkgsUnstable,
  guiEnabled,
  doomemacs,
  ...
}:
let
  configDir = ".emacs.d";
  # emacsPkgs = pkgsUnstable.callPackage (import "${nixpkgsUnstable}/pkgs/applications/editors/emacs");
  # emacsGuiPkg =
in
{

  home.sessionVariables = {
    # Otherwise doom will try to create directories in the nix store
    EMACSDIR = "~/${configDir}";
  }
  // (lib.optionalAttrs guiEnabled { EDITOR = "emacsclient"; });

  home.sessionPath = [ "~/${configDir}/bin" ];

  home.file.".emacs.d/.local/cache/debug-adapters/js-debug" =
    let
      version = "1.100.1";
    in
    {
      source = pkgs.fetchzip {
        url = "https://github.com/microsoft/vscode-js-debug/releases/download/v${version}/js-debug-dap-v${version}.tar.gz";
        hash = "sha256-NM/ehAy6gUbr2DtyjbrGp7dJZMUI7iR8Ku2cVWQISn8=";
      };
    };

  home.activation.linkDoomEmacsConfig =
    let
      src = /. + ./conf/doom.d;
    in
    lib.hm.dag.entryAfter [ "writeBoundary" ] ''
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
    package =
      with pkgsUnstable;
      (emacsPackagesFor (
        if guiEnabled then
          (emacs30-pgtk.override {
            withNativeCompilation = true;
          }).overrideAttrs
            (old: {
              patches = old.patches ++ [ ./emacs-window-name.patch ];
            })
        else
          emacs30-nox
      )).emacsWithPackages
        (
          epkgs: with epkgs; [
            treesit-grammars.with-all-grammars
            vterm
          ]
        );
  };

}
