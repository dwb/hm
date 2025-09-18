{
  config,
  lib,
  pkgs,
  pkgsUnstable,
  guiEnabled,
  doomemacs,
  doomemacs-modules,
  ...
}:
let
  configDir = ".emacs.d";
  # emacsPkgs = pkgsUnstable.callPackage (import "${nixpkgsUnstable}/pkgs/applications/editors/emacs");
  # emacsGuiPkg =

  baseEmacs =
    if guiEnabled then
      (pkgsUnstable.emacs31-pgtk.override {
        withNativeCompilation = true;
      }).overrideAttrs
        (old: {
          patches = old.patches ++ [
            ./emacs-window-name.patch
            ./emacs-macos-notifications.patch
            # ./emacs-flicker-fix.patch
            # ./emacs-dock-icon.patch
          ];
        })
    else
      pkgsUnstable.emacs31-nox;

  # `programs.emacs` is not used: the module wraps `package` with
  # `emacsWithPackages` unconditionally, and a second wrap would produce a
  # second Emacs.app that `exec`s into the first. See ./pkgs/emacs-app-wrapper.nix.
  emacsPackages = (pkgsUnstable.emacsPackagesFor baseEmacs).overrideScope (
    final: _prev: {
      emacsWithPackages = pkgsUnstable.callPackage ./pkgs/emacs-app-wrapper.nix { } final;
      withPackages = final.emacsWithPackages;
    }
  );

  emacsPackage = emacsPackages.emacsWithPackages (
    epkgs: with epkgs; [
      treesit-grammars.with-all-grammars
      vterm
      ghostel
      # (epkgs.callPackage (import ./pkgs/ghostel.nix) {})
    ]
  );

  signEmacsApp = pkgs.stdenv.hostPlatform.isDarwin && guiEnabled;

  lsregister =
    "/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework"
    + "/Support/lsregister";
in
{

  home.sessionVariables = {
    # Otherwise doom will try to create directories in the nix store
    EMACSDIR = "~/${configDir}";
  }
  // (lib.optionalAttrs guiEnabled { EDITOR = "emacsclient"; });

  home.sessionPath = [ "~/${configDir}/bin" ];

  home.packages = [
    emacsPackage
    # pkgsUnstable.claude-agent-acp
  ];

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
      run $RSYNC -rlp --delete --chmod=+w $VERBOSE_ARG ${doomemacs-modules}/ ~/${configDir}/sources/doom+

      if [[ -n $checkout ]]; then
        ln -snf "$checkout" ~/.doom.d
      else
        run $RSYNC -r --delete $VERBOSE_ARG ${src}/ ~/.doom.d
      fi
    '';

  # Install a signed copy of Emacs.app at a fixed path outside the store.
  # UNUserNotificationCenter needs a bundle that Launch Services has a record
  # of, which means the bundle must carry a real signature (not just the
  # linker's ad-hoc Mach-O one) — and signing needs a writable copy, which the
  # store cannot provide. The path is fixed rather than per-generation so the
  # notification permission grant survives rebuilds.
  home.activation.signEmacsApp = lib.mkIf signEmacsApp (
    lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      dst=${lib.escapeShellArg config.home.homeDirectory}/Applications/Emacs.app

      # Was a mkalias alias file before this activation existed; see linkapps.nix.
      if [[ -e $dst && ! -d $dst ]]; then
        run rm -f "$dst"
      fi

      run ${pkgs.rsync}/bin/rsync -rlt --delete --chmod=u+w $VERBOSE_ARG \
        ${emacsPackage}/Applications/Emacs.app/ "$dst/"

      # Ad-hoc identity: there is no code signing identity in the keychain, and
      # a Nix build could not use one anyway. --deep is not needed, the bundle
      # has no nested code.
      run /usr/bin/codesign --force --sign - --identifier org.gnu.Emacs "$dst"
      run ${lsregister} -f "$dst"
    ''
  );

}
