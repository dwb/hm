{
  config,
  lib,
  pkgs,
  ...
}:

{

  home = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
    file."Applications/Home Manager Apps".enable = false;

    activation.aliasHomeManagerApplications = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      targetDir="${config.home.homeDirectory}/Applications"
      run mkdir -p "$targetDir"
      shopt -s nullglob
      for app in "$newGenPath"/home-path{,/Applications}/*.app; do
        app="$(realpath "$app")"
        name="$(basename "$app")"
        # Emacs.app is installed as a real, code-signed copy by
        # home.activation.signEmacsApp; an alias here would clash with it.
        if [[ $name == Emacs.app ]]; then continue; fi
        targetApp="$targetDir/$name"
        if [[ -f $targetApp ]]; then run rm "$targetApp"; fi
        run ${pkgs.mkalias}/bin/mkalias -v "$app" "$targetApp"
      done
    '';
  };

}
