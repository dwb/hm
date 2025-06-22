{ config, lib, pkgs, ... }:

{

  home = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
    file."Applications/Home Manager Apps".enable = false;

    activation.aliasHomeManagerApplications = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      app_folder="${config.home.homeDirectory}/Applications"
      mkdir -p "$app_folder"
      for app in "$genProfilePath"/home-path/*.app "$genProfilePath"/home-path/Applications/*.app; do
        app="$(realpath "$app")"
        app_target="$app_folder/$(basename "$app")"
        if [[ -f $app_target ]]; then run rm "$app_target"; fi
        run ${pkgs.mkalias}/bin/mkalias -v "$app" "$app_target"
      done
    '';
  };

}
