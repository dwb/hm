{ config, lib, pkgs, ... }:

{

  home = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
    file."Applications/Home Manager Apps".enable = false;

    activation.aliasHomeManagerApplications = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      app_folder="${config.home.homeDirectory}/Applications"
      find "$genProfilePath/home-path/Applications" -type l -print | while read -r app; do
          app_target="$app_folder/$(basename "$app")"
          real_app="$(readlink "$app")"
          echo "mkalias '$real_app' '$app_target'" >&2
          $DRY_RUN_CMD ${pkgs.mkalias}/bin/mkalias "$real_app" "$app_target"
      done
    '';
  };

}
