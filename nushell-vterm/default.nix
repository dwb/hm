{ config, lib, pkgs, ... }:
let
  enabled = config.programs.nushell.enableVtermIntegration;

  configDir = if pkgs.stdenv.isDarwin then
    "Library/Application Support/nushell"
  else
    "${config.xdg.configHome}/nushell";
in
{
  # home-manager module to integrate nushell into emacs-vterm. prompt wrapping, command running, etc

  options.programs.nushell.enableVtermIntegration =
    lib.mkEnableOption "emacs vterm integration";

  config = lib.mkIf enabled {
    programs.nushell.extraConfig = ''
      source '${./vterm.nu}'
    '';
  };
}
