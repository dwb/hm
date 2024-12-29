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
    home.file.".config/nushell/emacs-vterm-config.nu".text = ''
      source '~/${configDir}/config.nu'
      source '${./vterm.nu}'
      use vterm
      use vprompt
      $env.PROMPT_COMMAND_BEFORE_VTERM = $env.PROMPT_COMMAND
      $env.PROMPT_COMMAND = { $"(do $env.PROMPT_COMMAND_BEFORE_VTERM)(vprompt left-prompt-track-cwd)" }
    '';
  };
}
