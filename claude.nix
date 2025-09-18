{ config, lib, pkgs, ... }:

let
  ast-grep-skills = pkgs.fetchFromGitHub {
    owner = "ast-grep";
    repo = "agent-skill";
    rev = "c2a9bc154f4ffe08b25d28d5e852dfac8c0d0d8a";
    hash = "sha256-awochSE2OupbsmaGx0xc7wHf0ovVMSdtHv4gZAGWOus=";
  };
in {
  home.file.".claude/skills/ast-grep" = {
    source = "${ast-grep-skills}/ast-grep/skills/ast-grep";
  };

  home.file.".claude/skills/outline" = {
    source = "${ast-grep-skills}/ast-grep/skills/outline";
  };

  home.packages = [
    pkgs.ast-grep
  ];
}
