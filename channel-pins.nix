{ nixpkgs, nixpkgsUnstable, ... }:

{
  nix.channels = {
    inherit nixpkgs nixpkgsUnstable;
  };
}
