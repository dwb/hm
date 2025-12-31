{ nixpkgs, nixpkgsUnstable, ... }:

{
  nix.registry = {
    nixpkgs = {
      exact = true;
      from = {
        type = "indirect";
        id = "nixpkgs";
      };
      flake = nixpkgs;
    };
    nixpkgsUnstable = {
      exact = true;
      from = {
        type = "indirect";
        id = "nixpkgsUnstable";
      };
      flake = nixpkgsUnstable;
    };
  };
}
