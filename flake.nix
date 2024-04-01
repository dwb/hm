{
  description = "dani's home-manager config";

  inputs = {
    home-manager = {
      url = "github:nix-community/home-manager/release-23.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, nixpkgsUnstable, home-manager }:
    with builtins;
    with nixpkgs.lib;

    let
      myUsernames = [ "dan" "dwb" ];

      importPkgs = input: system:
        import input {
          inherit system;
          config.allowUnfree = true;
        };
      forAllSystems = genAttrs platforms.all;

      mkHome = system: myUsername: home-manager.lib.homeManagerConfiguration {
        inherit system;
        modules = [
          (import ./home.nix {
            pkgs = importPkgs nixpkgs system;
            pkgsUnstable = importPkgs nixpkgsUnstable system;
            inherit myUsername;
          })
        ];
      };
    in
      {
        # sigh: https://github.com/nix-community/home-manager/issues/3075#issuecomment-1330661815
        packages = forAllSystems (system: {
          ${system}.homeConfigurations = genAttrs myUsernames (mkHome system);
        });
      };

}
