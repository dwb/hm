{
  description = "dani's home-manager config";

  inputs = {
    home-manager = {
      url = "github:nix-community/home-manager/release-23.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nu-scripts = {
      url = "github:nushell/nu_scripts";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, nixpkgsUnstable, home-manager, nu-scripts }:
    with builtins;
    with nixpkgs.lib;

    let
      usernames = [ "dan" "dwb" ];

      importPkgs = input: system:
        import input {
          inherit system;
          config.allowUnfree = true;
        };
      forAllSystems = genAttrs platforms.all;

      deps = ({ pkgs, ... }: {
        inherit nu-scripts;
        pkgsUnstable = importPkgs nixpkgsUnstable pkgs.system;
      });

      home = import ./home.nix;

      homeModule = { pkgs, username, ... }@args: {
        home-manager.users.${username} = home (args // (deps args));
      };

    in {
      # sigh: https://github.com/nix-community/home-manager/issues/3075#issuecomment-1330661815
      packages = forAllSystems (system: {
        homeConfigurations = genAttrs usernames
          (username: home-manager.lib.homeManagerConfiguration {
            pkgs = importPkgs nixpkgs system;
            modules = [
              ({ pkgs, ... }@args: {
                config._module.args = {
                  inherit username;
                } // (deps args);
              })
              home
            ];
          });
      });

      nixosModules.home = homeModule;
      darwinModules.home = homeModule;

      devShells = forAllSystems (system:
        with (importPkgs nixpkgs system); {
          default = mkShellNoCC {
            packages = [ home-manager.packages.${system}.default ];
            shellHook = ''
              printf '%s\n' ''' '# dani’s home-manager config #'  '''
            '';
          };
        });

      formatter =
        forAllSystems (system: (importPkgs nixpkgsUnstable system).nixfmt);
    };

}
