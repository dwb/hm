{
  description = "dani's home-manager config";

  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixos-24.05";
    };
    nixpkgsUnstable = {
      url = "github:NixOS/nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager/release-24.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nu-scripts = {
      url = "github:nushell/nu_scripts";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, nixpkgsUnstable, home-manager, nu-scripts }:
    with builtins;
    with nixpkgs.lib;

    let
      usernames = [ "dan" "dwb" ];

      importPkgs = input: system:
        import input {
          inherit system;
          config = {
            allowUnfree = true;
            packageOverrides = pkgs: {
              jre = pkgs.jre_headless;
            };
          };
        };
      forAllSystems = genAttrs platforms.all;

      deps = ({ pkgs, ... }: inputs // {
        pkgsUnstable = importPkgs nixpkgsUnstable pkgs.system;
      });

      home = import ./home.nix;
      registryPins = import ./registry-pins.nix { inherit nixpkgs nixpkgsUnstable; };

      homeModule = { pkgs, username, ... }@args: {
        home-manager.users.${username} = home (args // (deps args));
      };

      # TODO: use these
      nixpkgsConfig = {
        global = { ... }: {
          config.nixpkgs.config = {
            allowUnfree = true;
          };
        };
        noGUI = { ... }: {
          config.nixpkgs.config = {
            packageOverrides = pkgs: {
              jre = pks.jre_headless;
            };
          };
        };
      };

    in {
      # sigh: https://github.com/nix-community/home-manager/issues/3075#issuecomment-1330661815
      packages = forAllSystems (system: {
        homeConfigurations = genAttrs usernames
          (username: home-manager.lib.homeManagerConfiguration {
            pkgs = importPkgs nixpkgs system;
            modules = [
              ({ pkgs, ... }@args: {
                _module.args = {
                  inherit username;
                } // (deps args);
              })

              home
            ];
          });
      });

      nixosModules = {
        inherit registryPins nixpkgsConfig;
        home = homeModule;
      };

      darwinModules = {
        inherit registryPins nixpkgsConfig;
        home = homeModule;
      };

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
