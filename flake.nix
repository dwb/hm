{
  description = "dani's home-manager config";

  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/release-24.11";
    };
    nixpkgsUnstable = {
      url = "github:NixOS/nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager/release-24.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    doomemacs = {
      url = "github:doomemacs/doomemacs/master";
      flake = false;
    };
    nu-scripts = {
      url = "github:nushell/nu_scripts";
      flake = false;
    };
  };

  outputs = inputs@{ nixpkgs, nixpkgsUnstable, home-manager, ... }:
    let
      inherit (nixpkgs) lib;

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
      forAllSystems = lib.genAttrs lib.platforms.all;

      deps = ({ pkgs, ... }: inputs // {
        pkgsUnstable = importPkgs nixpkgsUnstable pkgs.system;
      });

      home = import ./home.nix;
      registryPins = import ./registry-pins.nix { inherit nixpkgs nixpkgsUnstable; };
      channelPins = import ./channel-pins.nix { inherit nixpkgs nixpkgsUnstable; };

      homeModule = { pkgs, username, ... }@args: {
        home-manager.users.${username} = {
          imports = [ home ];
          _module.args = {
            inherit username;
          } // inputs // (deps args);
        };
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
              jre = pkgs.jre_headless;
            };
          };
        };
      };

    in {
      # sigh: https://github.com/nix-community/home-manager/issues/3075#issuecomment-1330661815
      packages = forAllSystems (system: {
        homeConfigurations = lib.genAttrs usernames
          (username: home-manager.lib.homeManagerConfiguration {
            pkgs = importPkgs nixpkgs system;
            modules = [
              ({ pkgs, ... }@args: {
                _module.args = {
                  inherit username;
                } // inputs // (deps args);
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

      homeManagerModules = {
        inherit channelPins;
      }

      devShells = forAllSystems (system:
        let pkgs = (importPkgs nixpkgs system); in {
          default = pkgs.mkShellNoCC {
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
