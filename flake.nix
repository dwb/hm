{
  description = "dani's home-manager config";

  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/release-25.11";
    };
    nixpkgsUnstable = {
      url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    };
    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
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

  outputs =
    inputs@{
      nixpkgs,
      nixpkgsUnstable,
      home-manager,
      ...
    }:
    let
      inherit (nixpkgs) lib;

      usernames = [
        "dan"
        "dwb"
      ];

      nushellPatched =
        system:
        let
          pkgs = import nixpkgsUnstable { inherit system; };
          overlay = final: prev: {
            version = "0.112.2";

            src = pkgs.fetchFromGitHub {
              owner = "nushell";
              repo = "nushell";
              tag = final.version;
              hash = "sha256-wc7mfbwkJO5gq9mwsiTVx74+btqU6Ox8tPhnXkfmXRU=";
            };

            cargoPatches = [ ./nushell-crossterm-fix.patch ];
            cargoHash = "sha256-osWYPJ8/zKcEbakk9vXXKGMl7sbl5S1vfNYpiQ0HDeQ=";

            doCheck = false;
          };
        in
        pkgs.nushell.override (old: {
          rustPlatform = old.rustPlatform // {
            buildRustPackage = args: old.rustPlatform.buildRustPackage (lib.extends overlay args);
          };
        });

      baseNixpkgsConfig = {
        allowUnfree = true;
        packageOverrides =
          pkgs:
          let
            system = pkgs.stdenv.hostPlatform.system;
          in
          {
            direnv = pkgs.direnv.overrideAttrs { doCheck = false; };
            jre = pkgs.jre_headless;
            nushell = nushellPatched system;
            # Plugins inherit cargoHash from their nushell arg (nixpkgs pattern).
            # Our patched nushell has a different cargoHash (crossterm cargoPatches
            # change the vendor set), so we pin plugins to the original nushell to
            # keep their cargoHash consistent. The plugin ABI is unaffected.
            nushellPlugins = lib.mapAttrs (_: p: p.override { nushell = pkgs.nushell; }) pkgs.nushellPlugins;
          };
      };

      importPkgs =
        input: system:
        import input {
          inherit system;
          config = baseNixpkgsConfig;
        };

      forAllSystems = lib.genAttrs [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];

      deps = (
        { pkgs, ... }:
        inputs
        // {
          pkgsUnstable = importPkgs nixpkgsUnstable pkgs.stdenv.hostPlatform.system;
        }
      );

      home = import ./home.nix;
      registryPins = import ./registry-pins.nix;
      channelPins = import ./channel-pins.nix;

      homeModule =
        { pkgs, username, ... }@args:
        {
          home-manager.users.${username} = {
            imports = [ home ];
            _module.args = {
              inherit username;
            }
            // inputs
            // (deps args);
          };
        };

      # TODO: use these
      nixpkgsConfig = {
        global =
          { ... }:
          {
            config.nixpkgs.config = {
              allowUnfree = true;
            };
          };
        noGUI =
          { ... }:
          {
            config.nixpkgs.config = {
              packageOverrides = pkgs: {
                jre = pkgs.jre_headless;
              };
            };
          };
      };

      homeConfigurationsFor = system:
        let
          pkgs = importPkgs nixpkgs system;
        in
          lib.genAttrs usernames (
            username:
            home-manager.lib.homeManagerConfiguration {
              inherit pkgs;
              modules = [
                (
                  { pkgs, ... }@args:
                  {
                    _module.args = {
                      inherit username;
                    }
                    // inputs
                    // (deps args);
                  }
                )

                home
              ];
            }
          );

    in
    {

      apps = forAllSystems (system: {
        default = {
          type = "app";
          program = lib.getExe (importPkgs nixpkgs system).home-manager;
        };
      });

      # sigh: https://github.com/nix-community/home-manager/issues/3075#issuecomment-1330661815
      packages = forAllSystems (
        system:
        let
          pkgs = importPkgs nixpkgs system;
        in
        {
          homeConfigurations = homeConfigurationsFor system;

          iosevkaDWB = (
            pkgs.iosevka.override {
              privateBuildPlan = builtins.readFile ./iosevka-private-build-plans.toml;
              set = "DWB";
            }
          );
          iosevkaDWBTerm = (
            pkgs.iosevka.override {
              privateBuildPlan = builtins.readFile ./iosevka-term-private-build-plans.toml;
              set = "DWBTerm";
            }
          );
        }
      );

      homeConfigurations = forAllSystems homeConfigurationsFor;

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
      };

      devShells = forAllSystems (
        system:
        let
          pkgs = (importPkgs nixpkgs system);
        in
        {
          default = pkgs.mkShellNoCC {
            packages = [ home-manager.packages.${system}.default ];
            shellHook = ''
              printf '%s\n' ''' '# dani’s home-manager config #'  '''
            '';
          };
        }
      );

      formatter = forAllSystems (system: (importPkgs nixpkgs system).nixfmt);

      lib = {
        nixpkgsConfig = baseNixpkgsConfig;
      };
    };

}
