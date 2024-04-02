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

      deps = ({ pkgs ? null, system ? null, ... }@args:
        assert assertMsg ((pkgs != null) || (system != null))
          ("hm: deps: either pkgs or system must be given: [ "
            + (pipe [ attrNames (concatStringsSep ", ") ] args) + " ]");
        let pkgs = pkgs || (importPkgs nixpkgs system);
        in {
          inherit pkgs nu-scripts;
          pkgsUnstable = importPkgs nixpkgsUnstable pkgs.system;
        });

      depsModule = args: { config._module.args = deps args; };

      home = import ./home.nix;

      homeModule = { username, ... }@args: seq (throw "asdf") {
        imports = [ depsModule ];
        home-manager.users.${username} = home args;
      };

    in {
      # sigh: https://github.com/nix-community/home-manager/issues/3075#issuecomment-1330661815
      packages = forAllSystems (system: {
        ${system}.homeConfigurations = genAttrs usernames
          (home-manager.lib.homeManagerConfiguration
            (args: home (args // (deps args))));
      });

      nixosModules.home = homeModule;
      darwinModules.home = homeModule;

      devShells = forAllSystems (system:
        with (importPkgs nixpkgs system); {
          default = mkShellNoCC {
            packages = [ ];
            shellHook = ''
              printf '%s\n' ''' '# dani’s home-manager config #'  '''
            '';
          };
        });

      formatter =
        forAllSystems (system: (importPkgs nixpkgsUnstable system).nixfmt);
    };

}
