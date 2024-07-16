{
  inputs = {
    #nixpkgs.url = "github:nixos/nixpkgs/4e6868b1aa3766ab1de169922bb3826143941973";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    flake-root.url = "github:srid/flake-root";
    mission-control.url = "github:Platonic-Systems/mission-control";
    #reflex-gi-gtk.url = "gitlab:Kritzefitz/reflex-gi-gtk/0.2.0.1";
    #reflex-gi-gtk.flake = false;
    #reflex.url = "github:reflex-frp/reflex/v0.9.3.0";
    #reflex.flake = false;
    primitive.url = "github:haskell/primitive/v0.7.0.1";
    primitive.flake = false;
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.flake-root.flakeModule
        inputs.mission-control.flakeModule
      ];
      perSystem = { self', system, lib, config, pkgs, ... }: {
        haskellProjects.default = {
          # basePackages = pkgs.haskell.packages.ghc96;
          packages = {
            #primitive.source = inputs.primitive;
            primitive.source = "0.7.4.0";
            #aeson.source = "2.2.1.0";

            #reflex-gi-gtk.source = inputs.reflex-gi-gtk;
            #reflex.source = inputs.reflex;
          };
          settings = {
            primitive =  {
              check = false;
            };
            reflex-gi-gtk =  {
              #check = false;
              #haddock = false;
              #extraBuildDepends = [ pkgs.stork ];
              broken = false;
            };
          };
          devShell = {
            tools = hp: {
              treefmt = config.treefmt.build.wrapper;
            } // config.treefmt.build.programs;
          };
          autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell
        };
        treefmt.config = {
          inherit (config.flake-root) projectRootFile;
          package = pkgs.treefmt;
          programs.ormolu.enable = true;
          programs.nixpkgs-fmt.enable = true;
          programs.cabal-fmt.enable = true;
          programs.hlint.enable = true;
          settings.formatter.ormolu = {
            options = [
             #"--ghc-opt"
             #"-XImportQualifiedPost"
            ];
          };
        };
        mission-control.scripts = {
          docs = {
            description = "Start Hoogle server for project dependencies";
            exec = ''
              echo http://127.0.0.1:8888
              hoogle serve -p 8888 --local
            '';
            category = "Dev Tools";
          };
          repl = {
            description = "Start the cabal repl";
            exec = ''
              cabal repl "$@"
            '';
            category = "Dev Tools";
          };
          fmt = {
            description = "Format the source tree";
            exec = config.treefmt.build.wrapper;
            category = "Dev Tools";
          };
          run = {
            description = "Run the project with ghcid auto-recompile";
            exec = ''
              ghcid -c "cabal repl exe:x" --warnings -T :main
            '';
            category = "Primary";
          };
        };

        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.jarne;

        devShells.default = pkgs.mkShell {
         inputsFrom = [
           config.haskellProjects.default.outputs.devShell
           config.flake-root.devShell
           config.mission-control.devShell
         ];
      };
    };
  };
}
