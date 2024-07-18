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

    reactive-banana.url = "github:HeinrichApfelmus/reactive-banana";
    reactive-banana.flake = false;
    wxHaskell.url = "git+https://codeberg.org/wxHaskell/wxHaskell?ref=master";
    wxHaskell.flake = false;
    # https://github.com/HeinrichApfelmus/reactive-banana/
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
          packages = {
            primitive.source = "0.7.4.0";
            wx.source = "${inputs.wxHaskell}/wx";
            wxdirect.source = "${inputs.wxHaskell}/wxdirect";
            reactive-banana.source = "${inputs.reactive-banana}/reactive-banana";
            reactive-banana-wx.source = "${inputs.reactive-banana}/reactive-banana-wx";
            #reflex-gi-gtk.source = inputs.reflex-gi-gtk;
            #reflex.source = inputs.reflex;
            #process.source = "1.4.3.0";
          };
          settings = {
            primitive.check = false;
            wxcore = {super, ...}: {
              custom = _:
                let
                  wxc = (pkgs.callPackage ./nix/wxHaskell/wxc.nix {})
                      .overrideAttrs {src = "${inputs.wxHaskell}/wxc";};
                in
                (super.callPackage ./nix/wxHaskell/wxcore.nix {
                  wxGTK = pkgs.wxGTK32;
                  inherit wxc ;
                }).overrideAttrs (o: {
                  src = "${inputs.wxHaskell}/wxcore";
                  # https://github.com/NixOS/nixpkgs/issues/41340#issuecomment-394219692
                  strictDeps = true;
                  nativeBuildInputs = o.nativeBuildInputs ++ [
                    pkgs.pkg-config
                    pkgs.wxGTK32 # wxdirect needs wx-config at config time
                    wxc
                  ];
                  isExecutable = false;
                  isLibrary = true;
                  buildPhase = ''
                    cp ${./nix/wxHaskell}/*.hs src/haskell/Graphics/UI/WXCore/
                  '' + o.buildPhase;
                  patches = [
                    ./nix/wxHaskell/0001-inline-autogen-d-code.patch
                    ./nix/wxHaskell/0002-strip-autogen-from-cabal-build-process.patch
                  ];
                });
            };
            reactive-banana-wx = {super, ...}: {
              cabalFlags.buildExamples = true;
              custom = o : o.overrideAttrs (old: {
                buildInputs = (with super; [random array containers process filepath executable-path]) ++ old.buildInputs;
              });
              patches = [
                ./nix/reactive-banana/0001-remove-upper-bound-on-random.patch
              ];
            };
            reflex-gi-gtk =  {
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
