{
  description = "Halogen Hooks";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-23.05";
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
    easy-ps = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, easy-ps, flake-utils, ... }: let
    name = "halogen-hooks";
    supportedSystems = ["aarch64-darwin" "x86_64-darwin" "x86_64-linux"];
  in
    flake-utils.lib.eachSystem supportedSystems (
      system: let
        pkgs = import nixpkgs {inherit system;};
        pursPkgs = import easy-ps {inherit pkgs;};
      in {
        devShells = {
          default = pkgs.mkShell {
            inherit name;
            packages = [
              pkgs.nodejs-18_x
              pkgs.esbuild

              pkgs.nodePackages.bower
              pkgs.nodePackages.parcel-bundler
              pkgs.nodePackages.parcel

              pursPkgs.purescript-language-server               
              pursPkgs.purs
              pursPkgs.spago
              pursPkgs.purs-tidy
            ];
          };
        };
      }
    );
}
