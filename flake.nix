{
  description = "Nix flake for Aja";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    flake-utils.url = "github:numtide/flake-utils";
    
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          overlays = [ (import rust-overlay) ];
          pkgs = import nixpkgs {
            inherit system overlays;
          };
          rustTools = (pkgs.rust-bin.stable.latest.default.override {
            extensions = ["rust-src"];
          });
        in
        with pkgs;
        {
          devShells.default = mkShell {
            nativeBuildInputs = [
              rustTools
              qbe
              rust-analyzer
              pkg-config
              just
              eza
              fd
              openssl.dev
            ];
          };
        }
      ); 
}
