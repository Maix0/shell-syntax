{
  description = "A basic flake with a shell";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };
  outputs = {
    self,
    nixpkgs,
    flake-utils,
    rust-overlay,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [(import rust-overlay)];
      };
    in {
      devShell = with pkgs; let
        rust_dev =
          rust-bin.stable.latest.default.override
          {
            extensions = ["rust-src"];
          };
      in
        mkShell {
          nativeBuildInputs = [
            pkgs.bashInteractive
          ];
          buildInputs = [
            rust_dev
          ];
        };
    });
}
