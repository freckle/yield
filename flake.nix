{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs =
    inputs:
    inputs.flake-utils.lib.eachDefaultSystem (
      system:
      let
        nixpkgs = inputs.nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = nixpkgs.mkShell {
          buildInputs = [ nixpkgs.zlib ];

          nativeBuildInputs = [
            nixpkgs.haskellPackages.fourmolu
            nixpkgs.haskellPackages.cabal-install
            (nixpkgs.haskell-language-server.override {
              supportedGhcVersions = ["9103"];
            })
          ];

          shellHook = ''
            export STACK_YAML=stack-lts23.yaml
          '';
        };
      }
    );
}
