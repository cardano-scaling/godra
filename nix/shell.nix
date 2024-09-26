{ repoRoot, inputs, pkgs, system, lib }:

cabalProject:

{
  name = "godra";

  # packages = lib.traceSeq inputs.CHaP [
  packages = [
    # inputs.cardano-node.packages.cardano-node
    # inputs.cardano-node.packages.cardano-cli
    pkgs.nodejs
  ];

  # tools = {
  #   haskell-language-server =
  #     let
  #       hlsProject = pkgs.haskell-nix.cabalProject' {
  #         name = "haskell-language-server";
  #         src = inputs.iogx.inputs.haskell-nix.inputs."hls-2.6";
  #         configureArgs = "--disable-benchmarks --disable-tests";
  #         compiler-nix-name = lib.mkDefault "ghc96";
  #         modules = [ ];
  #       };
  #     in
  #     hlsProject.hsPkgs.haskell-language-server.components.exes.haskell-language-server;
  # };
}
