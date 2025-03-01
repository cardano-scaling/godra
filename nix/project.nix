{ repoRoot, inputs, pkgs, lib, system }:

let

  cabalProject = pkgs.haskell-nix.cabalProject' ({ config, pkgs, ... }: {
    name = "godra";
    src = ../.;
    compiler-nix-name = "ghc96";
    shell.withHoogle = false;
    inputMap = {
      "https://chap.intersectmbo.org/" = inputs.iogx.inputs.CHaP;
    };
  });

  project = lib.iogx.mkHaskellProject {
    inherit cabalProject;
    shellArgs = repoRoot.nix.shell;
  };

in

project
