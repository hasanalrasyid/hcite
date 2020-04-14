{hcite ? import ./hcite {}
}:
let
  pkgs = hcite.pkgs;
  project = hcite.project;
  packages = hcite.packages;
#let
#  hciteservice = pkgs.haskellPackages.callCabal2nix "hciteservice" (./hciteservice) {};
### pkgs = import <nixpkgs> { inherit system; };
### sources = import ./nix/sources.nix;
### backend = import ./backend.nix { };
### frontend = import ./frontend/default.nix { };
in
pkgs.dockerTools.buildImage {
  name = "hasanalrasyid/hcite";
  tag = "latest";

  contents =  [ pkgs.busybox
                project.ghc.hciteservice
              ];

  config = {
    Cmd = [ "/bin/bash" ];
    WorkingDir = "/public";
  };
}
