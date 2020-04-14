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

  fromImage = pkgs.dockerTools.pullImage {
      imageName = "alpine";
      imageDigest = "sha256:e1871801d30885a610511c867de0d6baca7ed4e6a2573d506bbec7fd3b03873f";
      sha256 = "05wcg38vsygjzf59cspfbb7cq98c7x18kz2yym6rbdgx960a0kyq";
  };

  contents =  [ pkgs.busybox
                project.ghc.hciteservice
              ];

  config = {
    Cmd = [ "/bin/bash" ];
    WorkingDir = "/public";
  };
}
