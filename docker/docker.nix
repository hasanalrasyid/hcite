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

# fromImage = pkgs.dockerTools.pullImage {
#     imageName = "alpine";
#     imageDigest = "sha256:cb8a924afdf0229ef7515d9e5b3024e23b3eb03ddbba287f4a19c6ac90b8d221";
#     sha256 = "0c1hvm2ck57kq5pwa29v3qjj1mqn2yx2dgkx2iizxqdym77f97dz";
# };

  contents =  [ pkgs.busybox
                project.ghc.hciteservice
              ];

  config = {
    Cmd = [ "/bin/bash" ];
    WorkingDir = "/public";
  };
}
