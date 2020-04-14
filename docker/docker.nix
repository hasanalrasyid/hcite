{hcite ? import ./hcite {}
}:
let
  pkgs = hcite.pkgs;
  project = hcite.project;
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
