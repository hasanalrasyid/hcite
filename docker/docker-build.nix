{hcite ? import ./hcite {}
}:
let
  pkgs = hcite.pkgs;
  project = hcite.project;
in
pkgs.dockerTools.buildImage {
  name = "hasanalrasyid/hcite";
  tag = "dev";

  contents =  [ pkgs.busybox
                pkgs.protobuf
              ];

  config = {
    Cmd = [ "/bin/bash" ];
    WorkingDir = "/public";
  };
}
