{ reflex-platform ? import ./reflex-platform {
  config.android_sdk.accept_license = true;
  config.allowBroken = true;
  config.extraoptions = "
    keep-outputs = true
    keep-derivations = true
  ";
  config.doHaddock = false;
  } }:

reflex-platform.project ({ pkgs, ... }:
{
  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
    otherlibs = ./otherlibs;
    bulmex = ./bulmex-4.0.0;
    doHaddock = false;
    doCheck = false;
  };

  android.frontend = {
    executableName = "frontend";
    applicationId = "org.example.frontend";
    displayName = "Frontend";
    assets = ./frontend/static;
  };

  ios.frontend = {
    executableName = "frontend";
    bundleIdentifier = "org.example.frontend";
    bundleName = "Example iOS App";
  };

  shells = {
    ghc = ["common" "backend" "frontend"];
    ghcjs = ["common" "frontend"];
    doHaddock = false;
    doCheck = false;
  };
})
