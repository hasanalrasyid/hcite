{ reflex-platform ? import ./reflex-platform {
  config.android_sdk.accept_license = true;
  config.allowBroken = true;
  } }:

reflex-platform.project ({ pkgs, ... }:
{
  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
    otherlibs = ./otherlibs;
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
    ghc = ["otherlibs" "common" "backend" "frontend"];
    ghcjs = ["otherlibs" "common" "frontend"];
  };
})
