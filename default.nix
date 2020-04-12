{ reflex-platform ? import ./reflex-platform {
  config.android_sdk.accept_license = true;
  config.allowBroken = true;
  config.extraoptions = "
    keep-outputs = true
    keep-derivations = true
  ";
  config.doHaddock = false;
  }}:

reflex-platform.project ({ pkgs, ... }:
{
  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
    otherlibs = ./otherlibs;
    bulmex = ./lib/bulmex-4.0.0;

    hs-abci-extra = ./lib/kepler/hs-abci-extra;
    hs-abci-sdk = ./lib/kepler/hs-abci-sdk;
    hs-abci-server = ./lib/kepler/hs-abci-server;
    hs-abci-types = ./lib/kepler/hs-abci-types;
    hs-tendermint-client = ./lib/kepler/hs-tendermint-client;
    hs-iavl-client = ./lib/kepler/hs-iavl-client;
    hs-abci-test-utils =./lib/kepler/hs-abci-test-utils;

    hciteservice = ./hciteservice;

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
    ghc = ["proto-lens"

           "hs-abci-types"
           "hs-abci-extra"
           "hs-abci-server"
           "hs-abci-sdk"
           "hs-tendermint-client"
           "hs-iavl-client"
           "hs-abci-test-utils"
           "hciteservice"

           "common" "backend" "frontend"];
    ghcjs = ["common" "frontend"];
    doHaddock = false;
    doCheck = false;
  };
  withHoogle = false;
  overrides = self: super: {
    doHaddock = false;
    doCheck = false;
    polysemy = self.callCabal2nix "polysemy" (./lib/polysemy-1.3.0.0) {};
    polysemy-plugin = self.callCabal2nix "polysemy-plugin" (./lib/polysemy-plugin-0.2.5.0) {};
    polysemy-zoo = self.callCabal2nix "polysemy-zoo" (./lib/polysemy-zoo-0.7.0.0) {};

    bloodhound =  pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak (
      self.callHackage "bloodhound" "0.16.0.0" {}
    ));
    avl-auth = pkgs.haskell.lib.dontCheck (self.callCabal2nix "avl-auth" (pkgs.fetchFromGitHub {
      owner = "oscoin";
      repo = "avl-auth";
      rev = "dfc468845a82cdd7d759943b20853999bc026505";
      sha256 = "005j98hmzzh9ybd8wb073i47nwvv1hfh844vv4kflba3m8d75d80";
    }) {});

    http2-client-grpc = self.callCabal2nix "http2-client-grpc" (pkgs.fetchFromGitHub {
      owner = "lucasdicioccio";
      repo = "http2-client-grpc";
      rev = "6a1aacfc18e312ef57552133f13dd1024c178706";
      sha256 = "0zqzxd6x3hlhhhq24pybjy18m0r66d9rddl9f2zk4g5k5g0zl906";
    }) {};
    xxhash = pkgs.haskell.lib.dontCheck (self.callHackage "xxhash" "0.0.2" {});

    prometheus = pkgs.haskell.lib.dontCheck (self.callHackage "prometheus" "2.1.3" {});

    proto3-wire = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak (
      self.callHackage "proto3-wire" "1.1.0" {}
    ));
    proto3-suite = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak (
      self.callHackage "proto3-suite" "0.4.0.0" {}
    ));
    katip-elasticsearch = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak (
      self.callHackage "katip-elasticsearch" "0.6.0.0" {}
    ));
  };
})
