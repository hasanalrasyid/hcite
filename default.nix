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
    bulmex = ./lib/bulmex-4.0.0;

    bloodhound = ./lib/bloodhound;
#   bloodhound = (builtins.fetchGit{
#       url = https://github.com/bitemyapp/bloodhound;
#       rev = "c6233c493b1a7c3df8099872bbc1f66c5f25d95f";
#     });
    katip = ./lib/katip/katip;
    katip-elasticsearch = ./lib/katip/katip-elasticsearch;
# override reflex-dom-core...
#   http-client = ./lib/http-client/http-client;


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
#          "proto3-suite"
           "http2-client-grpc"
           "bloodhound"
           "polysemy"
           "polysemy-plugin"
           "polysemy-zoo"
           "xxhash"
           "avl-auth"

           "hs-abci-types"
           "hs-abci-extra"
           "hs-abci-server"
           "hs-abci-sdk"
           "hs-tendermint-client"
           "hs-abci-test-utils"
           "hs-iavl-client"
#          "hciteservice"

           "common" "backend" "frontend"];
    ghcjs = ["common" "frontend"];
    doHaddock = false;
    doCheck = false;
  };
  withHoogle = false;
  overrides = self: super: {
#   tasty = self.callHackage "tasty" "1.1.0.4" {};
#   tasty-hunit = self.callHackage "tasty-hunit" "0.10.0.1" {};
    http2-client-grpc = self.callCabal2nix "http2-client-grpc" (pkgs.fetchFromGitHub {
      owner = "lucasdicioccio";
      repo = "http2-client-grpc";
      rev = "6a1aacfc18e312ef57552133f13dd1024c178706";
      sha256 = "0zqzxd6x3hlhhhq24pybjy18m0r66d9rddl9f2zk4g5k5g0zl906";
    }) {};
    avl-auth = self.callCabal2nix "avl-auth" (pkgs.fetchFromGitHub {
      owner = "oscoin";
      repo = "avl-auth";
      rev = "dfc468845a82cdd7d759943b20853999bc026505";
      sha256 = "005j98hmzzh9ybd8wb073i47nwvv1hfh844vv4kflba3m8d75d80";
    }) {};
    xxhash = self.callCabal2nix "xxhash" (pkgs.fetchFromGitHub {
      owner = "christian-marie";
      repo = "xxhash";
      rev = "4023031704aae7a90aae1cb3762d9dac935337b5";
      sha256 = "0jqv0fb4fqb2rpayzmd1af560czfsa4dk13j0hwrw7l76jfm45ca";
    }) {};
    prometheus = self.callCabal2nix "prometheus" (pkgs.fetchFromGitHub {
      owner = "bitnomial";
      repo = "prometheus";
      rev = "82b3fb9f47fa2819496674ec2eaaabe03a07c7db";
      sha256 = "1ksqpmpnwxvcx2zwisswsnay8cigjrcdm5vrhlrldr2hlbzn6s3i";
    }) {};
    proto3-wire = self.callCabal2nix "proto3-wire" (pkgs.fetchFromGitHub {
      owner = "awakesecurity";
      repo = "proto3-wire";
      rev = "23015cf6363d1962fde6bdff0de111f7ec59ab75";
      sha256 = "0x8kd8yrfr85jknvbw59zl77wbd2864wmbvizynsk2jarzlqhqid";
      #sha256 = "0x8kd8yrfr851111111111111111111111111111111111111111";
    }) {};
    polysemy = self.callCabal2nix "polysemy" (pkgs.fetchFromGitHub{
      owner = "polysemy-research";
      repo = "polysemy";
      rev = "d7d3a938f4c1374161949c2d0aeee542e895e821";
      sha256 = "0qd8mn8phv487kp4qnkn6hicglz37gpajrwqlzl64fxyahi1fh2a";
      #sha256 = "0x8kd8yrfr851111111111111111111111111111111111111111";
    }) {};
    polysemy-plugin = self.callCabal2nix "polysemy-plugin" (pkgs.fetchFromGitHub{
      owner = "polysemy-research";
      repo = "polysemy";
      rev = "d7d3a938f4c1374161949c2d0aeee542e895e821";
      sha256 = "0qd8mn8phv487kp4qnkn6hicglz37gpajrwqlzl64fxyahi1fh2a";
    }) {};
    polysemy-zoo = self.callCabal2nix "polysemy-zoo" (pkgs.fetchFromGitHub{
      owner = "polysemy-research";
      repo = "polysemy-zoo";
      rev = "57c6012e196db7fe1ce7551f1f762cbddc71f095";
      sha256 = "18smd2c66gdn9585sdkn60ykvdvkbvkxrnnl9zix687dca6h9jw0";
    }) {};
    proto3-suite = self.callCabal2nix "proto3-suite" (pkgs.fetchFromGitHub {
      owner = "awakesecurity";
      repo = "proto3-suite";
      rev = "3f6dd6f612cf2eba3c05798926ff924b0d5ab4fa";
      sha256 = "0g7j7axx9rkrzw32ky9xl08zj34rx4mqafd89lrpnsi8lcq2z06j";
    }) {};
  };
})
