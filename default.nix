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

    proto3-suite = (builtins.fetchGit{
        url =  https://github.com/awakesecurity/proto3-suite;
        rev = "3f6dd6f612cf2eba3c05798926ff924b0d5ab4fa";
      });
    proto3-wire = (builtins.fetchGit{
        url =  https://github.com/awakesecurity/proto3-wire;
        rev = "23015cf6363d1962fde6bdff0de111f7ec59ab75";
      });
    polysemy = (builtins.fetchGit{
        url = https://github.com/polysemy-research/polysemy;
        rev = "d7d3a938f4c1374161949c2d0aeee542e895e821";
    });
    polysemy-plugin = (builtins.fetchGit{
        url = https://github.com/polysemy-research/polysemy;
        rev = "d7d3a938f4c1374161949c2d0aeee542e895e821";
      });
    polysemy-zoo = (builtins.fetchGit{
        url = https://github.com/polysemy-research/polysemy-zoo;
        rev = "57c6012e196db7fe1ce7551f1f762cbddc71f095";
      });
    bloodhound = ./lib/bloodhound;
#   bloodhound = (builtins.fetchGit{
#       url = https://github.com/bitemyapp/bloodhound;
#       rev = "c6233c493b1a7c3df8099872bbc1f66c5f25d95f";
#     });
    katip = ./lib/katip/katip;
    katip-elasticsearch = ./lib/katip/katip-elasticsearch;
# override reflex-dom-core...
#   http-client = ./lib/http-client/http-client;
    http2-client-grpc = (builtins.fetchGit{
      url = https://github.com/lucasdicioccio/http2-client-grpc;
      rev = "6a1aacfc18e312ef57552133f13dd1024c178706";
    });
    avl-auth = (builtins.fetchGit {
      url = https://github.com/oscoin/avl-auth;
      rev = "dfc468845a82cdd7d759943b20853999bc026505";
    });
    xxhash = (builtins.fetchGit {
      url = https://github.com/christian-marie/xxhash;
      rev = "4023031704aae7a90aae1cb3762d9dac935337b5";
    });
    prometheus = (builtins.fetchGit {
      url = https://github.com/bitnomial/prometheus;
      rev = "82b3fb9f47fa2819496674ec2eaaabe03a07c7db";
    });


    hs-abci-extra = ./lib/kepler/hs-abci-extra;
    hs-abci-sdk = ./lib/kepler/hs-abci-sdk;
    hs-abci-server = ./lib/kepler/hs-abci-server;
#   hs-abci-types = pkgs.callPackage ./hs-abci-types.nix {};
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
           "http2-client-grpc"
           "bloodhound"
           "polysemy"
           "polysemy-plugin"
           "polysemy-zoo"
           "proto3-wire"
           "proto3-suite"
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
})
