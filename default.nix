{}:
with (import (builtins.fetchTarball {
  url =  "https://github.com/dmjio/miso/archive/ea25964565074e73d4052b56b60b6e101fa08bc5.tar.gz";
  sha256 = "1yb9yvc0ln4yn1jk2k5kwwa1s32310abawz40yd8cqqkm1z7w6wg";
}) {});
let
  inherit (pkgs) runCommand closurecompiler;
  inherit (pkgs.haskell.packages) ghc865 ghcjs;
  client = ghcjs.callCabal2nix "swiss-tourney-pairings" ./. {};
  server = ghc865.callCabal2nix "swiss-tourney-pairings" ./. {};
in
  runCommand "swiss-tourney-pairings" { inherit client server; } ''
    mkdir -p $out/{bin,static}
    cp ${server}/bin/* $out/bin
    ${closurecompiler}/bin/closure-compiler --compilation_level ADVANCED_OPTIMIZATIONS \
      --jscomp_off=checkVars \
      --externs=${client}/bin/client.jsexe/all.js.externs \
      ${client}/bin/client.jsexe/all.js > temp.js
    cp -r ${./static}/* $out/static
    mv temp.js $out/static/all.js
  ''
