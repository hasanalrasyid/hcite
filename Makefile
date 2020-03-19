ghc:
	nix-shell -A shells.ghc --run \
  "cabal --project-file=cabal.project \
    --builddir=dist-ghc new-build all"
build:
	nix-build -o android-frontend-result -A android.frontend --verbose
js:
	nix-build -o js-frontend-result -A ghcjs.frontend --verbose
