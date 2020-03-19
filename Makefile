ghc:
#	nix-build -o native-frontend-result -A ghc.frontend --verbose
	nix-shell -A shells.ghc --run \
  "cabal --project-file=cabal.project \
    --builddir=dist-ghc new-build all"
b:
	nix-build -o native-backend-result -A ghc.backend --verbose
build:
	nix-build -o android-frontend-result -A android.frontend --verbose
js:
	nix-build -o js-frontend-result -A ghcjs.frontend --verbose
