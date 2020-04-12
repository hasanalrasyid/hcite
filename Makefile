test:
	nix-shell -A shells.ghc --verbose
ghc:
	nix-shell -A shells.ghc --run \
  "cabal --project-file=cabal.project \
    --builddir=dist-ghc new-build all"
android:
	nix-shell -A android.frontend --run \
  "rm -f dist-android"
build:
	nix-build -o android-frontend-result -A android.frontend --verbose
js:
	nix-build -o js-frontend-result -A ghcjs.frontend --verbose
front:
	nix-shell -A shells.ghc --run 'ghcid -W -c "cabal new-repl frontend"'
back:
	nix-shell -A shells.ghc --run 'ghcid -W -c "cabal new-repl backend"'
