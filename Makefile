ghc:
	nix-shell -A shells.ghc --run \
  "cabal --project-file=cabal.project \
    --builddir=dist-ghc new-build all"
android:
	nix-shell -A android.frontend --run \
  "ln -fs /nix/store/hd0cm8xg023dm53qhggq8my56f1ka0pf-org.example.frontend dist-android"
build:
	nix-build -o android-frontend-result -A android.frontend --verbose
js:
	nix-build -o js-frontend-result -A ghcjs.frontend --verbose
