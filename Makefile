build:
	nix-build -o android-frontend-result -A android.frontend --verbose
ghc:
	nix-build -o native-frontend-result -A ghc.frontend --verbose
js:
	nix-build -o js-frontend-result -A ghcjs.frontend --verbose
