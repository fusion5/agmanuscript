let
  nixpkgs = <nixpkgs>;
  pkgs = import nixpkgs { config = {}; overlays = []; };
  haskellPackages = pkgs.haskellPackages;
  ancientgreek = haskellPackages.callCabal2nix "ancientgreek" ./. {};
in
  haskellPackages.shellFor {
    packages = p: [
      ancientgreek
    ];
    withHoogle = false;
    buildInputs = with haskellPackages; [
      haskell-language-server
      ghcid
      cabal-install
      hpack
    ];
  }
