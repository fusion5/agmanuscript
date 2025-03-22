let
  nixpkgs = <nixpkgs>;
  pkgs = import nixpkgs { config = {}; overlays = []; };
  haskellPackages = pkgs.haskellPackages;
  ancientgreek = haskellPackages.callCabal2nix "ancientgreek" ./. {};
  dictionaries = pkgs.fetchFromGitHub {
    owner  = "PerseusDL";
    repo   = "lexica";
    rev    = "50f2d2430b2dec2b2a0bfe416740cf52f7ea4879";
    sha256 = "sha256-7HvM3UzYuJ9Y+QTkj4t7W1MQEUXVXeRwB6D3zG9/HX8=";
  };
  dictionaryPath = n: "CTS_XML_TEI/perseus/pdllex/grc/lsj/grc.lsj.perseus-eng${n}.xml";

in
  haskellPackages.shellFor {
    packages = p: [
      ancientgreek
    ];
    withHoogle  = false;
    dict_path   = dictionaries;
    dict_path_1 = "${dictionaries}/${dictionaryPath "1"}";
    buildInputs = with haskellPackages; [
      cabal-install
      dictionaries
      fourmolu
      ghcid
      haskell-language-server
      hpack
    ];
  }
