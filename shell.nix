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
  # Entry paths:
  # Word:
  # xml sel --no-doc-namespace -t -c \
  #   "/TEI.2/text/body/div0/entryFree[@type=\"main\"]/orth" <file_path>
  # <entryFree id="n18947" key="a)wti/zomai" type="main" opt="n" TEIform="entryFree">
  #   <orth extent="suff" lang="greek" opt="n" TEIform="orth">a)wt-i/zomai
  #   </orth>,
  #   <orth extent="suff" lang="greek" opt="n" TEIform="orth">a)wt-i/zomai
  #   </orth>,
  #   <sense> ... </sense>
  #   <sense> ... </sense>
  # </entryFree>

in
  haskellPackages.shellFor {
    packages = p: [
      ancientgreek
    ];
    withHoogle  = false;
    dict_path   = dictionaries;
    dict_path_1 = "${dictionaries}/${dictionaryPath "1"}";
    buildInputs = with haskellPackages; [
      dictionaries
      haskell-language-server
      ghcid
      cabal-install
      hpack
      pkgs.xmlstarlet
      # pkgs.readmdict
    ];
  }
