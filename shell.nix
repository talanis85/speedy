let np = import <nixpkgs> {};
    hp = np.haskell.packages.ghc822;
    disguise = np.callPackage ./disguise.nix {};
    speedy = hp.callPackage ./default.nix {
      disguise = hp.callPackage disguise {};
    };
in speedy.env
