{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let 
        pkgs = import nixpkgs { inherit system; };

        ghcVersion = "ghc910";

        haskellEnv = pkgs.haskell.packages.${ghcVersion}.ghcWithPackages (ps: with ps; [
          stack
          megaparsec
          containers
          mtl
          unordered-containers
          directory
          process
          filepath
          async
          tasty
          tasty-hunit
          temporary
          silently
        ]);

        nativeBuildInputs = [
          haskellEnv
          pkgs.nasm 
          pkgs.gcc
          pkgs.gnumake
          pkgs.binutils
        ];

        # haskellPkgs     = with pkgs; [ stack ghc ];
        # buildToolsPkgs  = with pkgs; [ nasm gcc gnumake ];
        debugPkgs       = with pkgs; [ valgrind gdb ];
        unitTestsPkgs   = with pkgs; [ python3 ];
        otherPkgs       = with pkgs; [ hlint ];
        
        allPkgs = nativeBuildInputs
          ++ debugPkgs 
          ++ unitTestsPkgs 
          ++ otherPkgs;

      in with pkgs; {
        # nix build
        packages.default = stdenv.mkDerivation {
          pname = "rune";
          version = "1.0.0";
          src = ./RuneLang;

          nativeBuildInputs = nativeBuildInputs;
          # buildInputs = [ ];

          buildPhase = ''
            export HOME=$TMPDIR
            export STACK_ROOT=$TMPDIR/.stack
            export STACK_IN_NIX_SHELL=1
            make all STACK_NIX_FLAGS="--system-ghc --no-install-ghc --offline"
          '';
          installPhase = ''
            mkdir -p $out/bin
            cp rune $out/bin
          '';
        };
        # nix dev shell
        devShells.default = mkShell {
         buildInputs = allPkgs;
         shellHook = ''
           export PKG_CONFIG_PATH=${pkgs.lib.makeLibraryPath [ ]}:$PKG_CONFIG_PATH
           export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath [ ]}:$LD_LIBRARY_PATH
         '';
       };
   });
}
