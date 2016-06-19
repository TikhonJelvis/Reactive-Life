{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, life, stdenv
      , threepenny-gui
      }:
      mkDerivation {
        pname = "reactive-life";
        version = "0.2.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base containers threepenny-gui ];
        executableHaskellDepends = [ base containers life threepenny-gui ];
        homepage = "jelv.is/frp";
        description = "Conway's game of life as a demo of functional reactive programming";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskell.packages.ghc7103
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
