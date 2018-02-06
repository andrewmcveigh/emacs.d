let
  pkgs = import <nixpkgs> {};
  stdenv = pkgs.stdenv;
in stdenv.mkDerivation rec {
  name = "tools";
  buildInputs = [
    pkgs.ghc
    pkgs.zlib
    pkgs.pkgconfig
  ];

  shellHook = ''
    export PKG_CONFIG_PATH=${pkgs.zlib.dev}/lib/pkgconfig
  '';

}
