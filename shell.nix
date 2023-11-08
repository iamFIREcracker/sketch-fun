# https://paperless.blog/nix-shell-template
let
  pkgs =
    import (
      fetchTarball {
        name = "nixos-23.05_2023-09-18";
        url =
          "https://github.com/NixOS/nixpkgs/archive/f81112bbb0f357b80333300c4702ff7538096156.tar.gz";
        sha256 = "0pxhiggxl5drnpva9287w136rrykcd7xk1nyh655wgf6sb38a82k";
      }
    )
    {};
in
  pkgs.mkShell {
    packages = [
      pkgs.SDL2
      pkgs.SDL2_image
      pkgs.SDL2_ttf
    ];
    LD_LIBRARY_PATH = "/usr/lib/wsl/lib/:${pkgs.SDL2}/lib:${pkgs.SDL2_image}/lib:${pkgs.SDL2_ttf}/lib";
  }
