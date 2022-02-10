{ config, pkgs, ... }:

{ # imports = [  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "james";
  home.homeDirectory = "/home/james";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.

  # home.stateVersion = "21.11";

  nixpkgs.config.allowUnfree = true;

  home.packages = with pkgs; [
    pinentry_emacs
    pciutils
    chromium
    zoom-us
    # (import (fetchTarball https://github.com/haskell/haskell-language-server/archive/745ef26f406dbdd5e4a538585f8519af9f1ccb09.tar.gz)).defaultPackage.x86_64-linux
    # (import (fetchFromGitHub {
    #   owner = "haskell";
    #   repo = "haskell-language-server";
    #   rev = "745ef26f406dbdd5e4a538585f8519af9f1ccb09";
    #   sha256 = "10vj4wb0gdvfnrg1d7r3dqjnkw34ryh7v4fvxsby6fvn1l2kvsj5";
    # }))
    obs-studio
    pcmanfm
    gnuplot
    nmap
    nnn
    qbittorrent
  ];
}
